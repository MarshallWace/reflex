// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{collections::BTreeSet, num::NonZeroUsize};

use crate::StateUpdate;
use reflex::{
    cache::EvaluationCache,
    core::{DependencyList, DynamicState, Expression, Signal, StateToken, Term},
    hash::{hash_object, HashId},
};

pub type SubscriptionToken = usize;

pub struct Store<T: EvaluationCache> {
    state: DynamicState,
    cache: T,
    subscriptions: Vec<Subscription>,
    subscription_counter: usize,
    is_flushing: bool,
}
impl<T: EvaluationCache> Store<T> {
    pub fn new(cache: T, state: Option<DynamicState>) -> Self {
        Self {
            state: state.unwrap_or_else(|| DynamicState::new()),
            subscriptions: Vec::new(),
            cache,
            subscription_counter: 0,
            is_flushing: false,
        }
    }
    pub fn subscribe<THandler, TErr>(
        &mut self,
        expression: Expression,
        signal_handler: THandler,
    ) -> (
        SubscriptionToken,
        Vec<(SubscriptionToken, Result<Expression, Vec<TErr>>)>,
    )
    where
        THandler: Fn(&[Signal]) -> Vec<Result<Expression, TErr>> + Sync + Send,
    {
        self.subscription_counter += 1;
        let id = self.subscription_counter;
        let subscription = Subscription::new(id, expression);
        self.subscriptions.push(subscription);
        let updates = self
            .flush(None, signal_handler)
            .into_iter()
            .map(|(subscription, result)| (subscription.id, result))
            .collect::<Vec<_>>();
        (id, updates)
    }
    pub fn unsubscribe(&mut self, id: SubscriptionToken) -> bool {
        self.subscriptions
            .iter()
            .position(|subscription| subscription.id == id)
            .map(|index| {
                self.subscriptions.remove(index);
                true
            })
            .unwrap_or(false)
    }
    pub fn update<THandler, TErr>(
        &mut self,
        updates: impl IntoIterator<Item = (StateToken, StateUpdate)>,
        signal_handler: THandler,
    ) -> Vec<(SubscriptionToken, Result<Expression, Vec<TErr>>)>
    where
        THandler: Fn(&[Signal]) -> Vec<Result<Expression, TErr>> + Sync + Send,
    {
        let updates = updates
            .into_iter()
            .filter_map(|(key, update)| {
                let existing_value = self.state.get(key);
                let existing_hash = existing_value.map(hash_object);
                let value = match update {
                    StateUpdate::Value(value) => value,
                    StateUpdate::Patch(updater) => updater(existing_value),
                };
                match existing_hash {
                    Some(hash) if hash == hash_object(&value) => None,
                    _ => Some((key, value)),
                }
            })
            .collect::<Vec<_>>();
        if updates.is_empty() {
            return Vec::new();
        }
        let updated_keys = UpdateBatch::from(updates.iter().map(|(key, _)| *key));
        for (key, value) in updates {
            self.state.set(key, value);
        }
        let updates = self
            .flush(Some(updated_keys), signal_handler)
            .into_iter()
            .map(|(subscription, result)| (subscription.id, result))
            .collect::<Vec<_>>();
        updates
    }
    fn flush<'a: 'b, 'b, THandler, TErr: 'b>(
        &'a mut self,
        updates: Option<UpdateBatch>,
        signal_handler: THandler,
    ) -> impl IntoIterator<Item = (&Subscription, Result<Expression, Vec<TErr>>)> + 'b
    where
        THandler: Fn(&[Signal]) -> Vec<Result<Expression, TErr>> + Sync + Send,
    {
        if self.is_flushing {
            panic!("Flush already in progress");
        } else {
            self.is_flushing = true;
        }
        let tasks = self
            .subscriptions
            .iter_mut()
            .map(FlushTask::new)
            .collect::<Vec<_>>();
        let results = flush_recursive(
            tasks,
            signal_handler,
            &mut self.state,
            &mut self.cache,
            match updates {
                Some(updates) => vec![updates],
                None => Vec::new(),
            },
        );
        self.is_flushing = false;
        self.subscriptions
            .iter()
            .zip(results)
            .filter_map(|(subscription, result)| match result {
                None => None,
                Some(result) => Some((subscription, result)),
            })
    }
}

type UpdateSet = BTreeSet<StateToken>;

struct Subscription {
    id: SubscriptionToken,
    expression: Expression,
    dependencies: Option<(HashId, DependencyList)>,
}
impl Subscription {
    fn new(id: SubscriptionToken, expression: Expression) -> Self {
        Self {
            id,
            expression,
            dependencies: None,
        }
    }
    fn execute(
        &mut self,
        state: &DynamicState,
        updates: Option<&UpdateSet>,
        cache: &mut impl EvaluationCache,
    ) -> Option<Expression> {
        let state_hash = hash_object(&state);
        let is_unchanged = match updates {
            Some(updates) => match &self.dependencies {
                Some((previous_state_hash, dependencies)) => {
                    *previous_state_hash == state_hash || !dependencies.contains(updates)
                }
                _ => false,
            },
            _ => false,
        };
        if is_unchanged {
            return None;
        }
        let result = self.expression.evaluate(state, cache);
        let (result, dependencies) = result.into_parts();
        self.dependencies = Some((state_hash, dependencies));
        Some(result)
    }
}

fn combine_updates<'a>(updates: impl IntoIterator<Item = &'a UpdateSet>) -> Option<UpdateSet> {
    let results = updates
        .into_iter()
        .fold(BTreeSet::new(), |mut results, batch| {
            results.extend(batch.iter());
            results
        });
    if results.is_empty() {
        None
    } else {
        Some(results)
    }
}

struct FlushTask<'a, TErr> {
    subscription: &'a mut Subscription,
    result: Option<Result<Expression, Vec<TErr>>>,
    latest_update_batch: Option<NonZeroUsize>,
}
impl<'a, TErr> FlushTask<'a, TErr> {
    fn new(subscription: &'a mut Subscription) -> Self {
        Self {
            subscription,
            result: None,
            latest_update_batch: None,
        }
    }
}

struct UpdateBatch {
    current: UpdateSet,
    combined: Option<UpdateSet>,
}
impl UpdateBatch {
    fn from(keys: impl IntoIterator<Item = StateToken>) -> Self {
        Self {
            current: keys.into_iter().collect(),
            combined: None,
        }
    }
    fn append(&self, keys: impl IntoIterator<Item = StateToken>) -> Self {
        let updates = keys.into_iter().collect();
        let combined = Some(match &self.combined {
            Some(existing) => existing.union(&updates).copied().collect(),
            None => self.current.union(&updates).copied().collect(),
        });
        Self {
            current: updates,
            combined,
        }
    }
}

fn flush_recursive<'a, THandler, TErr>(
    mut tasks: Vec<FlushTask<'a, TErr>>,
    signal_handler: THandler,
    state: &mut DynamicState,
    cache: &mut impl EvaluationCache,
    mut update_batches: Vec<UpdateBatch>,
) -> Vec<Option<Result<Expression, Vec<TErr>>>>
where
    THandler: Fn(&[Signal]) -> Vec<Result<Expression, TErr>>,
{
    let current_batch_index = NonZeroUsize::new(update_batches.len());
    for task in tasks.iter_mut() {
        let combined_updates = match task.latest_update_batch {
            Some(batch_index) => combine_updates(
                update_batches[batch_index.get()..]
                    .iter()
                    .map(|batch| &batch.current),
            ),
            _ => None,
        };
        let combined_updates = combined_updates.as_ref().or_else(|| {
            update_batches
                .last()
                .map(|batch| batch.combined.as_ref().unwrap_or(&batch.current))
        });
        task.latest_update_batch = current_batch_index;
        let signal_results = match task.subscription.execute(state, combined_updates, cache) {
            None => None,
            Some(result) => match result.value() {
                Term::Signal(term) => {
                    let signals = &term
                        .signals()
                        .into_iter()
                        .map(|signal| signal.clone())
                        .collect::<Vec<_>>();
                    let results = signal_handler(&signals);
                    if results.len() != signals.len() {
                        panic!("Invalid signal handler result");
                    }
                    let updates = signals.iter().zip(results.into_iter()).map(
                        |(signal, result)| match result {
                            Ok(result) => Ok((signal.id(), result)),
                            Err(error) => Err(error),
                        },
                    );
                    let (updates, errors): (Vec<_>, Vec<_>) = partition_results(updates);
                    // TODO: Investigate storing signal handler result subscriptions/dependencies
                    Some(Err((updates, errors)))
                }
                _ => Some(Ok(result)),
            },
        };
        match signal_results {
            Some(Ok(result)) => {
                task.result = Some(Ok(result));
            }
            Some(Err((updates, errors))) => {
                if !errors.is_empty() {
                    task.result = Some(Err(errors));
                }
                update_batches.push({
                    let keys = updates.iter().map(|(key, _)| *key);
                    match update_batches.last() {
                        Some(batch) => batch.append(keys),
                        None => UpdateBatch::from(keys),
                    }
                });
                for (key, value) in updates {
                    state.set(key, value);
                }
                return flush_recursive(tasks, signal_handler, state, cache, update_batches);
            }
            _ => {}
        }
    }
    tasks.into_iter().map(|task| task.result).collect()
}

fn partition_results<TOk, TErr, TOkValues, TErrValues>(
    input: impl IntoIterator<Item = Result<TOk, TErr>>,
) -> (TOkValues, TErrValues)
where
    TOkValues: Default + Extend<TOk>,
    TErrValues: Default + Extend<TErr>,
{
    let mut oks = TOkValues::default();
    let mut errs = TErrValues::default();
    for item in input {
        match item {
            Ok(value) => oks.extend(Some(value)),
            Err(value) => errs.extend(Some(value)),
        }
    }
    (oks, errs)
}
