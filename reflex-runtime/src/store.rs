// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{BTreeSet, HashMap},
    num::NonZeroUsize,
};

use crate::{StateUpdate, StateUpdateType};
use itertools::{Either, Itertools};
use reflex::{
    cache::EvaluationCache,
    core::{DependencyList, DynamicState, Expression, Signal, StateToken, Term},
    hash::{hash_object, HashId},
};

pub type SubscriptionToken = usize;

type SignalCache = HashMap<StateToken, Result<Expression, HandlerError>>;
type HandlerError = Option<String>;

pub struct Store<T: EvaluationCache> {
    state: DynamicState,
    cache: T,
    signal_cache: SignalCache,
    subscriptions: Vec<Subscription>,
    subscription_counter: usize,
    pending_updates: Option<UpdateBatch>,
}
impl<T: EvaluationCache> Store<T> {
    pub fn new(cache: T, state: Option<DynamicState>) -> Self {
        Self {
            state: state.unwrap_or_else(|| DynamicState::new()),
            cache,
            signal_cache: SignalCache::new(),
            subscriptions: Vec::new(),
            subscription_counter: 0,
            pending_updates: None,
        }
    }
    pub fn subscribe(&mut self, expression: Expression) -> SubscriptionToken {
        self.subscription_counter += 1;
        let subscription_id = self.subscription_counter;
        let subscription = Subscription::new(subscription_id, expression);
        self.subscriptions.push(subscription);
        subscription_id
    }
    pub fn unsubscribe(&mut self, subscription_id: SubscriptionToken) -> bool {
        self.subscriptions
            .iter()
            .position(|subscription| subscription.id == subscription_id)
            .map(|index| {
                self.subscriptions.remove(index);
                true
            })
            .unwrap_or(false)
    }
    pub fn update(&mut self, updates: impl IntoIterator<Item = StateUpdate>) {
        let updates = updates
            .into_iter()
            .filter_map(|update| {
                let key = update.state_token;
                let existing_value = self.state.get(key);
                let existing_hash = existing_value.map(hash_object);
                let value = match update.update {
                    StateUpdateType::Value(value) => value,
                    StateUpdateType::Patch(updater) => updater(existing_value),
                };
                match existing_hash {
                    Some(hash) if hash == hash_object(&value) => None,
                    _ => Some((key, value)),
                }
            })
            .collect::<Vec<_>>();
        if updates.is_empty() {
            return;
        }
        let updated_keys = updates.iter().map(|(key, _)| *key);
        self.pending_updates = Some(match self.pending_updates.take() {
            Some(existing) => existing.extend(updated_keys),
            None => UpdateBatch::from(updated_keys),
        });
        for (key, value) in updates {
            self.state.set(key, value);
        }
    }
    pub fn flush<THandler>(
        &mut self,
        signal_handler: THandler,
    ) -> impl IntoIterator<Item = (SubscriptionToken, Result<Expression, Vec<String>>)> + '_
    where
        THandler: Fn(&[&Signal]) -> Vec<Result<Expression, HandlerError>> + Sync + Send,
    {
        let tasks = self
            .subscriptions
            .iter_mut()
            .map(FlushTask::new)
            .collect::<Vec<_>>();
        let updates = self.pending_updates.take();
        let results = flush_recursive(
            tasks,
            signal_handler,
            &mut self.state,
            &mut self.cache,
            &mut self.signal_cache,
            match updates {
                Some(updates) => vec![updates],
                None => Vec::new(),
            },
        );
        self.subscriptions
            .iter()
            .map(|subscription| subscription.id)
            .zip(results)
            .filter_map(|(subscription_id, result)| match result {
                None => None,
                Some(result) => Some((subscription_id, result)),
            })
    }
}

type UpdateSet = BTreeSet<StateToken>;

struct Subscription {
    id: SubscriptionToken,
    expression: Expression,
    result: Option<(HashId, Expression, DependencyList)>,
}
impl Subscription {
    fn new(id: SubscriptionToken, expression: Expression) -> Self {
        Self {
            id,
            expression,
            result: None,
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
            Some(updates) => match &self.result {
                Some((previous_state_hash, _, dependencies)) => {
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
        let updated_result = match &self.result {
            Some((_, previous_result, _))
                if (hash_object(&result) == hash_object(previous_result)) =>
            {
                None
            }
            _ => Some(Expression::clone(&result)),
        };
        self.result = Some((state_hash, result, dependencies));
        updated_result
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

struct FlushTask<'a> {
    subscription: &'a mut Subscription,
    result: Option<Result<Expression, Vec<String>>>,
    latest_update_batch: Option<NonZeroUsize>,
}
impl<'a> FlushTask<'a> {
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
    fn extend(mut self, keys: impl IntoIterator<Item = StateToken>) -> Self {
        match self.combined {
            None => {
                self.current.extend(keys);
                Self {
                    current: self.current,
                    combined: None,
                }
            }
            Some(mut combined) => {
                let keys = keys.into_iter().collect::<Vec<_>>();
                self.current.extend(keys.iter().copied());
                combined.extend(keys.iter().copied());
                Self {
                    current: self.current,
                    combined: Some(combined),
                }
            }
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

fn flush_recursive<'a, THandler>(
    mut tasks: Vec<FlushTask<'a>>,
    signal_handler: THandler,
    state: &mut DynamicState,
    cache: &mut impl EvaluationCache,
    signal_cache: &mut SignalCache,
    mut update_batches: Vec<UpdateBatch>,
) -> Vec<Option<Result<Expression, Vec<String>>>>
where
    THandler: Fn(&[&Signal]) -> Vec<Result<Expression, HandlerError>>,
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
                    let (existing_signals, added_signals): (Vec<_>, Vec<&Signal>) =
                        term.signals().into_iter().partition_map(|signal| {
                            let signal_id = signal.id();
                            match signal_cache.get(&signal_id) {
                                Some(result) => match result {
                                    Ok(result) => {
                                        Either::Left((signal_id, Ok(Expression::clone(result))))
                                    }
                                    Err(error) => Either::Left((signal_id, Err(error.clone()))),
                                },
                                None => Either::Right(signal),
                            }
                        });
                    let signal_results = added_signals
                        .iter()
                        .map(|signal| signal.id())
                        .zip(signal_handler(added_signals.as_slice()))
                        .inspect(|(signal_id, result)| {
                            signal_cache.insert(*signal_id, result.clone());
                        });
                    let (updates, errors): (Vec<_>, Vec<_>) = existing_signals
                        .into_iter()
                        .chain(signal_results)
                        .map(|(id, result)| match result {
                            Ok(result) => Ok((id, result)),
                            Err(error) => Err(error),
                        })
                        .partition_result();
                    let errors = errors.into_iter().flatten().collect::<Vec<String>>();
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
                return flush_recursive(
                    tasks,
                    signal_handler,
                    state,
                    cache,
                    signal_cache,
                    update_batches,
                );
            }
            _ => {}
        }
    }
    tasks.into_iter().map(|task| task.result).collect()
}
