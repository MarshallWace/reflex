// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{HashMap, HashSet},
    num::NonZeroUsize,
};

use reflex::{
    cache::EvaluationCache,
    core::{DependencyList, DynamicState, Expression, Signal, StateToken},
    hash::{HashId, Hashable},
};

pub type SubscriptionToken = usize;

pub struct Store {
    state: DynamicState,
    cache: EvaluationCache,
    subscriptions: Vec<Subscription>,
    subscription_counter: usize,
    is_flushing: bool,
}
impl Store {
    pub fn new(state: Option<DynamicState>) -> Self {
        Self {
            state: state.unwrap_or_else(|| DynamicState::new()),
            subscriptions: Vec::new(),
            cache: EvaluationCache::new(),
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
        Vec<(SubscriptionToken, Result<Expression, TErr>)>,
    )
    where
        THandler: Fn(&[Signal]) -> Result<Vec<Expression>, TErr> + Sync + Send,
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
        updates: impl IntoIterator<Item = (StateToken, Expression)>,
        signal_handler: THandler,
    ) -> Vec<(SubscriptionToken, Result<Expression, TErr>)>
    where
        THandler: Fn(&[Signal]) -> Result<Vec<Expression>, TErr> + Sync + Send,
    {
        let updates = updates
            .into_iter()
            .filter(|(key, value)| match self.state.get(*key) {
                Some(existing) => existing.hash() != value.hash(),
                None => true,
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
    ) -> impl IntoIterator<Item = (&Subscription, Result<Expression, TErr>)> + 'b
    where
        THandler: Fn(&[Signal]) -> Result<Vec<Expression>, TErr> + Sync + Send,
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

type UpdateSet = HashSet<StateToken>;

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
        cache: &mut EvaluationCache,
    ) -> Option<Result<Expression, Vec<Signal>>> {
        let state_hash = state.hash();
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
        let (result, dependencies) = result.unwrap();
        self.dependencies = Some((state_hash, dependencies));
        Some(result)
    }
}

fn combine_updates<'a>(updates: impl IntoIterator<Item = &'a UpdateSet>) -> Option<UpdateSet> {
    let results = updates
        .into_iter()
        .fold(HashSet::new(), |mut results, batch| {
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
    result: Option<Result<Expression, TErr>>,
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
    cache: &mut EvaluationCache,
    mut update_batches: Vec<UpdateBatch>,
) -> Vec<Option<Result<Expression, TErr>>>
where
    THandler: Fn(&[Signal]) -> Result<Vec<Expression>, TErr>,
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
        let signal_updates = match task.subscription.execute(state, combined_updates, cache) {
            None => None,
            Some(result) => match result {
                Ok(expression) => {
                    task.result = Some(Ok(expression));
                    None
                }
                Err(signals) => {
                    let signals = deduplicate_signals(signals);
                    match signal_handler(&signals) {
                        Err(error) => {
                            task.result = Some(Err(error));
                            None
                        }
                        Ok(values) => {
                            if values.len() != signals.len() {
                                panic!("Invalid signal handler result");
                            }
                            let updates = signals
                                .iter()
                                .map(|signal| signal.hash())
                                .zip(values.into_iter())
                                .collect::<Vec<_>>();
                            Some(updates)
                        }
                    }
                }
            },
        };
        if let Some(updates) = signal_updates {
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
    }
    tasks.into_iter().map(|task| task.result).collect()
}

fn deduplicate_signals(signals: Vec<Signal>) -> Vec<Signal> {
    // TODO: Improve signal deduplication performance
    signals
        .into_iter()
        .map(|signal| (signal.hash(), signal))
        .collect::<HashMap<_, _>>()
        .drain()
        .map(|(_, signal)| signal)
        .collect::<Vec<_>>()
}
