// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{collections::{BTreeSet, HashMap}, iter::once, num::NonZeroUsize, time::Instant};

use crate::{StateUpdate, StateUpdateType};
use itertools::{Either, Itertools};
use reflex::{compiler::{InstructionPointer, NativeFunctionRegistry, Program}, core::{
        Applicable, DependencyList, DynamicState, EvaluationResult, Expression, ExpressionFactory,
        HeapAllocator, Reducible, Rewritable, Signal, SignalType, StateToken,
    }, hash::{hash_object, HashId}, interpreter::{execute, Execute, InterpreterCache, InterpreterOptions}, lang::ValueTerm};

pub type SubscriptionToken = usize;

#[allow(type_alias_bounds)]
type SignalCache<T: Expression> = HashMap<StateToken, Result<T, HandlerError>>;
type HandlerError = Option<String>;

pub struct Store<
    T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Execute<T>,
    TCache: InterpreterCache<T>,
> {
    state: DynamicState<T>,
    plugins: NativeFunctionRegistry<T>,
    interpreter_options: InterpreterOptions,
    cache: TCache,
    signal_cache: SignalCache<T>,
    subscriptions: Vec<Subscription<T>>,
    subscription_counter: usize,
    pending_updates: Option<UpdateBatch>,
}
impl<
        T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Execute<T>,
        TCache: InterpreterCache<T>,
    > Store<T, TCache>
{
    pub fn new(
        cache: TCache,
        state: Option<DynamicState<T>>,
        plugins: NativeFunctionRegistry<T>,
        interpreter_options: InterpreterOptions,
    ) -> Self {
        Self {
            state: state.unwrap_or_else(|| DynamicState::new()),
            plugins,
            interpreter_options,
            cache,
            signal_cache: SignalCache::new(),
            subscriptions: Vec::new(),
            subscription_counter: 0,
            pending_updates: None,
        }
    }
    pub fn subscribe(&mut self, target: Program, entry_point: InstructionPointer) -> SubscriptionToken {
        self.subscription_counter += 1;
        let subscription_id = self.subscription_counter;
        let subscription = Subscription::new(subscription_id, target, entry_point);
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
    pub fn update(&mut self, updates: impl IntoIterator<Item = StateUpdate<T>>) {
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
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> impl IntoIterator<Item = (SubscriptionToken, Result<T, Vec<String>>)> + '_
    where
        THandler: Fn(&[&Signal<T>]) -> Vec<Result<T, HandlerError>>,
    {
        let tasks = self
            .subscriptions
            .iter_mut()
            .map(FlushTask::new)
            .collect::<Vec<_>>();
        let updates = self.pending_updates.take();
        println!("[Store] Flushing...");
        let start_time = Instant::now();
        let results = flush_recursive(
            tasks,
            signal_handler,
            &mut self.state,
            factory,
            allocator,
            &self.plugins,
            &self.interpreter_options,
            &mut self.cache,
            &mut self.signal_cache,
            match updates {
                Some(updates) => vec![updates],
                None => Vec::new(),
            },
        );
        println!("[Store] Flush completed in {:?}", start_time.elapsed());
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

struct Subscription<T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Execute<T>> {
    id: SubscriptionToken,
    target: Program,
    entry_point: InstructionPointer,
    result: Option<(HashId, T, DependencyList)>,
}
impl<T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Execute<T>> Subscription<T> {
    fn new(id: SubscriptionToken, target: Program, entry_point: InstructionPointer) -> Self {
        Self {
            id,
            target,
            entry_point,
            result: None,
        }
    }
    fn execute(
        &mut self,
        state: &DynamicState<T>,
        updates: Option<&UpdateSet>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        plugins: &NativeFunctionRegistry<T>,
        interpreter_options: &InterpreterOptions,
        cache: &mut impl InterpreterCache<T>,
    ) -> Option<T> {
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
        let result = execute(
            &self.target,
            self.entry_point,
            state,
            factory,
            allocator,
            plugins,
            interpreter_options,
            cache,
        )
        .unwrap_or_else(|error| {
            EvaluationResult::new(
                create_error_signal(error, factory, allocator),
                DependencyList::empty(),
            )
        });
        let (result, dependencies) = result.into_parts();
        let updated_result = match &self.result {
            Some((_, previous_result, _)) if (result.id() == previous_result.id()) => None,
            _ => Some(result.clone()),
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

struct FlushTask<'a, T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Execute<T>> {
    subscription: &'a mut Subscription<T>,
    result: Option<Result<T, Vec<String>>>,
    latest_update_batch: Option<NonZeroUsize>,
}
impl<'a, T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Execute<T>>
    FlushTask<'a, T>
{
    fn new(subscription: &'a mut Subscription<T>) -> Self {
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

fn flush_recursive<
    'a,
    T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Execute<T>,
    THandler,
>(
    mut tasks: Vec<FlushTask<'a, T>>,
    signal_handler: THandler,
    state: &mut DynamicState<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    plugins: &NativeFunctionRegistry<T>,
    interpreter_options: &InterpreterOptions,
    cache: &mut impl InterpreterCache<T>,
    signal_cache: &mut SignalCache<T>,
    mut update_batches: Vec<UpdateBatch>,
) -> Vec<Option<Result<T, Vec<String>>>>
where
    THandler: Fn(&[&Signal<T>]) -> Vec<Result<T, HandlerError>>,
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
        let signal_results = match task.subscription.execute(
            state,
            combined_updates,
            factory,
            allocator,
            plugins,
            interpreter_options,
            cache,
        ) {
            None => None,
            Some(result) => match factory.match_signal_term(&result) {
                Some(result) => {
                    let (existing_signals, added_signals): (Vec<_>, Vec<&Signal<T>>) =
                        result.signals().into_iter().partition_map(|signal| {
                            let signal_id = signal.id();
                            match signal_cache.get(&signal_id) {
                                Some(existing) => match existing {
                                    Ok(result) => Either::Left((signal_id, Ok(result.clone()))),
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
                    factory,
                    allocator,
                    plugins,
                    interpreter_options,
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

fn create_error_signal<T: Expression>(
    message: String,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_signal_term(allocator.create_signal_list(once(allocator.create_signal(
        SignalType::Error,
        allocator.create_unit_list(
            factory.create_value_term(ValueTerm::String(allocator.create_string(message))),
        ),
    ))))
}
