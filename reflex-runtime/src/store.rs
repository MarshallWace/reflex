// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{collections::BTreeSet, iter::once, num::NonZeroUsize, time::Instant};

use crate::{StateUpdate, StateUpdateType};
use itertools::{Either, Itertools};
use reflex::{
    compiler::{InstructionPointer, NativeFunctionRegistry, Program},
    core::{
        Applicable, DependencyList, DynamicState, EvaluationResult, Expression, ExpressionFactory,
        HeapAllocator, Reducible, Rewritable, Signal, SignalId, SignalType, StateToken,
    },
    hash::{hash_object, HashId},
    interpreter::{execute, InterpreterCache, InterpreterOptions},
    lang::{BuiltinTerm, ValueTerm, WithCompiledBuiltins},
    DependencyCache,
};

pub type SubscriptionToken = usize;

type HandlerError = Option<String>;

pub struct Store<
    T: Expression + Rewritable<T> + Reducible<T> + Applicable<T>,
    TCache: InterpreterCache<T>,
> {
    state: DynamicState<T>,
    builtins: Vec<(BuiltinTerm, InstructionPointer)>,
    plugins: NativeFunctionRegistry<T>,
    interpreter_options: InterpreterOptions,
    cache: TCache,
    signal_cache: RuntimeCache<T>,
    subscriptions: Vec<Subscription<T>>,
    subscription_counter: usize,
    pending_updates: Option<UpdateBatch>,
}
impl<T: Expression + Rewritable<T> + Reducible<T> + Applicable<T>, TCache: InterpreterCache<T>>
    Store<T, TCache>
{
    pub fn new(
        cache: TCache,
        state: Option<DynamicState<T>>,
        builtins: Vec<(BuiltinTerm, InstructionPointer)>,
        plugins: NativeFunctionRegistry<T>,
        interpreter_options: InterpreterOptions,
    ) -> Self {
        Self {
            state: state.unwrap_or_else(|| DynamicState::new()),
            builtins,
            plugins,
            interpreter_options,
            cache,
            signal_cache: RuntimeCache::default(),
            subscriptions: Vec::new(),
            subscription_counter: 0,
            pending_updates: None,
        }
    }
    pub fn subscribe(
        &mut self,
        target: Program,
        entry_point: InstructionPointer,
        initiators: Option<Vec<SignalId>>,
    ) -> SubscriptionToken {
        self.subscription_counter += 1;
        let subscription_id = self.subscription_counter;
        println!("[Store] Subscribe #{}", subscription_id);
        let is_root = initiators.is_none();
        let subscription = Subscription::new(subscription_id, target, entry_point, is_root);
        self.subscriptions.push(subscription);
        self.signal_cache
            .register_subscription(subscription_id, initiators.unwrap_or(Vec::new()));
        if is_root {
            self.signal_cache.retain_subscription(subscription_id);
        }
        subscription_id
    }
    pub fn unsubscribe(&mut self, subscription_id: SubscriptionToken) -> bool {
        println!("[Store] Unsubscribe #{}", subscription_id);
        self.subscriptions
            .iter()
            .position(|subscription| subscription.id == subscription_id)
            .map(|index| {
                let subscription = self.subscriptions.remove(index);
                if subscription.is_root {
                    self.signal_cache.release_subscription(subscription_id);
                }
                if let Some(cache_key) = subscription.result.and_then(|result| result.cache_key) {
                    self.cache.release(cache_key);
                }
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
        factory: &(impl ExpressionFactory<T> + WithCompiledBuiltins),
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
        println!("[Store] Flushing {} roots...", tasks.len());
        let start_time = Instant::now();
        let results = flush_recursive(
            tasks,
            signal_handler,
            &mut self.state,
            factory,
            allocator,
            &self.builtins,
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
                Some(result) => {
                    println!("[Store] Emit #{}", subscription_id);
                    Some((subscription_id, result))
                }
            })
    }
    pub fn gc(&mut self) -> (Vec<SignalId>, Vec<SubscriptionToken>) {
        let start_time = Instant::now();
        println!("[Store] GC started");
        {
            print!("[Store] Purging cache entries...");
            let start_time = Instant::now();
            let metrics = self.cache.gc();
            println!("{:?} ({})", start_time.elapsed(), metrics);
        };
        let (disposed_signals, disposed_subscriptions) = {
            print!("[Store] Computing inactive signals...");
            let start_time = Instant::now();
            let (disposed_signals, disposed_subscriptions) = self.signal_cache.gc();
            println!("{:?}", start_time.elapsed());
            if !disposed_signals.is_empty() {
                print!("[Store] Removing cached signal results...");
                let start_time = Instant::now();
                for signal_id in disposed_signals.iter() {
                    self.state.remove(signal_id);
                }
                println!("{:?}", start_time.elapsed());
            }
            (disposed_signals, disposed_subscriptions)
        };
        println!("[Store] GC completed in {:?}", start_time.elapsed());
        (disposed_signals, disposed_subscriptions)
    }
}

type UpdateSet = BTreeSet<StateToken>;

struct Subscription<T: Expression> {
    id: SubscriptionToken,
    target: Program,
    entry_point: InstructionPointer,
    is_root: bool,
    result: Option<SubscriptionResult<T>>,
}
struct SubscriptionResult<T: Expression> {
    result: EvaluationResult<T>,
    cache_key: Option<HashId>,
    state_hash: HashId,
}
impl<T: Expression> SubscriptionResult<T> {
    fn result(&self) -> &EvaluationResult<T> {
        &self.result
    }
}
impl<T: Expression + Rewritable<T> + Reducible<T> + Applicable<T>> Subscription<T> {
    fn new(
        id: SubscriptionToken,
        target: Program,
        entry_point: InstructionPointer,
        is_root: bool,
    ) -> Self {
        Self {
            id,
            target,
            entry_point,
            is_root,
            result: None,
        }
    }
    fn execute(
        &mut self,
        state: &DynamicState<T>,
        updates: Option<&UpdateSet>,
        factory: &(impl ExpressionFactory<T> + WithCompiledBuiltins),
        allocator: &impl HeapAllocator<T>,
        builtins: &[(BuiltinTerm, InstructionPointer)],
        plugins: &NativeFunctionRegistry<T>,
        interpreter_options: &InterpreterOptions,
        cache: &mut impl InterpreterCache<T>,
    ) -> (Option<T>, Option<&DependencyList>) {
        let state_hash = hash_object(&state);
        let is_unchanged = match updates {
            Some(updates) => match &self.result {
                Some(result) => {
                    result.state_hash == state_hash
                        || !result.result.dependencies().intersects(updates)
                }
                _ => false,
            },
            _ => false,
        };
        if is_unchanged {
            return (None, None);
        }
        let (result, cache_key) = execute(
            &self.target,
            self.entry_point,
            state,
            factory,
            allocator,
            builtins,
            plugins,
            interpreter_options,
            cache,
        )
        .map(|(result, cache_key)| (result, Some(cache_key)))
        .unwrap_or_else(|error| {
            let result = EvaluationResult::new(
                create_error_signal(error, factory, allocator),
                DependencyList::empty(),
            );
            let cache_key = None;
            (result, cache_key)
        });
        let previous_result = &self.result;
        let previous_cache_key = previous_result.as_ref().and_then(|result| result.cache_key);
        let updated_value = match previous_result {
            Some(previous) if previous.result.result().id() == result.result().id() => None,
            _ => Some(result.result().clone()),
        };
        self.result = Some(SubscriptionResult {
            result,
            cache_key,
            state_hash,
        });
        if let Some(cache_key) = cache_key {
            cache.retain(cache_key);
        }
        if let Some(previous_cache_key) = previous_cache_key {
            cache.release(previous_cache_key);
        }
        (
            updated_value,
            self.result
                .as_ref()
                .map(|result| result.result().dependencies()),
        )
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

struct FlushTask<'a, T: Expression + Rewritable<T> + Reducible<T> + Applicable<T>> {
    subscription: &'a mut Subscription<T>,
    result: Option<Result<T, Vec<String>>>,
    latest_update_batch: Option<NonZeroUsize>,
}
impl<'a, T: Expression + Rewritable<T> + Reducible<T> + Applicable<T>> FlushTask<'a, T> {
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

fn flush_recursive<'a, T: Expression + Rewritable<T> + Reducible<T> + Applicable<T>, THandler>(
    mut tasks: Vec<FlushTask<'a, T>>,
    signal_handler: THandler,
    state: &mut DynamicState<T>,
    factory: &(impl ExpressionFactory<T> + WithCompiledBuiltins),
    allocator: &impl HeapAllocator<T>,
    builtins: &[(BuiltinTerm, InstructionPointer)],
    plugins: &NativeFunctionRegistry<T>,
    interpreter_options: &InterpreterOptions,
    cache: &mut impl InterpreterCache<T>,
    signal_cache: &mut RuntimeCache<T>,
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
        let subscription_id = task.subscription.id;
        let (result, dependencies) = task.subscription.execute(
            state,
            combined_updates,
            factory,
            allocator,
            builtins,
            plugins,
            interpreter_options,
            cache,
        );
        let updated_result = match result {
            None => None,
            Some(value) => match factory.match_signal_term(&value) {
                Some(value) => {
                    let (existing_signal_results, added_signals): (Vec<_>, Vec<&Signal<T>>) =
                        value.signals().into_iter().partition_map(|signal| {
                            let signal_id = signal.id();
                            match signal_cache.retrieve_signal_result(signal_id) {
                                Some(existing) => Either::Left((signal_id, existing.clone())),
                                None => Either::Right(signal),
                            }
                        });
                    let added_signal_results = added_signals
                        .iter()
                        .map(|signal| signal.id())
                        .zip(signal_handler(added_signals.as_slice()));
                    let combined_signal_results = added_signal_results
                        .chain(existing_signal_results)
                        .inspect(|(signal_id, result)| {
                            signal_cache.register_signal(*signal_id, result.clone());
                        });
                    let (updates, errors): (Vec<_>, Vec<_>) = combined_signal_results
                        .map(|(id, result)| match result {
                            Ok(result) => Ok((id, result)),
                            Err(error) => Err(error),
                        })
                        .partition_result();
                    let errors = errors.into_iter().flatten().collect::<Vec<String>>();
                    Some(Err((updates, errors)))
                }
                _ => Some(Ok(value)),
            },
        };
        if let Some(dependencies) = dependencies {
            signal_cache.update_subscription_dependencies(subscription_id, dependencies);
        }
        if let Some(result) = updated_result {
            match result {
                Ok(result) => {
                    task.result = Some(Ok(result));
                }
                Err((updates, errors)) => {
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
                        builtins,
                        plugins,
                        interpreter_options,
                        cache,
                        signal_cache,
                        update_batches,
                    );
                }
            }
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

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum RuntimeCacheKey {
    Signal(SignalId),
    Subscription(SubscriptionToken),
}

enum RuntimeCacheEntry<T: Expression> {
    Signal(Result<T, HandlerError>),
    Subscription(HashId),
}
struct RuntimeCache<T: Expression> {
    cache: DependencyCache<RuntimeCacheKey, RuntimeCacheEntry<T>>,
}
impl<T: Expression> Default for RuntimeCache<T> {
    fn default() -> Self {
        Self {
            cache: DependencyCache::default(),
        }
    }
}
impl<T: Expression> RuntimeCache<T> {
    fn register_subscription(
        &mut self,
        subscription_id: SubscriptionToken,
        initiators: impl IntoIterator<Item = SignalId>,
    ) {
        let key = RuntimeCacheKey::Subscription(subscription_id);
        let value = RuntimeCacheEntry::Subscription(0);
        self.cache.set(key, value, None);
        for signal_id in initiators {
            self.cache
                .add_child(&RuntimeCacheKey::Signal(signal_id), &key);
        }
    }
    fn update_subscription_dependencies(
        &mut self,
        subscription_id: SubscriptionToken,
        dependencies: &DependencyList,
    ) -> bool {
        let key = RuntimeCacheKey::Subscription(subscription_id);
        let dependencies_hash = hash_object(dependencies);
        match self
            .cache
            .replace_value(&key, RuntimeCacheEntry::Subscription(dependencies_hash))
        {
            Some(RuntimeCacheEntry::Subscription(previous_dependencies_hash))
                if previous_dependencies_hash != dependencies_hash =>
            {
                self.cache.replace_children(
                    &key,
                    dependencies
                        .iter()
                        .map(|signal_id| RuntimeCacheKey::Signal(signal_id))
                        .filter(|key| self.cache.contains_key(key))
                        .collect(),
                )
            }
            _ => false,
        }
    }
    fn retain_subscription(&mut self, subscription_id: SubscriptionToken) {
        let key = RuntimeCacheKey::Subscription(subscription_id);
        self.cache.retain(once(&key));
    }
    fn release_subscription(&mut self, subscription_id: SubscriptionToken) {
        let key = RuntimeCacheKey::Subscription(subscription_id);
        self.cache.release(once(&key));
    }
    fn register_signal(&mut self, signal_id: SignalId, result: Result<T, HandlerError>) {
        self.cache.set(
            RuntimeCacheKey::Signal(signal_id),
            RuntimeCacheEntry::Signal(result),
            None,
        )
    }
    fn retrieve_signal_result(&self, signal_id: SignalId) -> Option<&Result<T, HandlerError>> {
        self.cache
            .get(&RuntimeCacheKey::Signal(signal_id))
            .and_then(|entry| match entry {
                RuntimeCacheEntry::Signal(result) => Some(result),
                _ => None,
            })
    }
    fn gc(&mut self) -> (Vec<SignalId>, Vec<SubscriptionToken>) {
        self.cache
            .gc::<Vec<_>>()
            .into_iter()
            .partition_map(|(key, _)| match key {
                RuntimeCacheKey::Signal(signal_id) => Either::Left(signal_id),
                RuntimeCacheKey::Subscription(subscription_id) => Either::Right(subscription_id),
            })
    }
}
