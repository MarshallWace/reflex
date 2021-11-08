// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use futures::{
    future::{AbortHandle, Abortable},
    stream, FutureExt,
};
use itertools::{Either, Itertools};
use parking_lot::RwLock;
use reflex::{
    compiler::{
        create_main_function, hash_program_root, Compile, Compiler, CompilerMode, CompilerOptions,
        Instruction, InstructionPointer, Program,
    },
    core::{
        Applicable, DependencyList, DynamicState, EvaluationResult, Expression, ExpressionFactory,
        HeapAllocator, Reducible, Rewritable, Signal, SignalId, SignalType, StateCache, StateToken,
        StringValue, Uuid,
    },
    hash::{hash_object, HashId},
    interpreter::{
        execute, CacheEntries, DefaultInterpreterCache, GcMetrics, InterpreterCache,
        InterpreterCacheEntry, InterpreterCacheKey, InterpreterOptions,
    },
    lang::ValueTerm,
    DependencyCache,
};
use std::{
    collections::{
        hash_map::{DefaultHasher, Entry},
        HashMap, HashSet,
    },
    convert::identity,
    future::Future,
    hash::{Hash, Hasher},
    iter::once,
    pin::Pin,
    sync::{Arc, Mutex},
    task::{Context, Poll},
    time::Instant,
};
use tokio::{
    pin,
    sync::{broadcast, mpsc, oneshot, watch},
};
use tokio_stream::wrappers::{BroadcastStream, WatchStream};
pub use tokio_stream::{Stream, StreamExt};

pub type SignalResult<T> = (T, Option<RuntimeEffect<T>>);
pub type SignalHandlerResult<T> = Option<Vec<Result<SignalResult<T>, String>>>;

pub trait AsyncExpression: Expression + Send + Sync + Unpin + 'static {}
impl<T> AsyncExpression for T
where
    T: Expression + Send + Sync + Unpin + 'static,
    T::String: StringValue + Send + Sync,
{
}
pub trait AsyncExpressionFactory<T: AsyncExpression>:
    ExpressionFactory<T> + Send + Sync + Clone + 'static
{
}
impl<T, E: AsyncExpression> AsyncExpressionFactory<E> for T
where
    T: ExpressionFactory<E> + Send + Sync + Clone + 'static,
    E::String: StringValue + Send + Sync,
{
}
pub trait AsyncHeapAllocator<T: AsyncExpression>:
    HeapAllocator<T> + Send + Sync + Clone + 'static
{
}
impl<T, E: AsyncExpression> AsyncHeapAllocator<E> for T
where
    T: HeapAllocator<E> + Send + Sync + Clone + 'static,
    E::String: StringValue + Send + Sync,
{
}
pub trait AsyncInterpreterCache<T: AsyncExpression>:
    InterpreterCache<T> + Send + Sync + Clone + 'static
{
}
impl<T, E: AsyncExpression> AsyncInterpreterCache<E> for T
where
    T: InterpreterCache<E> + Send + Sync + Clone + 'static,
    E::String: StringValue + Send + Sync,
{
}

pub type SubscriptionId = usize;

pub struct StateUpdate<T: Expression> {
    state_token: StateToken,
    update: StateUpdateType<T>,
}
impl<T: Expression> StateUpdate<T> {
    pub fn update(&self) -> &StateUpdateType<T> {
        &self.update
    }
}
pub enum StateUpdateType<T: Expression> {
    Value(T),
    Patch(Box<dyn Fn(Option<&T>) -> T + Send + Sync + 'static>),
}
impl<T: Expression> StateUpdate<T> {
    pub fn value(state_token: StateToken, value: T) -> Self {
        Self {
            state_token,
            update: StateUpdateType::Value(value),
        }
    }
    pub fn state_token(&self) -> StateToken {
        self.state_token
    }
    pub fn patch(
        state_token: StateToken,
        updater: impl Fn(Option<&T>) -> T + Send + Sync + 'static,
    ) -> Self {
        Self {
            state_token,
            update: StateUpdateType::Patch(Box::new(updater)),
        }
    }
}

pub type SyncEffect<T> = Vec<StateUpdate<T>>;
pub type AsyncEffect<T> = (
    Pin<Box<dyn Future<Output = Vec<StateUpdate<T>>> + Send + 'static>>,
    DisposeCallback,
);
pub type StreamEffect<T> = (
    Pin<Box<dyn Stream<Item = Vec<StateUpdate<T>>> + Send + 'static>>,
    DisposeCallback,
);
pub type DisposeCallback = Pin<Box<dyn Future<Output = ()> + Send + 'static>>;

pub enum RuntimeEffect<T: Expression> {
    Sync(SyncEffect<T>),
    Async(AsyncEffect<T>),
    Stream(StreamEffect<T>),
}
impl<T: Expression> RuntimeEffect<T> {
    pub fn assign(state_token: StateToken, value: T) -> Self {
        Self::batch_assign(once((state_token, value)))
    }
    pub fn update(
        state_token: StateToken,
        updater: impl Fn(Option<&T>) -> T + Send + Sync + 'static,
    ) -> Self {
        Self::batch_update(once((state_token, updater)))
    }
    pub fn deferred(
        update: impl Future<Output = StateUpdate<T>> + Send + 'static,
        dispose: impl Future<Output = ()> + Send + 'static,
    ) -> Self {
        Self::batch_deferred(update.map(|update| vec![update]), dispose)
    }
    pub fn stream(
        update: impl Stream<Item = StateUpdate<T>> + Send + 'static,
        dispose: impl Future<Output = ()> + Send + 'static,
    ) -> Self {
        Self::batch_stream(update.map(|update| vec![update]), dispose)
    }
    pub fn batch_assign(values: impl IntoIterator<Item = (StateToken, T)>) -> Self {
        Self::Sync(
            values
                .into_iter()
                .map(|(state_token, value)| StateUpdate::value(state_token, value))
                .collect::<Vec<_>>(),
        )
    }
    pub fn batch_update(
        updates: impl IntoIterator<
            Item = (StateToken, impl Fn(Option<&T>) -> T + Send + Sync + 'static),
        >,
    ) -> Self {
        Self::Sync(
            updates
                .into_iter()
                .map(|(state_token, updater)| StateUpdate::patch(state_token, Box::new(updater)))
                .collect::<Vec<_>>(),
        )
    }
    pub fn batch_deferred(
        updates: impl Future<Output = Vec<StateUpdate<T>>> + Send + 'static,
        dispose: impl Future<Output = ()> + Send + 'static,
    ) -> Self {
        Self::Async((Box::pin(updates), Box::pin(dispose)))
    }
    pub fn batch_stream(
        updates: impl Stream<Item = Vec<StateUpdate<T>>> + Send + 'static,
        dispose: impl Future<Output = ()> + Send + 'static,
    ) -> Self {
        Self::Stream((Box::pin(updates), Box::pin(dispose)))
    }
}

#[derive(Clone)]
pub struct RuntimeState<T: Hash + Clone> {
    hash: HashId,
    values: im::HashMap<StateToken, T>,
}
impl<T: Hash + Clone> Default for RuntimeState<T> {
    fn default() -> Self {
        Self {
            hash: DefaultHasher::new().finish(),
            values: im::HashMap::default(),
        }
    }
}
impl<T: Hash + Clone> Hash for RuntimeState<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash)
    }
}
impl<T: Hash + Clone> RuntimeState<T> {
    pub fn set(&mut self, key: StateToken, value: T) {
        self.extend(once((key, value)))
    }
    pub fn extend(&mut self, entries: impl IntoIterator<Item = (StateToken, T)>) {
        let mut has_changes = false;
        let mut hasher = DefaultHasher::new();
        hasher.write_u64(self.hash);
        for (key, value) in entries {
            let value_hash = hash_object(&value);
            let previous = self.values.insert(key, value);
            let was_updated = match &previous {
                Some(previous_value) => value_hash != hash_object(previous_value),
                None => true,
            };
            if was_updated {
                has_changes = true;
                hasher.write_u64(key);
                hasher.write_u64(value_hash);
            }
        }
        if has_changes {
            self.hash = hasher.finish();
        }
    }
    pub fn remove(&mut self, key: &StateToken) -> Option<T> {
        self.remove_many(once(key))
            .into_iter()
            .next()
            .and_then(|value| value)
    }
    pub fn remove_many<'a>(
        &'a mut self,
        keys: impl IntoIterator<
            Item = &'a StateToken,
            IntoIter = impl Iterator<Item = &'a StateToken> + 'a,
        >,
    ) -> impl IntoIterator<Item = Option<T>> + 'a {
        keys.into_iter().map(move |key| {
            let result = self.values.remove(&key);
            if result.is_some() {
                let mut hasher = DefaultHasher::new();
                hasher.write_u64(self.hash);
                hasher.write_u64(*key);
                hasher.write_u8(0);
                self.hash = hasher.finish();
            }
            result
        })
    }
}
impl<T: Hash + Clone> DynamicState<T> for RuntimeState<T> {
    fn id(&self) -> HashId {
        self.hash
    }
    fn has(&self, key: &StateToken) -> bool {
        self.values.contains_key(key)
    }
    fn get(&self, key: &StateToken) -> Option<&T> {
        self.values.get(key)
    }
}

pub struct StreamSubscription<'a, T: Expression, TUpdates: Stream<Item = T>> {
    channel: &'a RuntimeCommandChannel<T>,
    id: SubscriptionId,
    updates: TUpdates,
}
impl<'a, T: Expression, TUpdates: Stream<Item = T>> StreamSubscription<'a, T, TUpdates> {
    fn new(channel: &'a RuntimeCommandChannel<T>, id: SubscriptionId, updates: TUpdates) -> Self {
        Self {
            channel,
            id,
            updates,
        }
    }
    pub fn id(&self) -> SubscriptionId {
        self.id
    }
    pub fn into_stream(self) -> TUpdates {
        self.updates
    }
    pub async fn unsubscribe(&self) -> Result<bool, String> {
        let (send, receive) = oneshot::channel();
        let _ = self
            .channel
            .send(RuntimeCommand::unsubscribe(self.id, send))
            .await;
        match receive.await {
            Err(error) => Err(format!("{}", error)),
            Ok(result) => Ok(result),
        }
    }
}
impl<'a, T: Expression, TUpdates: Stream<Item = T> + Unpin> Stream
    for StreamSubscription<'a, T, TUpdates>
{
    type Item = T;

    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let updates = &mut self.updates;
        pin!(updates);
        Stream::poll_next(updates, cx)
    }
}

struct AsyncOperation<TRequest, TResponse> {
    payload: TRequest,
    response: oneshot::Sender<TResponse>,
}
impl<TRequest, TResponse> AsyncOperation<TRequest, TResponse> {
    fn new(payload: TRequest, response: oneshot::Sender<TResponse>) -> Self {
        Self { payload, response }
    }
}

struct StreamOperation<TRequest, TResponse, TUpdate> {
    payload: TRequest,
    response: oneshot::Sender<TResponse>,
    updates: watch::Sender<Option<TUpdate>>,
}
impl<TRequest, TResponse, TUpdate> StreamOperation<TRequest, TResponse, TUpdate> {
    fn new(
        payload: TRequest,
        response: oneshot::Sender<TResponse>,
        updates: watch::Sender<Option<TUpdate>>,
    ) -> Self {
        Self {
            payload,
            response,
            updates,
        }
    }
}

type RuntimeCommandChannel<T> = mpsc::Sender<RuntimeCommand<T>>;
type FlushPayload<T> = Arc<(RuntimeState<T>, Vec<StateToken>)>;

enum RuntimeCommand<T: Expression> {
    Subscribe(StreamOperation<SubscribeCommand, SubscriptionId, T>),
    Unsubscribe(AsyncOperation<UnsubscribeCommand, bool>),
    Emit(EmitCommand<T>),
    Update(UpdateCommand<T>),
}
struct SubscribeCommand {
    target: Program,
    entry_point: InstructionPointer,
    initiators: Option<Vec<SignalId>>,
}
struct UnsubscribeCommand {
    subscription_id: SubscriptionId,
}
struct UpdateCommand<T: Expression> {
    updates: Vec<StateUpdate<T>>,
}
struct EmitCommand<T: Expression> {
    subscription_id: SubscriptionId,
    cache_key: HashId,
    program: Arc<Program>,
    state: RuntimeState<T>,
    result: EvaluationResult<T>,
    cache_entries: CacheEntries<T>,
}
impl<T: Expression> RuntimeCommand<T> {
    fn subscribe(
        target: Program,
        entry_point: InstructionPointer,
        initiators: Option<Vec<SignalId>>,
        response: oneshot::Sender<SubscriptionId>,
        updates: watch::Sender<Option<T>>,
    ) -> Self {
        let payload = SubscribeCommand {
            target,
            entry_point,
            initiators,
        };
        Self::Subscribe(StreamOperation::new(payload, response, updates))
    }
    fn unsubscribe(subscription_id: SubscriptionId, response: oneshot::Sender<bool>) -> Self {
        let payload = UnsubscribeCommand { subscription_id };
        Self::Unsubscribe(AsyncOperation::new(payload, response))
    }
    fn emit(
        subscription_id: SubscriptionId,
        cache_key: HashId,
        program: Arc<Program>,
        state: RuntimeState<T>,
        result: EvaluationResult<T>,
        cache_entries: CacheEntries<T>,
    ) -> Self {
        let payload = EmitCommand {
            subscription_id,
            cache_key,
            program,
            state,
            result,
            cache_entries,
        };
        Self::Emit(payload)
    }
    fn update(updates: Vec<StateUpdate<T>>) -> Self {
        let payload = UpdateCommand { updates };
        Self::Update(payload)
    }
}

pub struct Runtime<T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>> {
    commands: RuntimeCommandChannel<T>,
    commands_handler: tokio::task::JoinHandle<()>,
    // Retain a reference to prevent broadcast channel closing immediately due to lack of listeners
    flush_keepalive: tokio::task::JoinHandle<()>,
}
impl<T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>> Drop
    for Runtime<T>
{
    fn drop(&mut self) {
        self.commands_handler.abort();
        self.flush_keepalive.abort();
    }
}
impl<T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>> Runtime<T> {
    pub fn new<THandler>(
        state: RuntimeState<T>,
        builtins: impl IntoIterator<Item = (Uuid, T)>,
        signal_handler: THandler,
        cache: RuntimeCache<T>,
        factory: &impl AsyncExpressionFactory<T>,
        allocator: &impl AsyncHeapAllocator<T>,
        interpreter_options: InterpreterOptions,
        compiler_options: CompilerOptions,
    ) -> Self
    where
        T::String: StringValue + Send + Sync,
        THandler: Fn(&str, &[&Signal<T>], &SignalHelpers<T>) -> SignalHandlerResult<T>
            + Send
            + Sync
            + 'static,
    {
        // TODO: Establish sensible defaults for channel buffer sizes
        let (commands_tx, mut commands_rx) = mpsc::channel(1024);
        let (flush_tx, flush_rx) = broadcast::channel(1024);
        let commands_handler = tokio::spawn({
            let builtins = Arc::new(builtins.into_iter().collect::<Vec<_>>());
            let mut store = Mutex::new(RuntimeStore::new(state));
            let factory = factory.clone();
            let allocator = allocator.clone();
            let commands = commands_tx.clone();
            let mut cache = cache;
            let empty_result = create_pending_result(&factory, &allocator);
            async move {
                while let Some(command) = commands_rx.recv().await {
                    let mut next_command = Some(command);
                    let mut pending_retains = Vec::new();
                    let mut pending_releases = Vec::new();
                    let mut pending_updates = None;
                    let mut pending_gc = false;
                    while let Some(command) = next_command {
                        match command {
                            RuntimeCommand::Subscribe(command) => {
                                let is_root = command.payload.initiators.is_none();
                                let (subscription_id, cache_key) = process_subscribe_command(
                                    command,
                                    &mut store,
                                    &mut cache,
                                    &commands,
                                    &flush_tx,
                                    &factory,
                                    &allocator,
                                    &builtins,
                                    &interpreter_options,
                                );
                                let subscription_cache_key = cache.register_subscription(
                                    subscription_id,
                                    cache_key,
                                    empty_result.clone(),
                                );
                                if is_root {
                                    pending_retains.push(subscription_cache_key);
                                }
                            }
                            RuntimeCommand::Unsubscribe(command) => {
                                let subscription_id = command.payload.subscription_id;
                                let result = process_unsubscribe_command(command, &mut store);
                                if let Some((cache_key, dispose)) = result {
                                    dispose();
                                    let subscription_cache_key = generate_subscription_cache_key(
                                        &subscription_id,
                                        &cache_key,
                                    );
                                    pending_releases.push(subscription_cache_key);
                                    pending_gc = true;
                                }
                            }
                            RuntimeCommand::Emit(command) => {
                                let updates = process_emit_command(
                                    command,
                                    &mut store,
                                    &mut cache,
                                    &commands,
                                    &signal_handler,
                                    &factory,
                                    &allocator,
                                    &compiler_options,
                                );
                                let (state, updated_keys) =
                                    store.get_mut().unwrap().update_state(updates);
                                match &mut pending_updates {
                                    None => {
                                        pending_updates = Some((state, updated_keys));
                                    }
                                    Some((pending_state, pending_updates)) => {
                                        *pending_state = state;
                                        pending_updates.extend(updated_keys)
                                    }
                                }
                                // TODO: GC when flush queue empty rather than after every emission
                                pending_gc = true
                            }
                            RuntimeCommand::Update(command) => {
                                let updates = command.updates;

                                let (state, updated_keys) =
                                    store.get_mut().unwrap().update_state(updates);
                                match &mut pending_updates {
                                    None => {
                                        pending_updates = Some((state, updated_keys));
                                    }
                                    Some((pending_state, pending_updates)) => {
                                        *pending_state = state;
                                        pending_updates.extend(updated_keys)
                                    }
                                }
                            }
                        }
                        next_command = next_queued_item(&mut commands_rx);
                    }
                    for cache_key in pending_retains {
                        cache.retain(cache_key);
                    }
                    for cache_key in pending_releases {
                        cache.release(cache_key);
                    }
                    if pending_gc {
                        gc(&mut store, &mut cache);
                    }
                    if let Some((state, updated_keys)) = pending_updates {
                        let _ = flush_tx.send(FlushPayload::new((state, updated_keys)));
                    }
                }
            }
        });
        Self {
            commands: commands_tx,
            commands_handler,
            flush_keepalive: tokio::spawn(drain_stream(BroadcastStream::new(flush_rx))),
        }
    }
    pub async fn subscribe<'a>(
        &'a self,
        program: Program,
        entry_point: InstructionPointer,
    ) -> Result<StreamSubscription<'a, T, impl Stream<Item = T>>, String> {
        create_subscription(&self.commands, program, entry_point, None).await
    }
    pub async fn unsubscribe(&self, subscription_id: SubscriptionId) -> Result<bool, String> {
        let (response_tx, response_rx) = oneshot::channel();
        let _ = self
            .commands
            .send(RuntimeCommand::unsubscribe(subscription_id, response_tx))
            .await;
        let result = response_rx.await;
        match result {
            Err(error) => Err(format!("{}", error)),
            Ok(result) => Ok(result),
        }
    }
}

async fn create_subscription<
    'a,
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
>(
    commands: &'a RuntimeCommandChannel<T>,
    program: Program,
    entry_point: InstructionPointer,
    initiators: Option<Vec<SignalId>>,
) -> Result<StreamSubscription<'a, T, impl Stream<Item = T>>, String> {
    let (response_tx, response_rx) = oneshot::channel();
    let (updates_tx, updates_rx) = watch::channel(None);
    let _ = commands
        .send(RuntimeCommand::subscribe(
            program,
            entry_point,
            initiators,
            response_tx,
            updates_tx,
        ))
        .await;
    let subscription_id = response_rx.await;
    match subscription_id {
        Err(error) => Err(format!("{}", error)),
        Ok(subscription_id) => Ok(StreamSubscription::new(
            commands,
            subscription_id,
            WatchStream::new(updates_rx).filter_map(identity),
        )),
    }
}

fn process_subscribe_command<T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T>>(
    command: StreamOperation<SubscribeCommand, SubscriptionId, T>,
    store: &mut Mutex<RuntimeStore<T>>,
    cache: &mut RuntimeCache<T>,
    commands: &RuntimeCommandChannel<T>,
    flush: &broadcast::Sender<FlushPayload<T>>,
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
    builtins: &Arc<Vec<(Uuid, T)>>,
    interpreter_options: &InterpreterOptions,
) -> (SubscriptionId, HashId)
where
    T::String: Send + Sync,
{
    let payload = command.payload;
    let target = payload.target;
    let entry_point = payload.entry_point;
    let initiators = payload.initiators;
    let cache_key = hash_program_root(&target, &entry_point);
    let (abort_handle, abort_registration) = AbortHandle::new_pair();
    let store = store.get_mut().unwrap();
    let subscription_id = store.subscribe(cache_key, initiators, move || abort_handle.abort());
    let _ = command.response.send(subscription_id);
    tokio::spawn(Abortable::new(
        {
            let flush = flush.clone();
            let factory = factory.clone();
            let allocator = allocator.clone();
            let builtins = builtins.clone();
            let interpreter_options = interpreter_options.clone();
            let cache = cache.clone();
            let commands = commands.clone();
            let updates = command.updates;
            // TODO: Avoid cloning program when sharing between interpreter and signal helpers
            let program = Arc::new(target.clone());
            let state = store.state.clone();
            async move {
                let mut flush_receiver = flush.subscribe();

                let (result, cache_entries) = evaluate_subscription(
                    subscription_id,
                    cache_key,
                    &program,
                    entry_point,
                    &state,
                    &factory,
                    &allocator,
                    &builtins,
                    &interpreter_options,
                    &cache,
                );

                let mut latest_result = Mutex::new(result.clone());

                if let Some(value) = parse_subscription_result(&result, &factory, &allocator) {
                    println!("[Runtime] Emit #{}", subscription_id);
                    let _ = updates.send(Some(value));
                }
                let _ = commands
                    .send(RuntimeCommand::emit(
                        subscription_id,
                        cache_key,
                        Arc::clone(&program),
                        state,
                        result,
                        cache_entries,
                    ))
                    .await;

                // TODO: Decrease subscription scheduling priority after initial result is emitted
                while let Ok(state) = flush_receiver.recv().await {
                    let (state, is_unchanged) = {
                        let (state, updates) = state.as_ref();
                        let mut latest_state = state.clone();
                        let mut accumulated_updates = HashSet::<StateToken>::new();
                        while let Ok(queued_state) = flush_receiver.try_recv() {
                            let (next_state, next_updates) = queued_state.as_ref();
                            latest_state = next_state.clone();
                            accumulated_updates.extend(next_updates);
                        }
                        let previous_result = latest_result.lock().unwrap();
                        let active_dependencies = previous_result.dependencies();
                        let is_unchanged = !updates
                            .iter()
                            .copied()
                            .chain(accumulated_updates)
                            .any(|key| active_dependencies.contains(key));
                        (latest_state, is_unchanged)
                    };
                    if is_unchanged {
                        continue;
                    }

                    let (result, cache_entries) = evaluate_subscription(
                        subscription_id,
                        cache_key,
                        &program,
                        entry_point,
                        &state,
                        &factory,
                        &allocator,
                        &builtins,
                        &interpreter_options,
                        &cache,
                    );

                    let previous_result =
                        std::mem::replace(latest_result.get_mut().unwrap(), result.clone());
                    if evaluation_results_are_equal(&previous_result, &result) {
                        continue;
                    }

                    let output_value = parse_subscription_result(&result, &factory, &allocator);
                    if let Some(value) = output_value {
                        println!("[Runtime] Emit #{}", subscription_id);
                        let _ = updates.send(Some(value));
                    }
                    let _ = commands
                        .send(RuntimeCommand::emit(
                            subscription_id,
                            cache_key,
                            Arc::clone(&program),
                            state,
                            result,
                            cache_entries,
                        ))
                        .await;
                }
            }
        },
        abort_registration,
    ));
    (subscription_id, cache_key)
}

fn evaluate_subscription<T: Expression + Rewritable<T> + Reducible<T> + Applicable<T>>(
    subscription_id: SubscriptionId,
    cache_key: HashId,
    program: &Program,
    entry_point: InstructionPointer,
    state: &impl DynamicState<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    builtins: &[(Uuid, T)],
    interpreter_options: &InterpreterOptions,
    cache: &impl InterpreterCache<T>,
) -> (EvaluationResult<T>, CacheEntries<T>) {
    println!("[Runtime] Evaluating subscription #{}", subscription_id);
    let start_time = Instant::now();
    let result = execute(
        cache_key,
        program,
        entry_point,
        state,
        factory,
        allocator,
        builtins,
        interpreter_options,
        cache,
    )
    .unwrap_or_else(|error| {
        (
            EvaluationResult::new(
                create_error_expression(
                    once(
                        factory
                            .create_value_term(ValueTerm::String(allocator.create_string(error))),
                    ),
                    factory,
                    allocator,
                ),
                DependencyList::empty(),
            ),
            CacheEntries::default(),
        )
    });
    println!(
        "[Runtime] Evaluated subscription #{} in {:?}",
        subscription_id,
        start_time.elapsed()
    );
    result
}

fn parse_subscription_result<T: Expression>(
    result: &EvaluationResult<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Option<T> {
    let (custom_signals, error_signals, is_pending) =
        match factory.match_signal_term(result.result()) {
            Some(signal) => {
                let (custom_signals, error_signals, is_pending) = signal.signals().iter().fold(
                    (Vec::new(), Vec::new(), false),
                    |(mut custom_signals, mut error_signals, mut is_pending), signal| {
                        match signal.signal_type() {
                            SignalType::Error => {
                                error_signals.push(signal);
                            }
                            SignalType::Pending => {
                                is_pending = true;
                            }
                            SignalType::Custom(_) => {
                                custom_signals.push(signal);
                                is_pending = true;
                            }
                        }
                        (custom_signals, error_signals, is_pending)
                    },
                );
                (custom_signals, error_signals, is_pending)
            }
            None => (Vec::new(), Vec::new(), false),
        };
    if !error_signals.is_empty() {
        if custom_signals.is_empty() && !is_pending {
            Some(result.result().clone())
        } else {
            Some(create_error_expression(
                error_signals
                    .into_iter()
                    .map(|signal| {
                        signal
                            .args()
                            .iter()
                            .next()
                            .cloned()
                            .unwrap_or_else(|| factory.create_value_term(ValueTerm::Null))
                    })
                    .collect::<Vec<_>>(),
                factory,
                allocator,
            ))
        }
    } else if is_pending {
        None
    } else {
        Some(result.result().clone())
    }
}

fn process_unsubscribe_command<T: Expression>(
    command: AsyncOperation<UnsubscribeCommand, bool>,
    store: &mut Mutex<RuntimeStore<T>>,
) -> Option<(HashId, impl FnOnce() + Send + Sync)> {
    let payload = command.payload;
    let subscription_id = payload.subscription_id;
    let subscription = store.get_mut().unwrap().unsubscribe(subscription_id);
    let result = if let Some(subscription) = subscription {
        Some((subscription.cache_key, subscription.dispose))
    } else {
        None
    };
    let _ = command.response.send(result.is_some());
    result
}

fn process_emit_command<
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    THandler,
>(
    payload: EmitCommand<T>,
    store: &mut Mutex<RuntimeStore<T>>,
    cache: &mut RuntimeCache<T>,
    commands: &RuntimeCommandChannel<T>,
    signal_handler: &THandler,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    compiler_options: &CompilerOptions,
) -> Vec<StateUpdate<T>>
where
    T::String: StringValue + Send + Sync,
    THandler: Fn(&str, &[&Signal<T>], &SignalHelpers<T>) -> SignalHandlerResult<T>
        + Send
        + Sync
        + 'static,
{
    let subscription_id = payload.subscription_id;
    let cache_key = payload.cache_key;
    let state = payload.state;
    let result = payload.result;
    let program = payload.program;
    let cache_entries = payload.cache_entries;
    cache.apply_updates(cache_entries);
    let store = store.get_mut().unwrap();
    let custom_signals = match factory.match_signal_term(result.result()) {
        Some(signal) => signal
            .signals()
            .iter()
            .filter(|signal| match signal.signal_type() {
                SignalType::Custom(_) => true,
                _ => false,
            })
            .collect::<Vec<_>>(),
        None => Vec::new(),
    };
    let handler_results = if custom_signals.is_empty() {
        None
    } else {
        let added_signals = custom_signals
            .into_iter()
            .filter(|signal| store.retrieve_signal_result(signal).is_none())
            .collect::<Vec<_>>();
        if added_signals.is_empty() {
            None
        } else {
            let (signal_results, effects) = handle_custom_signals(
                &added_signals,
                signal_handler,
                program,
                factory,
                allocator,
                compiler_options,
                commands,
            );
            store.register_signal_results(signal_results.iter().cloned());
            cache.apply_updates(signal_results.iter().cloned().map(|(signal_id, value)| {
                (
                    signal_id,
                    InterpreterCacheEntry::new(
                        EvaluationResult::new(value, DependencyList::empty()),
                        &state,
                    ),
                    Vec::new(),
                )
            }));
            Some((signal_results, effects))
        }
    };
    store.update_subscription_dependencies(subscription_id, result.dependencies());
    ensure_cache_entries(result.dependencies(), cache, &state, factory, allocator);
    cache.update_subscription_dependencies(subscription_id, cache_key, result.dependencies());
    let results = if let Some((signal_results, effects)) = handler_results {
        let (sync_updates, dispose_callbacks) = apply_effects(effects, commands);
        let outdated_dispose_callbacks = store.register_dispose_callbacks(dispose_callbacks);
        for dispose_callback in outdated_dispose_callbacks {
            tokio::spawn(dispose_callback);
        }
        signal_results
            .into_iter()
            .map(|(signal_id, value)| StateUpdate::value(signal_id, value))
            .chain(sync_updates)
            .collect()
    } else {
        Vec::new()
    };
    results
}

fn handle_custom_signals<
    'a,
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T> + 'a,
    THandler,
>(
    signals: &[&'a Signal<T>],
    signal_handler: &THandler,
    program: Arc<Program>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    compiler_options: &CompilerOptions,
    commands: &RuntimeCommandChannel<T>,
) -> (Vec<(SignalId, T)>, Vec<(SignalId, RuntimeEffect<T>)>)
where
    T::String: StringValue + Send + Sync,
    THandler: Fn(&str, &[&Signal<T>], &SignalHelpers<T>) -> SignalHandlerResult<T>
        + Send
        + Sync
        + 'static,
{
    let signal_helpers = SignalHelpers::new(program, commands.clone(), *compiler_options);
    let signals_by_type = signals
        .iter()
        .filter_map(|signal| match signal.signal_type() {
            SignalType::Custom(signal_type) => Some((signal, signal_type)),
            _ => None,
        })
        .fold(
            HashMap::<&str, Vec<&Signal<T>>>::new(),
            |mut results, (signal, signal_type)| {
                match results.entry(signal_type.as_str()) {
                    Entry::Occupied(mut entry) => {
                        entry.get_mut().push(signal);
                    }
                    Entry::Vacant(entry) => {
                        entry.insert(vec![signal]);
                    }
                }
                results
            },
        );
    let mut signal_results = HashMap::with_capacity(signals.len());
    for (signal_type, signals) in signals_by_type {
        let results = signal_handler(signal_type, &signals, &signal_helpers);
        if let Some(results) = results {
            let signal_ids = signals.iter().map(|signal| signal.id());
            if results.len() == signals.len() {
                signal_results.extend(signal_ids.zip(results));
            } else {
                signal_results.extend(signal_ids.map(|id| {
                    (
                        id,
                        Err(format!(
                            "Expected {} results from {} handler, received {}",
                            signals.len(),
                            signal_type,
                            results.len()
                        )),
                    )
                }));
            }
        }
    }
    let (signal_results, effects) = signals
        .iter()
        .map(|signal| {
            let signal_id = signal.id();
            let handler_result = match signal_results.remove(&signal.id()) {
                Some(result) => result,
                None => Err(format!("Unhandled signal: {}", signal)),
            };
            let (result, effect) =
                handler_result.unwrap_or_else(|error| {
                    (
                        create_error_expression(
                            once(factory.create_value_term(ValueTerm::String(
                                allocator.create_string(error),
                            ))),
                            factory,
                            allocator,
                        ),
                        None,
                    )
                });
            (signal_id, result, effect)
        })
        .fold(
            (Vec::with_capacity(signals.len()), Vec::new()),
            |(mut results, mut effects), (signal_id, result, effect)| {
                results.push((signal_id, result));
                if let Some(effect) = effect {
                    effects.push((signal_id, effect));
                }
                (results, effects)
            },
        );
    (signal_results, effects)
}

fn ensure_cache_entries<T: Expression>(
    dependencies: impl IntoIterator<Item = InterpreterCacheKey>,
    cache: &mut RuntimeCache<T>,
    state: &impl DynamicState<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) {
    let missing_dependencies = {
        let cache = cache.cache.read();
        dependencies
            .into_iter()
            .filter(|key| !cache.contains_key(key))
            .collect::<Vec<_>>()
    };
    if missing_dependencies.is_empty() {
        return;
    }
    let dummy_result = create_pending_result(factory, allocator);
    let mut cache = cache.cache.write();
    for key in missing_dependencies {
        cache.store_result(key, state, dummy_result.clone(), Vec::new());
    }
}

fn apply_effects<T: AsyncExpression>(
    effects: Vec<(SignalId, RuntimeEffect<T>)>,
    commands: &RuntimeCommandChannel<T>,
) -> (Vec<StateUpdate<T>>, Vec<(SignalId, DisposeCallback)>)
where
    T::String: StringValue + Send + Sync,
{
    effects
        .into_iter()
        .map(|(signal_id, effect)| {
            let (updates, dispose) = apply_effect(effect, commands);
            (updates, dispose.map(|dispose| (signal_id, dispose)))
        })
        .fold(
            (Vec::new(), Vec::new()),
            |(mut existing_sync_updates, mut existing_dispose_callbacks),
             (sync_updates, dispose_callback)| {
                existing_sync_updates.extend(sync_updates);
                existing_dispose_callbacks.extend(dispose_callback);
                (existing_sync_updates, existing_dispose_callbacks)
            },
        )
}

fn apply_effect<T: AsyncExpression>(
    effect: RuntimeEffect<T>,
    commands: &RuntimeCommandChannel<T>,
) -> (Vec<StateUpdate<T>>, Option<DisposeCallback>)
where
    T::String: StringValue + Send + Sync,
{
    let commands = commands.clone();
    match effect {
        RuntimeEffect::Sync(updates) => (updates, None),
        RuntimeEffect::Async((effect, dispose)) => {
            let (abort_handle, abort_registration) = AbortHandle::new_pair();
            tokio::spawn(Abortable::new(
                async move {
                    let updates = effect.await;
                    let _ = commands.send(RuntimeCommand::update(updates)).await;
                },
                abort_registration,
            ));
            (
                Vec::new(),
                Some(Box::pin(async move {
                    abort_handle.abort();
                    let _ = dispose.await;
                })),
            )
        }
        RuntimeEffect::Stream((mut effect, dispose)) => {
            let (abort_handle, abort_registration) = AbortHandle::new_pair();
            tokio::spawn(Abortable::new(
                async move {
                    while let Some(updates) = effect.next().await {
                        let _ = commands.send(RuntimeCommand::update(updates)).await;
                    }
                },
                abort_registration,
            ));
            (
                Vec::new(),
                Some(Box::pin(async move {
                    abort_handle.abort();
                    let _ = dispose.await;
                })),
            )
        }
    }
}

fn gc<T: Expression>(store: &mut Mutex<RuntimeStore<T>>, cache: &mut RuntimeCache<T>) {
    let start_time = Instant::now();
    println!("[Runtime] GC started");
    {
        print!("[Runtime] Purging cache entries...");
        let start_time = Instant::now();
        let metrics = cache.gc();
        println!("{:?} ({})", start_time.elapsed(), metrics);
    }
    {
        println!("[Runtime] Computing inactive subscriptions...");
        let start_time = Instant::now();
        let dispose_callbacks = store.get_mut().unwrap().gc();
        println!(
            "[Runtime] Inactive subscriptions computed in {:?}",
            start_time.elapsed()
        );
        if !dispose_callbacks.is_empty() {
            print!(
                "[Runtime] Cleaning up {} signal effects...",
                dispose_callbacks.len()
            );
            let start_time = Instant::now();
            for dispose_callback in dispose_callbacks {
                tokio::spawn(dispose_callback);
            }
            println!("{:?}", start_time.elapsed());
        }
    }
    println!("[Runtime] GC completed in {:?}", start_time.elapsed());
}

#[derive(Clone)]
pub struct SignalHelpers<T: AsyncExpression + Compile<T>>
where
    T::String: StringValue + Send + Sync,
{
    program: Arc<Program>,
    commands: RuntimeCommandChannel<T>,
    compiler_options: CompilerOptions,
}
impl<T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>>
    SignalHelpers<T>
where
    T::String: StringValue + Send + Sync,
{
    fn new(
        program: Arc<Program>,
        commands: RuntimeCommandChannel<T>,
        compiler_options: CompilerOptions,
    ) -> Self {
        Self {
            program,
            commands,
            compiler_options,
        }
    }
    pub fn program(&self) -> &Program {
        &self.program
    }
    pub fn watch_expression(
        self,
        expression: T,
        initiators: impl IntoIterator<Item = SignalId>,
        factory: &impl AsyncExpressionFactory<T>,
        allocator: &impl AsyncHeapAllocator<T>,
    ) -> Result<impl Stream<Item = T> + Send + 'static, String> {
        let initiators = initiators.into_iter().collect::<Vec<_>>();
        let entry_point = InstructionPointer::new(self.program.len());
        let prelude = (*self.program).clone();
        Compiler::new(self.compiler_options, Some(prelude))
            .compile(&expression, CompilerMode::Function, factory, allocator)
            .map(|program| {
                // TODO: Error if runtime expression depends on unrecognized native functions
                self.watch_compiled_expression(program, entry_point, initiators, factory, allocator)
            })
    }
    pub fn watch_state(
        self,
        state_token: StateToken,
        initiators: impl IntoIterator<Item = SignalId>,
        factory: &impl AsyncExpressionFactory<T>,
        allocator: &impl AsyncHeapAllocator<T>,
    ) -> impl Stream<Item = T> + Send + 'static {
        let initiators = initiators.into_iter().collect::<Vec<_>>();
        let program = compile_state_subscription_expression(state_token);
        self.watch_compiled_expression(
            program,
            InstructionPointer::default(),
            initiators,
            factory,
            allocator,
        )
    }
    fn watch_compiled_expression(
        self,
        program: Program,
        entry_point: InstructionPointer,
        initiators: Vec<SignalId>,
        factory: &impl AsyncExpressionFactory<T>,
        allocator: &impl AsyncHeapAllocator<T>,
    ) -> impl Stream<Item = T> + Send + 'static {
        futures::StreamExt::flatten(
            {
                let factory = factory.clone();
                let allocator = allocator.clone();
                async move {
                    match create_subscription(
                        &self.commands,
                        program,
                        entry_point,
                        Some(initiators),
                    )
                    .await
                    {
                        Err(error) => futures::StreamExt::left_stream(stream::iter(once(
                            create_error_expression(
                                once(factory.create_value_term(ValueTerm::String(
                                    allocator.create_string(error),
                                ))),
                                &factory,
                                &allocator,
                            ),
                        ))),
                        Ok(subscription) => {
                            futures::StreamExt::right_stream(subscription.into_stream())
                        }
                    }
                }
            }
            .into_stream(),
        )
    }
}

fn compile_state_subscription_expression(state_token: StateToken) -> Program {
    create_main_function([
        Instruction::PushSignal {
            signal_type: SignalType::Pending,
            num_args: 0,
        },
        Instruction::PushDynamic { state_token },
        Instruction::Evaluate,
        Instruction::Return,
    ])
}

#[derive(Clone)]
pub struct RuntimeCache<T: Expression> {
    cache: Arc<RwLock<DefaultInterpreterCache<T>>>,
}
impl<T: Expression> Default for RuntimeCache<T> {
    fn default() -> Self {
        Self {
            cache: Arc::new(RwLock::new(DefaultInterpreterCache::default())),
        }
    }
}
impl<T: Expression> InterpreterCache<T> for RuntimeCache<T> {
    fn retrieve_result(
        &self,
        key: &InterpreterCacheKey,
        state: &impl DynamicState<T>,
    ) -> Option<(EvaluationResult<T>, Option<InterpreterCacheEntry<T>>)> {
        self.cache.read().retrieve_result(key, state)
    }
    fn contains_key(&self, key: &InterpreterCacheKey) -> bool {
        self.cache.read().contains_key(key)
    }
}
impl<T: Expression> RuntimeCache<T> {
    pub fn new(
        entries: impl IntoIterator<
            Item = (
                InterpreterCacheKey,
                InterpreterCacheEntry<T>,
                Vec<InterpreterCacheKey>,
            ),
        >,
    ) -> Self {
        Self {
            cache: Arc::new(RwLock::new(DefaultInterpreterCache::new(entries))),
        }
    }

    pub fn register_subscription(
        &mut self,
        subscription_id: SubscriptionId,
        cache_key: InterpreterCacheKey,
        initial_result: EvaluationResult<T>,
    ) -> HashId {
        let subscription_cache_key = generate_subscription_cache_key(&subscription_id, &cache_key);
        let empty_state = StateCache::default();
        let mut cache = self.cache.write();
        if !cache.contains_key(&cache_key) {
            cache.store_result(cache_key, &empty_state, initial_result.clone(), Vec::new());
        }
        cache.store_result(
            subscription_cache_key,
            &empty_state,
            initial_result,
            vec![cache_key],
        );
        subscription_cache_key
    }
    pub fn update_subscription_dependencies(
        &mut self,
        subscription_id: SubscriptionId,
        cache_key: HashId,
        dependencies: &DependencyList,
    ) {
        let subscription_cache_key = generate_subscription_cache_key(&subscription_id, &cache_key);
        let mut cache = self.cache.write();
        let dependencies = once(cache_key)
            .chain(
                dependencies
                    .iter()
                    .filter(|state_token| cache.contains_key(state_token)),
            )
            .collect();
        cache.replace_children(&subscription_cache_key, dependencies);
    }
    pub fn retain(&mut self, key: InterpreterCacheKey) {
        self.cache.write().retain(key)
    }
    pub fn release(&mut self, key: InterpreterCacheKey) {
        self.cache.write().release(key)
    }
    pub fn gc(&mut self) -> GcMetrics {
        self.cache.write().gc()
    }
    pub fn apply_updates(
        &mut self,
        entries: impl IntoIterator<
            Item = (
                InterpreterCacheKey,
                InterpreterCacheEntry<T>,
                Vec<InterpreterCacheKey>,
            ),
        >,
    ) {
        self.cache.write().extend(entries)
    }
}

struct RuntimeStore<T: Expression> {
    state: RuntimeState<T>,
    subscriptions: Vec<Subscription>,
    subscription_counter: usize,
    subscription_cache: SubscriptionCache<T>,
    effect_cache: HashMap<SignalId, DisposeCallback>,
}
impl<T: Expression> RuntimeStore<T> {
    fn new(state: RuntimeState<T>) -> Self {
        Self {
            state,
            subscriptions: Vec::default(),
            subscription_counter: 0,
            subscription_cache: SubscriptionCache::default(),
            effect_cache: HashMap::default(),
        }
    }
    fn subscribe(
        &mut self,
        cache_key: HashId,
        initiators: Option<Vec<SignalId>>,
        dispose: impl FnOnce() -> () + Send + Sync + 'static,
    ) -> SubscriptionId {
        self.subscription_counter += 1;
        let subscription_id = self.subscription_counter;
        println!("[Runtime] Subscribe #{}", subscription_id);
        let is_root = initiators.is_none();
        self.subscriptions.push(Subscription::new(
            subscription_id,
            cache_key,
            is_root,
            dispose,
        ));
        self.subscription_cache
            .register_subscription(subscription_id, initiators.unwrap_or(Vec::new()));
        if is_root {
            self.subscription_cache.retain_subscription(subscription_id);
        }
        subscription_id
    }
    fn unsubscribe(&mut self, subscription_id: SubscriptionId) -> Option<Subscription> {
        self.subscriptions
            .iter()
            .position(|subscription| subscription.id == subscription_id)
            .map(|index| {
                println!("[Runtime] Unsubscribe #{}", subscription_id);
                let subscription = self.subscriptions.remove(index);
                if subscription.is_root {
                    self.subscription_cache
                        .release_subscription(subscription_id);
                }
                subscription
            })
    }
    fn update_subscription_dependencies(
        &mut self,
        subscription_id: SubscriptionId,
        dependencies: &DependencyList,
    ) -> bool {
        let subscription = self
            .subscriptions
            .iter()
            .find(|subscription| subscription.id == subscription_id);
        if let Some(_) = subscription {
            self.subscription_cache
                .update_subscription_dependencies(subscription_id, dependencies);
            true
        } else {
            false
        }
    }
    fn update_state(
        &mut self,
        updates: impl IntoIterator<Item = StateUpdate<T>>,
    ) -> (RuntimeState<T>, Vec<StateToken>) {
        let updates = updates
            .into_iter()
            .filter_map(|update| {
                let key = update.state_token;
                let existing_value = self.state.get(&key);
                let value = match update.update {
                    StateUpdateType::Value(value) => value,
                    StateUpdateType::Patch(updater) => updater(existing_value),
                };
                match existing_value {
                    Some(existing) if existing.id() == value.id() => None,
                    _ => Some((key, value)),
                }
            })
            .collect::<Vec<_>>();
        let updated_keys = updates.iter().map(|(key, _)| *key).collect();
        self.state.extend(updates);
        (self.state.clone(), updated_keys)
    }
    fn retrieve_signal_result(&self, signal: &Signal<T>) -> Option<&T> {
        self.subscription_cache.retrieve_signal_result(signal.id())
    }
    fn register_signal_results(&mut self, results: impl IntoIterator<Item = (SignalId, T)>) {
        for (signal_id, result) in results {
            self.subscription_cache.register_signal(signal_id, result);
        }
    }
    fn register_dispose_callbacks(
        &mut self,
        callbacks: impl IntoIterator<Item = (SignalId, DisposeCallback)>,
    ) -> impl IntoIterator<Item = DisposeCallback> {
        callbacks
            .into_iter()
            .filter_map(|(signal_id, callback)| self.effect_cache.insert(signal_id, callback))
            .collect::<Vec<_>>()
    }
    fn gc(&mut self) -> Vec<DisposeCallback> {
        let mut dispose_callbacks = Vec::new();
        loop {
            let (disposed_signal_ids, disposed_subscriptions): (Vec<_>, Vec<_>) =
                self.subscription_cache.gc();
            if !disposed_signal_ids.is_empty() {
                println!(
                    "[Runtime] Disposing {} inactive signals",
                    disposed_signal_ids.len()
                );
                for signal_id in disposed_signal_ids {
                    self.state.remove(&signal_id);
                    if let Some(dispose_callback) = self.effect_cache.remove(&signal_id) {
                        dispose_callbacks.push(dispose_callback)
                    }
                }
            }
            if !disposed_subscriptions.is_empty() {
                println!(
                    "[Runtime] Disposing {} inactive subscriptions",
                    disposed_subscriptions.len()
                );
                for subscription_id in disposed_subscriptions {
                    self.unsubscribe(subscription_id);
                }
                continue;
            }
            break;
        }
        dispose_callbacks
    }
}
struct Subscription {
    id: SubscriptionId,
    cache_key: HashId,
    is_root: bool,
    dispose: Box<dyn FnOnce() + Send + Sync>,
}
impl Subscription {
    fn new(
        id: SubscriptionId,
        cache_key: HashId,
        is_root: bool,
        dispose: impl FnOnce() + Send + Sync + 'static,
    ) -> Self {
        Self {
            id,
            cache_key,
            is_root,
            dispose: Box::new(dispose),
        }
    }
}

struct SubscriptionCache<T: Expression> {
    cache: DependencyCache<SignalCacheKey, SignalCacheEntry<T>>,
}
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum SignalCacheKey {
    Subscription(SubscriptionId),
    Signal(SignalId),
}
enum SignalCacheEntry<T: Expression> {
    Subscription(Option<HashId>),
    Signal(T),
}
impl<T: Expression> Default for SubscriptionCache<T> {
    fn default() -> Self {
        Self {
            cache: DependencyCache::default(),
        }
    }
}
impl<T: Expression> SubscriptionCache<T> {
    fn register_subscription(
        &mut self,
        subscription_id: SubscriptionId,
        initiators: impl IntoIterator<Item = SignalId>,
    ) {
        let key = SignalCacheKey::Subscription(subscription_id);
        let value = SignalCacheEntry::Subscription(None);
        self.cache.set(key, value, Vec::new());
        for signal_id in initiators {
            self.cache
                .add_child(&SignalCacheKey::Signal(signal_id), &key);
        }
    }
    fn update_subscription_dependencies(
        &mut self,
        subscription_id: SubscriptionId,
        dependencies: &DependencyList,
    ) -> bool {
        let key = SignalCacheKey::Subscription(subscription_id);
        let dependencies_hash = hash_object(dependencies);
        match self.cache.replace_value(
            &key,
            SignalCacheEntry::Subscription(Some(dependencies_hash)),
        ) {
            Some(SignalCacheEntry::Subscription(previous_dependencies_hash))
                if previous_dependencies_hash
                    .map(|previous_hash| previous_hash != dependencies_hash)
                    .unwrap_or(true) =>
            {
                self.cache.replace_children(
                    &key,
                    dependencies
                        .iter()
                        .map(|signal_id| SignalCacheKey::Signal(signal_id))
                        // FIXME: Filter out non-signal dependencies before updating dependency tree
                        .filter(|key| self.cache.contains_key(key))
                        .collect(),
                )
            }
            _ => false,
        }
    }
    fn retain_subscription(&mut self, subscription_id: SubscriptionId) {
        let key = SignalCacheKey::Subscription(subscription_id);
        self.cache.retain(once(&key));
    }
    fn release_subscription(&mut self, subscription_id: SubscriptionId) {
        let key = SignalCacheKey::Subscription(subscription_id);
        self.cache.release(once(&key));
    }
    fn register_signal(&mut self, signal_id: SignalId, result: T) {
        self.cache.set(
            SignalCacheKey::Signal(signal_id),
            SignalCacheEntry::Signal(result),
            Vec::new(),
        )
    }
    fn retrieve_signal_result(&self, signal_id: SignalId) -> Option<&T> {
        self.cache
            .get(&SignalCacheKey::Signal(signal_id))
            .and_then(|entry| match entry {
                SignalCacheEntry::Signal(result) => Some(result),
                _ => None,
            })
    }
    fn gc(&mut self) -> (Vec<SignalId>, Vec<SubscriptionId>) {
        self.cache
            .gc::<Vec<_>>()
            .into_iter()
            .partition_map(|(key, _)| match key {
                SignalCacheKey::Signal(signal_id) => Either::Left(signal_id),
                SignalCacheKey::Subscription(subscription_id) => Either::Right(subscription_id),
            })
    }
}

fn generate_subscription_cache_key(subscription_id: &SubscriptionId, cache_key: &HashId) -> HashId {
    let mut hasher = DefaultHasher::new();
    subscription_id.hash(&mut hasher);
    cache_key.hash(&mut hasher);
    hasher.finish()
}

fn create_error_expression<T: Expression>(
    errors: impl IntoIterator<Item = T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_signal_term(
        allocator.create_signal_list(errors.into_iter().map(|error| {
            allocator.create_signal(SignalType::Error, allocator.create_unit_list(error))
        })),
    )
}

fn create_pending_result<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> EvaluationResult<T> {
    EvaluationResult::new(
        create_pending_expression(factory, allocator),
        DependencyList::empty(),
    )
}

pub fn create_pending_expression<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_signal_term(allocator.create_signal_list(once(
        allocator.create_signal(SignalType::Pending, allocator.create_empty_list()),
    )))
}

fn evaluation_results_are_equal<T: Expression>(
    value1: &EvaluationResult<T>,
    value2: &EvaluationResult<T>,
) -> bool {
    value1.result().id() == value2.result().id() && value1.dependencies().eq(value2.dependencies())
}

fn next_queued_item<T>(target: &mut tokio::sync::mpsc::Receiver<T>) -> Option<T> {
    match target.poll_recv(&mut Context::from_waker(&futures::task::noop_waker())) {
        Poll::Ready(Some(value)) => Some(value),
        _ => None,
    }
}

fn drain_stream<T>(stream: impl Stream<Item = T>) -> impl Future<Output = ()> {
    futures::StreamExt::for_each(stream, |_| futures::future::ready(()))
}
