// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use futures::FutureExt;
use reflex::{
    compiler::{
        Compile, Compiler, CompilerMode, CompilerOptions, Instruction, InstructionPointer,
        NativeFunctionRegistry, Program,
    },
    core::{
        Applicable, Arity, Expression, ExpressionFactory, ExpressionList, HeapAllocator, Reducible,
        Rewritable, Signal, SignalId, SignalType, StateToken, StringValue,
    },
    hash::{hash_object, HashId},
    interpreter::{DefaultInterpreterCache, InterpreterCache, InterpreterOptions},
    lang::{term::NativeFunctionId, BuiltinTerm, ValueTerm, WithCompiledBuiltins},
};
use std::{
    any::TypeId,
    collections::{hash_map::Entry, HashMap},
    convert::identity,
    future::Future,
    iter::{empty, once},
    pin::Pin,
    sync::{Arc, Mutex},
    task::{Context, Poll},
};
use tokio::{
    pin,
    sync::{broadcast, mpsc, oneshot, watch},
};
use tokio_stream::wrappers::{BroadcastStream, WatchStream};
pub use tokio_stream::{Stream, StreamExt};

mod store;
use store::{Store, SubscriptionToken};

pub type SubscriptionResult<T> = Result<T, Vec<String>>;
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
#[derive(Clone)]
pub struct SignalHelpers<T: AsyncExpression + Compile<T>>
where
    T::String: StringValue + Send + Sync,
{
    program: Arc<Program>,
    builtins: Vec<(BuiltinTerm, InstructionPointer)>,
    plugins: Vec<(NativeFunctionId, Arity, InstructionPointer)>,
    commands: CommandChannel<T>,
    compiler_options: CompilerOptions,
}
impl<T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>>
    SignalHelpers<T>
where
    T::String: StringValue + Send + Sync,
{
    fn new(
        program: Arc<Program>,
        builtins: &[(BuiltinTerm, InstructionPointer)],
        plugins: &NativeFunctionRegistry<T>,
        commands: CommandChannel<T>,
        compiler_options: CompilerOptions,
    ) -> Self {
        Self {
            program,
            builtins: builtins.iter().copied().collect(),
            plugins: plugins
                .iter()
                .map(|(target, address)| (target.uid(), target.arity(), *address))
                .collect(),
            commands,
            compiler_options,
        }
    }
    pub fn program(&self) -> &Program {
        &self.program
    }
    pub fn builtins(&self) -> &[(BuiltinTerm, InstructionPointer)] {
        &self.builtins
    }
    pub fn plugins(&self) -> &[(NativeFunctionId, Arity, InstructionPointer)] {
        &self.plugins
    }
    pub fn watch_expression(
        self,
        expression: T,
        initiators: impl IntoIterator<Item = SignalId>,
        factory: &(impl AsyncExpressionFactory<T> + WithCompiledBuiltins),
        allocator: &impl AsyncHeapAllocator<T>,
    ) -> Result<impl Stream<Item = SubscriptionResult<T>> + Send + 'static, String> {
        let initiators = initiators.into_iter().collect::<Vec<_>>();
        let entry_point = InstructionPointer::new(self.program.len());
        let prelude = (*self.program).clone();
        Compiler::new(self.compiler_options, Some(prelude))
            .compile(
                &expression,
                CompilerMode::Expression,
                false,
                empty(),
                factory,
                allocator,
            )
            .map(|compiled| {
                // TODO: Error if runtime expression depends on unrecognized native functions
                let (program, _, _) = compiled.into_parts();
                self.watch_compiled_expression(program, entry_point, initiators, factory, allocator)
            })
    }
    pub fn watch_state(
        self,
        state_token: StateToken,
        initiators: impl IntoIterator<Item = SignalId>,
        factory: &(impl ExpressionFactory<T> + Send + Sync + Clone + 'static),
        allocator: &(impl HeapAllocator<T> + Send + Sync + Clone + 'static),
    ) -> impl Stream<Item = SubscriptionResult<T>> + Send + 'static {
        let initiators = initiators.into_iter().collect::<Vec<_>>();
        let program = Program::new(vec![
            Instruction::PushHash { value: Void::uid() },
            Instruction::PushDynamic { state_token },
            Instruction::Evaluate,
            Instruction::Return,
        ]);
        self.watch_compiled_expression(
            program,
            InstructionPointer::default(),
            initiators,
            factory,
            allocator,
        )
        .filter({
            let factory = factory.clone();
            move |value| match value {
                Ok(value) if Void::is(value, &factory) => false,
                _ => true,
            }
        })
    }
    fn watch_compiled_expression(
        self,
        program: Program,
        entry_point: InstructionPointer,
        initiators: Vec<SignalId>,
        factory: &impl AsyncExpressionFactory<T>,
        allocator: &impl AsyncHeapAllocator<T>,
    ) -> impl Stream<Item = SubscriptionResult<T>> + Send + 'static {
        let (results_tx, results_rx) = watch::channel::<Option<Result<T, Vec<String>>>>(None);
        // TODO: Unsubscribe signal handler watch expressions
        tokio::spawn({
            let factory = factory.clone();
            let allocator = allocator.clone();
            async move {
                match create_subscription(
                    program,
                    entry_point,
                    Some(initiators),
                    &self.commands,
                    &factory,
                    &allocator,
                )
                .await
                {
                    Err(error) => {
                        let _ = results_tx.send(Some(Err(vec![error])));
                    }
                    Ok(mut subscription) => {
                        while let Some(result) = subscription.next().await {
                            let _ = results_tx.send(Some(result));
                        }
                    }
                }
            }
        });
        WatchStream::new(results_rx).filter_map(identity)
    }
}

struct Void {}
impl Void {
    fn uid() -> HashId {
        hash_object(&TypeId::of::<Self>())
    }
    fn is<T: Expression>(value: &T, factory: &impl ExpressionFactory<T>) -> bool {
        match factory.match_value_term(value) {
            Some(ValueTerm::Hash(hash)) if *hash == Self::uid() => true,
            _ => false,
        }
    }
}

struct Operation<TRequest, TResponse> {
    payload: TRequest,
    response: oneshot::Sender<TResponse>,
}
impl<TRequest, TResponse> Operation<TRequest, TResponse> {
    fn new(payload: TRequest, response: oneshot::Sender<TResponse>) -> Self {
        Self { payload, response }
    }
}

struct StreamOperation<TRequest, TResponse, TUpdate> {
    payload: TRequest,
    response: oneshot::Sender<TResponse>,
    update: watch::Sender<TUpdate>,
}
impl<TRequest, TResponse, TUpdate> StreamOperation<TRequest, TResponse, TUpdate> {
    fn new(
        payload: TRequest,
        response: oneshot::Sender<TResponse>,
        update: watch::Sender<TUpdate>,
    ) -> Self {
        Self {
            payload,
            response,
            update,
        }
    }
}

pub type SubscriptionId = usize;

type CommandChannel<T> = mpsc::Sender<Command<T>>;
type ResultsChannel<T> = broadcast::Sender<(SubscriptionId, SubscriptionResult<T>)>;

type SubscribeCommand<T> = StreamOperation<
    (Program, InstructionPointer, Option<Vec<SignalId>>),
    SubscriptionId,
    Option<SubscriptionResult<T>>,
>;
type UnsubscribeCommand = Operation<SubscriptionId, bool>;
type UpdateCommand<T> = Operation<Vec<StateUpdate<T>>, ()>;

pub struct StateUpdate<T: Expression> {
    state_token: StateToken,
    update: StateUpdateType<T>,
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

enum Command<T: Expression> {
    Subscribe(SubscribeCommand<T>),
    Unsubscribe(UnsubscribeCommand),
    Update(UpdateCommand<T>),
}
impl<T: Expression> Command<T> {
    fn subscribe(
        program: Program,
        entry_point: InstructionPointer,
        initiators: Option<Vec<SignalId>>,
        response: oneshot::Sender<SubscriptionId>,
        update: watch::Sender<Option<SubscriptionResult<T>>>,
    ) -> Self {
        Self::Subscribe(StreamOperation::new(
            (program, entry_point, initiators),
            response,
            update,
        ))
    }
    fn unsubscribe(id: SubscriptionId, response: oneshot::Sender<bool>) -> Self {
        Self::Unsubscribe(Operation::new(id, response))
    }
    fn update(updates: Vec<StateUpdate<T>>, response: oneshot::Sender<()>) -> Self {
        Self::Update(Operation::new(updates, response))
    }
}
pub struct StreamSubscription<'a, T: Expression, TUpdates: Stream<Item = SubscriptionResult<T>>> {
    channel: &'a CommandChannel<T>,
    id: SubscriptionId,
    updates: TUpdates,
}
impl<'a, T: Expression, TUpdates: Stream<Item = SubscriptionResult<T>>>
    StreamSubscription<'a, T, TUpdates>
{
    fn new(channel: &'a CommandChannel<T>, id: SubscriptionId, updates: TUpdates) -> Self {
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
        let _ = self.channel.send(Command::unsubscribe(self.id, send)).await;
        match receive.await {
            Err(error) => Err(format!("{}", error)),
            Ok(result) => Ok(result),
        }
    }
}
impl<'a, T: Expression, TUpdates: Stream<Item = SubscriptionResult<T>> + Unpin> Stream
    for StreamSubscription<'a, T, TUpdates>
{
    type Item = SubscriptionResult<T>;

    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let updates = &mut self.updates;
        pin!(updates);
        Stream::poll_next(updates, cx)
    }
}

pub struct Runtime<T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>> {
    program: Arc<Program>,
    commands: CommandChannel<T>,
    results: ResultsChannel<T>,
}
impl<T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>> Runtime<T>
where
    T::String: StringValue + Send + Sync,
{
    pub fn new<THandler>(
        root: Program,
        signal_handler: THandler,
        factory: &(impl AsyncExpressionFactory<T> + WithCompiledBuiltins),
        allocator: &impl AsyncHeapAllocator<T>,
        builtins: Vec<(BuiltinTerm, InstructionPointer)>,
        plugins: NativeFunctionRegistry<T>,
        compiler_options: CompilerOptions,
        interpreter_options: InterpreterOptions,
        command_buffer_size: usize,
        result_buffer_size: usize,
    ) -> Self
    where
        THandler: Fn(&str, &[&Signal<T>], &SignalHelpers<T>) -> SignalHandlerResult<T>
            + Send
            + Sync
            + 'static,
    {
        let program = Arc::new(root);
        let (commands, results) = create_store(
            Arc::clone(&program),
            signal_handler,
            factory,
            allocator,
            builtins,
            plugins,
            interpreter_options,
            compiler_options,
            command_buffer_size,
            result_buffer_size,
        );
        Self {
            program,
            commands,
            results,
        }
    }
    pub fn program(&self) -> &Program {
        &self.program
    }
    pub async fn subscribe<'a>(
        &'a self,
        program: Program,
        entry_point: InstructionPointer,
        factory: &(impl ExpressionFactory<T> + Send + Clone + 'static),
        allocator: &(impl HeapAllocator<T> + Send + Clone + 'static),
    ) -> Result<StreamSubscription<'a, T, impl Stream<Item = SubscriptionResult<T>>>, String> {
        create_subscription(
            program,
            entry_point,
            None,
            &self.commands,
            factory,
            allocator,
        )
        .await
    }
    pub async fn unsubscribe(&self, id: SubscriptionId) -> Result<bool, String> {
        stop_subscription(id, &self.commands).await
    }
    pub fn watch_subscription(
        &self,
        subscription_id: SubscriptionId,
        factory: &(impl ExpressionFactory<T> + Send + Clone + 'static),
        allocator: &(impl HeapAllocator<T> + Send + Clone + 'static),
    ) -> impl Stream<Item = SubscriptionResult<T>> {
        BroadcastStream::new(self.results.subscribe()).filter_map({
            let factory = factory.clone();
            let allocator = allocator.clone();
            move |payload| match payload {
                Ok((id, result)) if id == subscription_id => match result {
                    Ok(expression) => {
                        match filter_pending_signals(expression, &factory, &allocator) {
                            Some(expression) => Some(Ok(expression)),
                            None => None,
                        }
                    }
                    Err(error) => Some(Err(error)),
                },
                _ => None,
            }
        })
    }
}

struct SharedKeyValueStore<K: Eq + std::hash::Hash, V> {
    commands: mpsc::Sender<SharedKeyValueStoreCommand<K, V>>,
}
impl<K: Eq + std::hash::Hash, V> Clone for SharedKeyValueStore<K, V> {
    fn clone(&self) -> Self {
        Self {
            commands: self.commands.clone(),
        }
    }
}
enum SharedKeyValueStoreCommand<K, V> {
    Insert(Operation<(K, V), Option<V>>),
    Remove(Operation<K, Option<V>>),
}
impl<K: Eq + std::hash::Hash + Send + 'static, V: Send + 'static> SharedKeyValueStore<K, V> {
    fn new() -> Self {
        let (commands_tx, mut commands_rx) =
            mpsc::channel::<SharedKeyValueStoreCommand<K, V>>(1024);
        tokio::spawn(async move {
            let mut cache = Mutex::new(HashMap::<K, V>::new());
            while let Some(command) = commands_rx.recv().await {
                match command {
                    SharedKeyValueStoreCommand::Insert(command) => {
                        let (key, value) = command.payload;
                        let existing_value = cache.get_mut().unwrap().insert(key, value);
                        let _ = command.response.send(existing_value);
                    }
                    SharedKeyValueStoreCommand::Remove(command) => {
                        let key = command.payload;
                        let value = cache.get_mut().unwrap().remove(&key);
                        let _ = command.response.send(value);
                    }
                }
            }
        });
        Self {
            commands: commands_tx,
        }
    }
    async fn insert(&self, key: K, value: V) -> Result<Option<V>, String> {
        let (response_tx, response_rx) = oneshot::channel();
        let _ = self
            .commands
            .send(SharedKeyValueStoreCommand::Insert(Operation::new(
                (key, value),
                response_tx,
            )))
            .await
            .map_err(|error| format!("{}", error))?;
        response_rx.await.map_err(|error| format!("{}", error))
    }
    async fn remove(&self, key: K) -> Result<Option<V>, String> {
        let (response_tx, response_rx) = oneshot::channel();
        let _ = self
            .commands
            .send(SharedKeyValueStoreCommand::Remove(Operation::new(
                key,
                response_tx,
            )))
            .await
            .map_err(|error| format!("{}", error))?;
        response_rx.await.map_err(|error| format!("{}", error))
    }
}

fn create_store<
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    THandler,
>(
    program: Arc<Program>,
    signal_handler: THandler,
    factory: &(impl AsyncExpressionFactory<T> + WithCompiledBuiltins),
    allocator: &impl AsyncHeapAllocator<T>,
    builtins: Vec<(BuiltinTerm, InstructionPointer)>,
    plugins: NativeFunctionRegistry<T>,
    interpreter_options: InterpreterOptions,
    compiler_options: CompilerOptions,
    command_buffer_size: usize,
    result_buffer_size: usize,
) -> (CommandChannel<T>, ResultsChannel<T>)
where
    T::String: StringValue + Send + Sync,
    THandler: Fn(&str, &[&Signal<T>], &SignalHelpers<T>) -> SignalHandlerResult<T>
        + Send
        + Sync
        + 'static,
{
    let (commands_tx, mut commands_rx) = mpsc::channel::<Command<T>>(command_buffer_size);
    let (results_tx, _) = broadcast::channel(result_buffer_size);
    tokio::spawn({
        let signal_helpers = SignalHelpers::new(
            program,
            &builtins,
            &plugins,
            commands_tx.clone(),
            compiler_options,
        );
        let dispose_cache = SharedKeyValueStore::<SignalId, DisposeCallback>::new();
        let results = results_tx.clone();
        let commands_tx = commands_tx.clone();
        let factory = factory.clone();
        let allocator = allocator.clone();
        async move {
            let cache = DefaultInterpreterCache::default();
            let mut store = Mutex::new(Store::new(
                cache,
                None,
                builtins,
                plugins,
                interpreter_options,
            ));
            while let Some(command) = commands_rx.recv().await {
                let mut next_command = Some(command);
                while let Some(command) = next_command {
                    match command {
                        Command::Subscribe(command) => {
                            process_subscribe_command(command, &mut store, &results)
                        }
                        Command::Unsubscribe(command) => {
                            process_unsubscribe_command(command, &mut store, &results)
                        }
                        Command::Update(command) => {
                            process_update_command(command, &mut store, &results)
                        }
                    }
                    next_command = next_queued_item(&mut commands_rx);
                }
                let updated_results = store.get_mut().unwrap().flush(
                {
                    |signals| {
                        let signal_results = handle_signals(
                            signals,
                            &commands_tx,
                            &signal_handler,
                            &signal_helpers,
                            &factory,
                        );
                        let (results, disposables): (Vec<_>, Vec<_>) = signals
                            .iter()
                            .map(|signal| signal.id())
                            .zip(signal_results.into_iter())
                            .map(|(signal_id, result)| match result {
                                Ok((result, dispose)) => {
                                    (Ok(result), dispose.map(|dispose| (signal_id, dispose)))
                                }
                                Err(error) => (Err(error), None),
                            })
                            .unzip();
                        let disposables = disposables.into_iter().filter_map(identity);
                        for (signal_id, dispose) in disposables {
                            tokio::spawn({
                                let dispose_cache = dispose_cache.clone();
                                async move { dispose_cache.insert(signal_id, dispose).await }
                            });
                        }
                        results
                    }
                },
                &factory,
                &allocator,
            )
                .into_iter()
                .filter_map(|(id, result)| match strip_pending_results(result) {
                    None => None,
                    Some(result) => Some((id, result)),
                });
                for update in updated_results {
                    let _ = results.send(update);
                }
                let disposed_signal_ids = gc(&mut store);
                if !disposed_signal_ids.is_empty() {
                    println!(
                        "[Runtime] Disposing {} inactive signals",
                        disposed_signal_ids.len()
                    );
                    for signal_id in disposed_signal_ids {
                        tokio::spawn({
                            let dispose_cache = dispose_cache.clone();
                            async move {
                                if let Some(dispose) =
                                    dispose_cache.remove(signal_id).await.unwrap()
                                {
                                    let _ = dispose.await;
                                }
                            }
                        });
                    }
                }
            }
        }
    });
    (commands_tx, results_tx)
}

fn gc<T: Expression + Rewritable<T> + Reducible<T> + Applicable<T>>(
    store: &mut Mutex<Store<T, DefaultInterpreterCache<T>>>,
) -> Vec<SignalId> {
    let mut combined_signal_ids = Vec::new();
    loop {
        let (disposed_signal_ids, disposed_subscriptions): (Vec<_>, Vec<_>) =
            store.get_mut().unwrap().gc();
        combined_signal_ids.extend(disposed_signal_ids);
        if disposed_subscriptions.is_empty() {
            break;
        }
        println!(
            "[Runtime] Disposing {} inactive subscriptions",
            disposed_subscriptions.len()
        );
        let store = store.get_mut().unwrap();
        for subscription_id in disposed_subscriptions {
            store.unsubscribe(subscription_id);
        }
    }
    combined_signal_ids
}

fn process_subscribe_command<
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T>,
    TCache: InterpreterCache<T>,
>(
    command: SubscribeCommand<T>,
    store: &mut Mutex<Store<T, TCache>>,
    results_tx: &ResultsChannel<T>,
) where
    T::String: StringValue + Send + Sync,
{
    let (program, entry_point, initiators) = command.payload;
    let subscription_id = store
        .get_mut()
        .unwrap()
        .subscribe(program, entry_point, initiators);
    let _ = command.response.send(subscription_id);

    let mut results_stream = get_subscription_results_stream(results_tx, subscription_id);

    let response_stream = command.update;
    tokio::spawn(async move {
        while let Some(update) = results_stream.next().await {
            let _ = response_stream.send(Some(update));
        }
    });
}

fn process_unsubscribe_command<
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T>,
    TCache: InterpreterCache<T>,
>(
    command: UnsubscribeCommand,
    store: &mut Mutex<Store<T, TCache>>,
    _results_tx: &ResultsChannel<T>,
) where
    T::String: StringValue + Send + Sync,
{
    let id = command.payload;
    let result = store.get_mut().unwrap().unsubscribe(id);
    let _ = command.response.send(result);
}

fn process_update_command<
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T>,
    TCache: InterpreterCache<T>,
>(
    command: UpdateCommand<T>,
    store: &mut Mutex<Store<T, TCache>>,
    _results_tx: &ResultsChannel<T>,
) where
    T::String: StringValue + Send + Sync,
{
    let updates = command.payload;
    store.get_mut().unwrap().update(updates);
    let _ = command.response.send(());
}

async fn create_subscription<'a, T: AsyncExpression>(
    program: Program,
    entry_point: InstructionPointer,
    initiators: Option<Vec<SignalId>>,
    commands: &'a CommandChannel<T>,
    factory: &(impl ExpressionFactory<T> + Send + Clone + 'static),
    allocator: &(impl HeapAllocator<T> + Send + Clone + 'static),
) -> Result<StreamSubscription<'a, T, impl Stream<Item = SubscriptionResult<T>>>, String>
where
    T::String: StringValue + Send + Sync,
{
    let (send, receive) = oneshot::channel();
    let (update_send, update_receive) = watch::channel(None);
    let command = Command::subscribe(program, entry_point, initiators, send, update_send);
    let _ = commands.send(command).await;
    let factory = factory.clone();
    let allocator = allocator.clone();
    match receive.await {
        Err(error) => Err(format!("{}", error)),
        Ok(subscription_id) => Ok(StreamSubscription::new(
            commands,
            subscription_id,
            WatchStream::new(update_receive)
                .filter_map(|result| result)
                .filter_map(move |result| match result {
                    Ok(expression) => {
                        match filter_pending_signals(expression, &factory, &allocator) {
                            Some(expression) => Some(Ok(expression)),
                            None => None,
                        }
                    }
                    Err(error) => Some(Err(error)),
                }),
        )),
    }
}

fn get_subscription_results_stream<T: AsyncExpression>(
    results_tx: &ResultsChannel<T>,
    subscription_id: SubscriptionToken,
) -> impl Stream<Item = SubscriptionResult<T>>
where
    T::String: StringValue + Send + Sync,
{
    // TODO: Investigate per-subscription results channels instead of shared global results channel
    BroadcastStream::new(results_tx.subscribe()).filter_map(move |payload| match payload {
        Ok((id, result)) if id == subscription_id => Some(result),
        _ => None,
    })
}

async fn stop_subscription<T: Expression>(
    id: SubscriptionId,
    channel: &CommandChannel<T>,
) -> Result<bool, String> {
    let (send, receive) = oneshot::channel();
    let _ = channel.send(Command::unsubscribe(id, send)).await;
    match receive.await {
        Err(error) => Err(format!("{}", error)),
        Ok(response) => Ok(response),
    }
}

fn handle_signals<
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    THandler,
>(
    signals: &[&Signal<T>],
    commands: &CommandChannel<T>,
    signal_handler: &THandler,
    signal_helpers: &SignalHelpers<T>,
    factory: &impl AsyncExpressionFactory<T>,
) -> Vec<Result<(T, Option<DisposeCallback>), Option<String>>>
where
    T::String: StringValue + Send + Sync,
    THandler: Fn(&str, &[&Signal<T>], &SignalHelpers<T>) -> SignalHandlerResult<T>
        + Send
        + Sync
        + 'static,
{
    let custom_signals = signals
        .iter()
        .filter_map(|signal| match signal.signal_type() {
            SignalType::Custom(signal_type) => Some((signal, signal_type)),
            _ => None,
        });
    let custom_signals_by_type = custom_signals.fold(
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
    let mut custom_signal_results = HashMap::with_capacity(signals.len());
    for (signal_type, signals) in custom_signals_by_type {
        let results = signal_handler(signal_type, &signals, signal_helpers);
        if let Some(results) = results {
            let signal_ids = signals.iter().map(|signal| signal.id());
            if results.len() == signals.len() {
                custom_signal_results.extend(signal_ids.zip(results));
            } else {
                custom_signal_results.extend(signal_ids.map(|id| {
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

    signals
        .into_iter()
        .map({
            let factory = factory.clone();
            let commands = commands.clone();
            move |signal| match signal.signal_type() {
                SignalType::Error => Err(Some(parse_error_signal_message(signal.args(), &factory))),
                SignalType::Pending => Err(None),
                SignalType::Custom(_) => match custom_signal_results.remove(&signal.id()) {
                    Some(result) => match result {
                        Ok((result, effect)) => {
                            let dispose = match effect {
                                Some(effect) => apply_effect(effect, &commands),
                                _ => None,
                            };
                            Ok((result, dispose))
                        }
                        Err(error) => Err(Some(error)),
                    },
                    None => Err(Some(format!("Unhandled signal: {}", signal))),
                },
            }
        })
        .collect()
}

fn apply_effect<T: AsyncExpression>(
    effect: RuntimeEffect<T>,
    commands: &CommandChannel<T>,
) -> Option<DisposeCallback>
where
    T::String: StringValue + Send + Sync,
{
    let commands = commands.clone();
    match effect {
        RuntimeEffect::Sync(updates) => {
            tokio::spawn(async move { emit_updates(updates, &commands).await });
            None
        }
        RuntimeEffect::Async((effect, dispose)) => {
            tokio::spawn(async move {
                let updates = effect.await;
                emit_updates(updates, &commands).await
            });
            Some(dispose)
        }
        RuntimeEffect::Stream((mut effect, dispose)) => {
            tokio::spawn(async move {
                while let Some(updates) = effect.next().await {
                    emit_updates(updates, &commands).await
                }
            });
            Some(dispose)
        }
    }
}

async fn emit_updates<T: Expression>(updates: Vec<StateUpdate<T>>, commands: &CommandChannel<T>) {
    let (send, receive) = oneshot::channel();
    let _ = commands.send(Command::update(updates, send)).await;
    let _ = receive.await;
}

fn parse_error_signal_message<T: Expression>(
    args: &ExpressionList<T>,
    factory: &impl ExpressionFactory<T>,
) -> String {
    args.iter()
        .map(|arg| {
            let message = match factory.match_value_term(arg) {
                Some(ValueTerm::String(message)) => Some(String::from(message.as_str())),
                _ => None,
            };
            message.unwrap_or_else(|| format!("{}", arg))
        })
        .collect::<Vec<_>>()
        .join(" ")
}

fn filter_pending_signals<T: Expression>(
    expression: T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Option<T> {
    match factory.match_signal_term(&expression) {
        Some(signal) if signal.signals().into_iter().any(is_pending_signal) => {
            let signals = signal
                .signals()
                .into_iter()
                .filter(|signal| !signal.is_type(&SignalType::Pending))
                .map(|signal| allocator.clone_signal(signal))
                .collect::<Vec<_>>();
            if signals.is_empty() {
                None
            } else {
                Some(factory.create_signal_term(allocator.create_signal_list(signals)))
            }
        }
        _ => Some(expression),
    }
}

fn is_pending_signal<T: Expression>(signal: &Signal<T>) -> bool {
    signal.is_type(&SignalType::Pending)
}

fn strip_pending_results<T: Expression>(
    result: Result<T, Vec<String>>,
) -> Option<SubscriptionResult<T>> {
    match result {
        Ok(result) => Some(Ok(result)),
        Err(errors) => {
            if errors.is_empty() {
                None
            } else {
                Some(Err(errors))
            }
        }
    }
}

fn next_queued_item<T>(target: &mut tokio::sync::mpsc::Receiver<T>) -> Option<T> {
    match target.poll_recv(&mut Context::from_waker(&futures::task::noop_waker())) {
        Poll::Ready(Some(value)) => Some(value),
        _ => None,
    }
}
