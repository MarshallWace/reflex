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
        Applicable, Expression, ExpressionFactory, ExpressionList, HeapAllocator, Reducible,
        Rewritable, Signal, SignalType, StateToken, StringValue,
    },
    hash::{hash_object, HashId},
    interpreter::{DefaultInterpreterCache, Execute, InterpreterCache, InterpreterOptions},
    lang::ValueTerm,
};
use std::{
    any::TypeId,
    collections::{hash_map::Entry, HashMap},
    convert::identity,
    future::Future,
    iter::once,
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
    commands: CommandChannel<T>,
    compiler_options: CompilerOptions,
}
impl<T: AsyncExpression + Rewritable<T> + Reducible<T> + Compile<T>> SignalHelpers<T>
where
    T::String: StringValue + Send + Sync,
{
    fn new(
        program: Arc<Program>,
        commands: CommandChannel<T>,
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
        factory: &impl AsyncExpressionFactory<T>,
        allocator: &impl AsyncHeapAllocator<T>,
    ) -> Result<impl Stream<Item = SubscriptionResult<T>> + Send + 'static, String> {
        let entry_point = InstructionPointer::new(self.program.len());
        let prelude = (*self.program).clone();
        Compiler::new(self.compiler_options, Some(prelude))
            .compile(&expression, CompilerMode::Program, factory, allocator)
            .map(|compiled| {
                // TODO: Error if runtime expression depends on unrecognized native functions
                let (program, _) = compiled.into_parts();
                self.watch_compiled_expression(program, entry_point, factory, allocator)
            })
    }
    pub fn watch_state(
        self,
        state_token: StateToken,
        factory: &(impl ExpressionFactory<T> + Send + Sync + Clone + 'static),
        allocator: &(impl HeapAllocator<T> + Send + Sync + Clone + 'static),
    ) -> impl Stream<Item = SubscriptionResult<T>> + Send + 'static {
        let program = Program::new(vec![
            Instruction::PushHash { value: Void::uid() },
            Instruction::PushDynamic { state_token },
            Instruction::Evaluate,
            Instruction::End,
        ]);
        self.watch_compiled_expression(program, InstructionPointer::default(), factory, allocator)
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
                    &self.commands,
                    &factory,
                    &allocator,
                )
                .await
                {
                    Err(error) => {
                        results_tx.send(Some(Err(vec![error]))).unwrap();
                    }
                    Ok(mut subscription) => {
                        while let Some(result) = subscription.next().await {
                            results_tx.send(Some(result)).unwrap();
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

type SubscribeCommand<T> =
    StreamOperation<(Program, InstructionPointer), SubscriptionId, Option<SubscriptionResult<T>>>;
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
pub type AsyncEffect<T> = Pin<Box<dyn Future<Output = Vec<StateUpdate<T>>> + Send + 'static>>;
pub type StreamEffect<T> = Pin<Box<dyn Stream<Item = Vec<StateUpdate<T>>> + Send + 'static>>;

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
    pub fn deferred(update: impl Future<Output = StateUpdate<T>> + Send + 'static) -> Self {
        Self::batch_deferred(update.map(|update| vec![update]))
    }
    pub fn stream(update: impl Stream<Item = StateUpdate<T>> + Send + 'static) -> Self {
        Self::batch_stream(update.map(|update| vec![update]))
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
    ) -> Self {
        Self::Async(Box::pin(updates))
    }
    pub fn batch_stream(updates: impl Stream<Item = Vec<StateUpdate<T>>> + Send + 'static) -> Self {
        Self::Stream(Box::pin(updates))
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
        response: oneshot::Sender<SubscriptionId>,
        update: watch::Sender<Option<SubscriptionResult<T>>>,
    ) -> Self {
        Self::Subscribe(StreamOperation::new(
            (program, entry_point),
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
        self.channel
            .send(Command::unsubscribe(self.id, send))
            .await
            .ok()
            .unwrap();
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

pub struct Runtime<
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T> + Execute<T>,
> {
    program: Arc<Program>,
    commands: CommandChannel<T>,
    results: ResultsChannel<T>,
}
impl<
        T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T> + Execute<T>,
    > Runtime<T>
where
    T::String: StringValue + Send + Sync,
{
    pub fn new<THandler>(
        root: Program,
        signal_handler: THandler,
        factory: &impl AsyncExpressionFactory<T>,
        allocator: &impl AsyncHeapAllocator<T>,
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
        create_subscription(program, entry_point, &self.commands, factory, allocator).await
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

fn create_store<
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T> + Execute<T>,
    THandler,
>(
    program: Arc<Program>,
    signal_handler: THandler,
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
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
    let (messages_tx, mut messages_rx) = mpsc::channel::<Command<T>>(command_buffer_size);
    let (results_tx, _) = broadcast::channel(result_buffer_size);
    let results = results_tx.clone();
    let signal_messages = messages_tx.clone();
    let factory = factory.clone();
    let allocator = allocator.clone();
    let signal_helpers = SignalHelpers::new(program, messages_tx.clone(), compiler_options);
    tokio::spawn(async move {
        let cache = DefaultInterpreterCache::default();
        let mut store = Mutex::new(Store::new(cache, None, plugins, interpreter_options));
        while let Some(command) = messages_rx.recv().await {
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
                next_command = next_queued_item(&mut messages_rx)
            }
            let updated_results = store.get_mut().unwrap().flush(
                |signals| {
                    handle_signals(
                        signals,
                        &signal_messages,
                        &signal_handler,
                        &signal_helpers,
                        &factory,
                    )
                },
                &factory,
                &allocator,
            );
            let updated_results =
                updated_results
                    .into_iter()
                    .filter_map(|(id, result)| match strip_pending_results(result) {
                        None => None,
                        Some(result) => Some((id, result)),
                    });
            for update in updated_results {
                results.send(update).unwrap();
            }
        }
    });
    (messages_tx, results_tx)
}

fn process_subscribe_command<
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Execute<T>,
    TCache: InterpreterCache<T>,
>(
    command: SubscribeCommand<T>,
    store: &mut Mutex<Store<T, TCache>>,
    results_tx: &ResultsChannel<T>,
) where
    T::String: StringValue + Send + Sync,
{
    let (program, entry_point) = command.payload;
    let subscription_id = store.get_mut().unwrap().subscribe(program, entry_point);
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
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Execute<T>,
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
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Execute<T>,
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
    commands: &'a CommandChannel<T>,
    factory: &(impl ExpressionFactory<T> + Send + Clone + 'static),
    allocator: &(impl HeapAllocator<T> + Send + Clone + 'static),
) -> Result<StreamSubscription<'a, T, impl Stream<Item = SubscriptionResult<T>>>, String>
where
    T::String: StringValue + Send + Sync,
{
    let (send, receive) = oneshot::channel();
    let (update_send, update_receive) = watch::channel(None);
    commands
        .send(Command::subscribe(program, entry_point, send, update_send))
        .await
        .ok()
        .unwrap();
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
    channel
        .send(Command::unsubscribe(id, send))
        .await
        .ok()
        .unwrap();
    match receive.await {
        Err(error) => Err(format!("{}", error)),
        Ok(response) => Ok(response),
    }
}

fn handle_signals<
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T> + Execute<T>,
    THandler,
>(
    signals: &[&Signal<T>],
    commands: &CommandChannel<T>,
    signal_handler: &THandler,
    signal_helpers: &SignalHelpers<T>,
    factory: &impl AsyncExpressionFactory<T>,
) -> Vec<Result<T, Option<String>>>
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

    let (results, effects): (Vec<_>, Vec<Option<_>>) = signals
        .into_iter()
        .map(|signal| match signal.signal_type() {
            SignalType::Error => Err(Some(parse_error_signal_message(signal.args(), factory))),
            SignalType::Pending => Err(None),
            SignalType::Custom(_) => match custom_signal_results.remove(&signal.id()) {
                Some(result) => match result {
                    Ok(result) => Ok(result),
                    Err(error) => Err(Some(error)),
                },
                None => Err(Some(format!("Unhandled signal: {}", signal))),
            },
        })
        .map(|result| match result {
            Err(error) => (Err(error), None),
            Ok((result, effect)) => (Ok(result), effect),
        })
        .unzip();

    for effect in effects.into_iter().filter_map(|effect| effect) {
        apply_effect(effect, commands)
    }
    results
}

fn apply_effect<T: AsyncExpression>(effect: RuntimeEffect<T>, commands: &CommandChannel<T>)
where
    T::String: StringValue + Send + Sync,
{
    let commands = commands.clone();
    match effect {
        RuntimeEffect::Sync(updates) => {
            tokio::spawn(async move { emit_updates(updates, &commands).await });
        }
        RuntimeEffect::Async(effect) => {
            tokio::spawn(async move {
                let updates = effect.await;
                emit_updates(updates, &commands).await
            });
        }
        RuntimeEffect::Stream(mut effect) => {
            tokio::spawn(async move {
                while let Some(updates) = effect.next().await {
                    emit_updates(updates, &commands).await
                }
            });
        }
    }
}

async fn emit_updates<T: Expression>(updates: Vec<StateUpdate<T>>, commands: &CommandChannel<T>) {
    let (send, receive) = oneshot::channel();
    commands
        .send(Command::update(updates, send))
        .await
        .ok()
        .unwrap();
    receive.await.unwrap();
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
