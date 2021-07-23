// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use futures::FutureExt;
use reflex::{
    cache::EvaluationCache,
    core::{Expression, Signal, SignalTerm, StateToken, Term, VariableTerm},
    hash::{hash_object, HashId},
    stdlib::{signal::SignalType, value::ValueTerm},
};
use std::{
    any::TypeId,
    collections::{hash_map::Entry, HashMap},
    future::Future,
    iter::once,
    pin::Pin,
    sync::Mutex,
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

pub type SubscriptionResult = Result<Expression, Vec<String>>;
pub type SignalResult = (Expression, Option<RuntimeEffect>);
pub type SignalHandlerResult = Option<Vec<Result<SignalResult, String>>>;

#[derive(Clone)]
pub struct SignalHelpers {
    commands: CommandChannel,
}
impl SignalHelpers {
    fn new(commands: CommandChannel) -> Self {
        Self { commands }
    }
    pub fn watch_expression(
        self,
        expression: Expression,
    ) -> impl Stream<Item = Expression> + Send + Sync + 'static {
        let (results_tx, results_rx) = watch::channel(None);
        // TODO: Unsubscribe signal handler watch expressions
        tokio::spawn(async move {
            match create_subscription(expression, &self.commands).await {
                Err(error) => {
                    results_tx
                        .send(Some(Expression::new(Term::Signal(SignalTerm::new(
                            Signal::new(
                                SignalType::Error,
                                vec![Expression::new(Term::Value(ValueTerm::String(error)))],
                            ),
                        )))))
                        .unwrap();
                }
                Ok(mut subscription) => {
                    while let Some(result) = subscription.next().await {
                        let value = match result {
                            Ok(value) => value,
                            Err(errors) => Expression::new(Term::Signal(SignalTerm::from(
                                errors.into_iter().map(|error| {
                                    Signal::new(
                                        SignalType::Error,
                                        vec![Expression::new(Term::Value(ValueTerm::String(
                                            error,
                                        )))],
                                    )
                                }),
                            ))),
                        };
                        results_tx.send(Some(value)).unwrap();
                    }
                }
            }
        });
        Box::pin(WatchStream::new(results_rx).filter_map(|value| value))
    }
    pub fn watch_state(
        self,
        state_token: StateToken,
    ) -> impl Stream<Item = Expression> + Send + Sync + 'static {
        self.watch_expression(Expression::new(Term::Variable(VariableTerm::dynamic(
            state_token,
            Void::create(),
        ))))
        .filter(|value| !Void::is(value))
    }
}

struct Void {}
impl Void {
    fn hash() -> HashId {
        hash_object(&TypeId::of::<Self>())
    }
    fn create() -> Expression {
        Expression::new(Term::Value(ValueTerm::Hash(Self::hash())))
    }
    fn is(value: &Expression) -> bool {
        match value.value() {
            Term::Value(ValueTerm::Hash(hash)) if *hash == Self::hash() => true,
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

type CommandChannel = mpsc::Sender<Command>;
type ResultsChannel = broadcast::Sender<(SubscriptionId, SubscriptionResult)>;

type SubscribeCommand = StreamOperation<Expression, SubscriptionId, Option<SubscriptionResult>>;
type UnsubscribeCommand = Operation<SubscriptionId, bool>;
type UpdateCommand = Operation<Vec<StateUpdate>, ()>;

pub struct StateUpdate {
    state_token: StateToken,
    update: StateUpdateType,
}
pub enum StateUpdateType {
    Value(Expression),
    Patch(Box<dyn Fn(Option<&Expression>) -> Expression + Send + Sync + 'static>),
}
impl StateUpdate {
    pub fn value(state_token: StateToken, value: Expression) -> Self {
        Self {
            state_token,
            update: StateUpdateType::Value(value),
        }
    }
    pub fn patch(
        state_token: StateToken,
        updater: impl Fn(Option<&Expression>) -> Expression + Send + Sync + 'static,
    ) -> Self {
        Self {
            state_token,
            update: StateUpdateType::Patch(Box::new(updater)),
        }
    }
}

pub enum RuntimeEffect {
    Sync(SyncEffect),
    Async(AsyncEffect),
    Stream(StreamEffect),
}
pub type SyncEffect = Vec<StateUpdate>;
pub type AsyncEffect = Pin<Box<dyn Future<Output = Vec<StateUpdate>> + Send + 'static>>;
pub type StreamEffect = Pin<Box<dyn Stream<Item = Vec<StateUpdate>> + Send + 'static>>;
impl RuntimeEffect {
    pub fn assign(state_token: StateToken, value: Expression) -> Self {
        Self::batch_assign(once((state_token, value)))
    }
    pub fn update(
        state_token: StateToken,
        updater: impl Fn(Option<&Expression>) -> Expression + Send + Sync + 'static,
    ) -> Self {
        Self::batch_update(once((state_token, updater)))
    }
    pub fn deferred(update: impl Future<Output = StateUpdate> + Send + 'static) -> Self {
        Self::batch_deferred(update.map(|update| vec![update]))
    }
    pub fn stream(update: impl Stream<Item = StateUpdate> + Send + 'static) -> Self {
        Self::batch_stream(update.map(|update| vec![update]))
    }
    pub fn batch_assign(values: impl IntoIterator<Item = (StateToken, Expression)>) -> Self {
        Self::Sync(
            values
                .into_iter()
                .map(|(state_token, value)| StateUpdate::value(state_token, value))
                .collect::<Vec<_>>(),
        )
    }
    pub fn batch_update(
        updates: impl IntoIterator<
            Item = (
                StateToken,
                impl Fn(Option<&Expression>) -> Expression + Send + Sync + 'static,
            ),
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
        updates: impl Future<Output = Vec<StateUpdate>> + Send + 'static,
    ) -> Self {
        Self::Async(Box::pin(updates))
    }
    pub fn batch_stream(updates: impl Stream<Item = Vec<StateUpdate>> + Send + 'static) -> Self {
        Self::Stream(Box::pin(updates))
    }
}

enum Command {
    Subscribe(SubscribeCommand),
    Unsubscribe(UnsubscribeCommand),
    Update(UpdateCommand),
}
impl Command {
    fn subscribe(
        expression: Expression,
        response: oneshot::Sender<SubscriptionId>,
        update: watch::Sender<Option<SubscriptionResult>>,
    ) -> Self {
        Self::Subscribe(StreamOperation::new(expression, response, update))
    }
    fn unsubscribe(id: SubscriptionId, response: oneshot::Sender<bool>) -> Self {
        Self::Unsubscribe(Operation::new(id, response))
    }
    fn update(updates: Vec<StateUpdate>, response: oneshot::Sender<()>) -> Self {
        Self::Update(Operation::new(updates, response))
    }
}
pub struct StreamSubscription<'a, T: Stream<Item = SubscriptionResult>> {
    channel: &'a CommandChannel,
    id: SubscriptionId,
    updates: T,
}
impl<'a, T: Stream<Item = SubscriptionResult> + Unpin> StreamSubscription<'a, T> {
    fn new(channel: &'a CommandChannel, id: SubscriptionId, updates: T) -> Self {
        Self {
            channel,
            id,
            updates,
        }
    }
    pub fn id(&self) -> SubscriptionId {
        self.id
    }
    pub fn into_stream(self) -> T {
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
impl<'a, T: Stream<Item = SubscriptionResult> + Unpin> Stream for StreamSubscription<'a, T> {
    type Item = SubscriptionResult;

    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let updates = &mut self.updates;
        pin!(updates);
        Stream::poll_next(updates, cx)
    }
}

pub struct Runtime {
    commands: CommandChannel,
    results: ResultsChannel,
}
impl Runtime {
    pub fn new<THandler, TCache: EvaluationCache + Send + Sync + 'static>(
        signal_handler: THandler,
        evaluation_cache: TCache,
        command_buffer_size: usize,
        result_buffer_size: usize,
    ) -> Self
    where
        THandler:
            Fn(&str, &[&Signal], &SignalHelpers) -> SignalHandlerResult + Send + Sync + 'static,
    {
        let (commands, results) = create_store(
            signal_handler,
            evaluation_cache,
            command_buffer_size,
            result_buffer_size,
        );
        Self { commands, results }
    }
    pub async fn subscribe<'a>(
        &'a self,
        expression: Expression,
    ) -> Result<StreamSubscription<'a, impl Stream<Item = SubscriptionResult>>, String> {
        create_subscription(expression, &self.commands).await
    }
    pub async fn start_subscription(
        &self,
        expression: Expression,
    ) -> Result<SubscriptionId, String> {
        let subscription = self.subscribe(expression).await;
        subscription.map(|subscription| subscription.id)
    }
    pub async fn unsubscribe(&self, id: SubscriptionId) -> Result<bool, String> {
        stop_subscription(id, &self.commands).await
    }
    pub fn watch_subscription(
        &self,
        subscription_id: SubscriptionId,
    ) -> impl Stream<Item = SubscriptionResult> {
        BroadcastStream::new(self.results.subscribe()).filter_map(move |payload| match payload {
            Ok((id, result)) if id == subscription_id => match result {
                Ok(expression) => match filter_pending_signals(expression) {
                    Some(expression) => Some(Ok(expression)),
                    None => None,
                },
                Err(error) => Some(Err(error)),
            },
            _ => None,
        })
    }
}

fn create_store<THandler, TCache: EvaluationCache + Send + Sync + 'static>(
    signal_handler: THandler,
    cache: TCache,
    command_buffer_size: usize,
    result_buffer_size: usize,
) -> (CommandChannel, ResultsChannel)
where
    THandler: Fn(&str, &[&Signal], &SignalHelpers) -> SignalHandlerResult + Send + Sync + 'static,
{
    let (messages_tx, mut messages_rx) = mpsc::channel::<Command>(command_buffer_size);
    let (results_tx, _) = broadcast::channel(result_buffer_size);
    let results = results_tx.clone();
    let signal_messages = messages_tx.clone();
    tokio::spawn(async move {
        let mut store = Mutex::new(Store::new(cache, None));
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
            let updated_results = store
                .get_mut()
                .unwrap()
                .flush(|signals| handle_signals(signals, &signal_messages, &signal_handler));
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

fn process_subscribe_command<TCache: EvaluationCache>(
    command: SubscribeCommand,
    store: &mut Mutex<Store<TCache>>,
    results_tx: &ResultsChannel,
) {
    let expression = command.payload;
    let subscription_id = store.get_mut().unwrap().subscribe(expression);
    let _ = command.response.send(subscription_id);

    let mut results_stream = get_subscription_results_stream(results_tx, subscription_id);

    let response_stream = command.update;
    tokio::spawn(async move {
        while let Some(update) = results_stream.next().await {
            let _ = response_stream.send(Some(update));
        }
    });
}

fn process_unsubscribe_command<TCache: EvaluationCache>(
    command: UnsubscribeCommand,
    store: &mut Mutex<Store<TCache>>,
    _results_tx: &ResultsChannel,
) {
    let id = command.payload;
    let result = store.get_mut().unwrap().unsubscribe(id);
    let _ = command.response.send(result);
}

fn process_update_command<TCache: EvaluationCache>(
    command: UpdateCommand,
    store: &mut Mutex<Store<TCache>>,
    _results_tx: &ResultsChannel,
) {
    let updates = command.payload;
    store.get_mut().unwrap().update(updates);
    let _ = command.response.send(());
}

async fn create_subscription<'a>(
    expression: Expression,
    commands: &'a CommandChannel,
) -> Result<StreamSubscription<'a, impl Stream<Item = SubscriptionResult>>, String> {
    let (send, receive) = oneshot::channel();
    let (update_send, update_receive) = watch::channel(None);
    commands
        .send(Command::subscribe(expression, send, update_send))
        .await
        .ok()
        .unwrap();
    match receive.await {
        Err(error) => Err(format!("{}", error)),
        Ok(subscription_id) => Ok(StreamSubscription::new(
            commands,
            subscription_id,
            WatchStream::new(update_receive)
                .filter_map(|result| result)
                .filter_map(|result| match result {
                    Ok(expression) => match filter_pending_signals(expression) {
                        Some(expression) => Some(Ok(expression)),
                        None => None,
                    },
                    Err(error) => Some(Err(error)),
                }),
        )),
    }
}

fn get_subscription_results_stream(
    results_tx: &ResultsChannel,
    subscription_id: SubscriptionToken,
) -> impl Stream<Item = Result<Expression, Vec<String>>> {
    // TODO: Investigate per-subscription results channels instead of shared global results channel
    BroadcastStream::new(results_tx.subscribe()).filter_map(move |payload| match payload {
        Ok((id, result)) if id == subscription_id => Some(result),
        _ => None,
    })
}

async fn stop_subscription(id: SubscriptionId, channel: &CommandChannel) -> Result<bool, String> {
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

fn handle_signals<THandler>(
    signals: &[&Signal],
    commands: &CommandChannel,
    signal_handler: &THandler,
) -> Vec<Result<Expression, Option<String>>>
where
    THandler: Fn(&str, &[&Signal], &SignalHelpers) -> SignalHandlerResult + Send + Sync + 'static,
{
    let custom_signals = signals
        .iter()
        .filter_map(|signal| match signal.signal_type() {
            SignalType::Custom(signal_type) => Some((signal, signal_type)),
            _ => None,
        });
    let custom_signals_by_type = custom_signals.fold(
        HashMap::<&String, Vec<&Signal>>::new(),
        |mut results, (signal, signal_type)| {
            match results.entry(signal_type) {
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
        let results = signal_handler(signal_type, &signals, &SignalHelpers::new(commands.clone()));
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
            SignalType::Error => Err(Some(parse_error_signal_message(signal.args()))),
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

fn apply_effect(effect: RuntimeEffect, commands: &CommandChannel) {
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

async fn emit_updates(updates: Vec<StateUpdate>, commands: &CommandChannel) {
    let (send, receive) = oneshot::channel();
    commands
        .send(Command::update(updates, send))
        .await
        .ok()
        .unwrap();
    receive.await.unwrap();
}

fn parse_error_signal_message(args: &[Expression]) -> String {
    args.iter()
        .map(|arg| {
            let message = match arg.value() {
                Term::Value(arg) => match arg {
                    ValueTerm::String(message) => Some(String::from(message)),
                    _ => None,
                },
                _ => None,
            };
            message.unwrap_or_else(|| format!("{}", arg))
        })
        .collect::<Vec<_>>()
        .join(" ")
}

fn filter_pending_signals(expression: Expression) -> Option<Expression> {
    match expression.value() {
        Term::Signal(signal) if signal.signals().into_iter().any(is_pending_signal) => {
            let signals = signal
                .signals()
                .into_iter()
                .filter(|signal| !signal.is_type(SignalType::Pending))
                .map(|signal| signal.clone())
                .collect::<Vec<_>>();
            if signals.is_empty() {
                None
            } else {
                Some(Expression::new(Term::Signal(SignalTerm::from(signals))))
            }
        }
        _ => Some(expression),
    }
}

fn is_pending_signal(signal: &Signal) -> bool {
    signal.is_type(SignalType::Pending)
}

fn strip_pending_results(result: Result<Expression, Vec<String>>) -> Option<SubscriptionResult> {
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
