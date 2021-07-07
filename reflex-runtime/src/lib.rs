// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
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
    pin::Pin,
    sync::Mutex,
};
use tokio::sync::{broadcast, mpsc, oneshot, watch};
pub use tokio_stream::Stream;
use tokio_stream::{
    wrappers::{BroadcastStream, WatchStream},
    StreamExt,
};

mod store;
use store::Store;

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

type SubscribeCommand = StreamOperation<
    Expression,
    (SubscriptionId, Option<SubscriptionResult>),
    Option<SubscriptionResult>,
>;
type UnsubscribeCommand = Operation<SubscriptionId, bool>;
type UpdateCommand = Operation<Vec<(StateToken, StateUpdate)>, ()>;
pub enum StateUpdate {
    Value(Expression),
    Patch(Box<dyn Fn(Option<&Expression>) -> Expression + Send + Sync + 'static>),
}

pub enum RuntimeEffect {
    Assignment(AssignmentEffect),
    Async(AsyncEffect),
    Stream(StreamEffect),
}
impl RuntimeEffect {
    pub fn assign(key: StateToken, value: Expression) -> Self {
        Self::Assignment(vec![(key, StateUpdate::Value(value))])
    }
    pub fn update(
        key: StateToken,
        updater: impl Fn(Option<&Expression>) -> Expression + Send + Sync + 'static,
    ) -> Self {
        Self::Assignment(vec![(key, StateUpdate::Patch(Box::new(updater)))])
    }
}
pub type AssignmentEffect = Vec<(StateToken, StateUpdate)>;
pub type AsyncEffect = Pin<Box<dyn Future<Output = Expression> + Send + 'static>>;
pub type StreamEffect = Pin<Box<dyn Stream<Item = Expression> + Send + 'static>>;

enum Command {
    Subscribe(SubscribeCommand),
    Unsubscribe(UnsubscribeCommand),
    Update(UpdateCommand),
}
impl Command {
    fn subscribe(
        expression: Expression,
        response: oneshot::Sender<(SubscriptionId, Option<SubscriptionResult>)>,
        update: watch::Sender<Option<SubscriptionResult>>,
    ) -> Self {
        Self::Subscribe(StreamOperation::new(expression, response, update))
    }
    fn unsubscribe(id: SubscriptionId, response: oneshot::Sender<bool>) -> Self {
        Self::Unsubscribe(Operation::new(id, response))
    }
    fn update(updates: Vec<(StateToken, StateUpdate)>, response: oneshot::Sender<()>) -> Self {
        Self::Update(Operation::new(updates, response))
    }
}
pub struct StreamSubscription<'a, T: Stream<Item = SubscriptionResult> + Unpin> {
    channel: &'a CommandChannel,
    id: SubscriptionId,
    initial_value: Option<SubscriptionResult>,
    updates: T,
}
impl<'a, T: Stream<Item = SubscriptionResult> + Unpin> StreamSubscription<'a, T> {
    fn new(
        channel: &'a CommandChannel,
        id: SubscriptionId,
        initial_value: Option<SubscriptionResult>,
        updates: T,
    ) -> Self {
        Self {
            channel,
            id,
            initial_value,
            updates,
        }
    }
    pub async fn next(&mut self) -> Option<SubscriptionResult> {
        match self.initial_value.take() {
            Some(result) => Some(result),
            None => self.updates.next().await,
        }
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
    ) -> Result<(SubscriptionId, Option<SubscriptionResult>), String> {
        start_subscription(expression, &self.commands).await
    }
    pub async fn stop_subscription(&self, id: SubscriptionId) -> Result<bool, String> {
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
            match command {
                Command::Subscribe(command) => process_subscribe_command(
                    command,
                    &mut store,
                    &signal_messages,
                    &results,
                    &signal_handler,
                ),
                Command::Unsubscribe(command) => {
                    process_unsubscribe_command(command, &mut store, &signal_messages, &results)
                }
                Command::Update(command) => process_update_command(
                    command,
                    &mut store,
                    &signal_messages,
                    &results,
                    &signal_handler,
                ),
            }
        }
    });
    (messages_tx, results_tx)
}

fn process_subscribe_command<THandler, TCache: EvaluationCache>(
    command: SubscribeCommand,
    store: &mut Mutex<Store<TCache>>,
    commands_tx: &CommandChannel,
    results_tx: &ResultsChannel,
    signal_handler: &THandler,
) where
    THandler: Fn(&str, &[&Signal], &SignalHelpers) -> SignalHandlerResult + Send + Sync + 'static,
{
    let expression = command.payload;
    let (subscription_id, results) = {
        store.get_mut().unwrap().subscribe(expression, |signals| {
            handle_signals(signals, commands_tx, signal_handler)
        })
    };
    let (initial_value, results): (Vec<_>, Vec<_>) = results
        .into_iter()
        .filter_map(|(id, result)| strip_pending_results(result).map(|result| (id, result)))
        .partition(|(id, _)| (*id == subscription_id));
    let initial_value = initial_value.into_iter().map(|(_, result)| result).next();
    let _ = command.response.send((subscription_id, initial_value));

    for update in results {
        results_tx.send(update).unwrap();
    }
    let mut stream =
        BroadcastStream::new(results_tx.subscribe()).filter_map(move |payload| match payload {
            Ok((id, result)) if id == subscription_id => Some(result),
            _ => None,
        });
    let update_stream = command.update;
    tokio::spawn(async move {
        while let Some(update) = stream.next().await {
            // TODO: Handle update channel errors
            update_stream.send(Some(update)).unwrap();
        }
    });
}

fn process_unsubscribe_command<TCache: EvaluationCache>(
    command: UnsubscribeCommand,
    store: &mut Mutex<Store<TCache>>,
    _commands_tx: &CommandChannel,
    _results_tx: &ResultsChannel,
) {
    let id = command.payload;
    let result = { store.get_mut().unwrap().unsubscribe(id) };
    let _ = command.response.send(result);
}

fn process_update_command<THandler, TCache: EvaluationCache>(
    command: UpdateCommand,
    store: &mut Mutex<Store<TCache>>,
    commands_tx: &CommandChannel,
    results_tx: &ResultsChannel,
    signal_handler: &THandler,
) where
    THandler: Fn(&str, &[&Signal], &SignalHelpers) -> SignalHandlerResult + Send + Sync + 'static,
{
    let updates = command.payload;
    let results = {
        store.get_mut().unwrap().update(updates, |signals| {
            handle_signals(signals, commands_tx, signal_handler)
        })
    };
    let results = results
        .into_iter()
        .filter_map(|(id, result)| strip_pending_results(result).map(|result| (id, result)));
    for update in results {
        results_tx.send(update).unwrap();
    }
    let _ = command.response.send(());
}

async fn create_subscription<'a>(
    expression: Expression,
    channel: &'a CommandChannel,
) -> Result<StreamSubscription<'a, impl Stream<Item = SubscriptionResult>>, String> {
    let (send, receive) = oneshot::channel();
    let (update_send, update_receive) = watch::channel(None);
    channel
        .send(Command::subscribe(expression, send, update_send))
        .await
        .ok()
        .unwrap();
    match receive.await {
        Err(error) => Err(format!("{}", error)),
        Ok((id, initial_value)) => Ok(StreamSubscription::new(
            channel,
            id,
            initial_value,
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

async fn start_subscription(
    expression: Expression,
    channel: &CommandChannel,
) -> Result<(SubscriptionId, Option<SubscriptionResult>), String> {
    let (send, receive) = oneshot::channel();
    let (update_send, _) = watch::channel(None);
    channel
        .send(Command::subscribe(expression, send, update_send))
        .await
        .ok()
        .unwrap();
    match receive.await {
        Err(error) => Err(format!("{}", error)),
        Ok(response) => Ok(response),
    }
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
    let custom_signals = signals.iter().filter_map(|signal| match signal.get_type() {
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

    let (results, effects): (Vec<_>, Vec<Option<(_, _)>>) = signals
        .into_iter()
        .map(|signal| match signal.get_type() {
            SignalType::Error => Err(Some(parse_error_signal_message(signal.args()))),
            SignalType::Pending => Err(None),
            SignalType::Custom(_) => match custom_signal_results.remove(&signal.id()) {
                Some(result) => match result {
                    Ok(result) => Ok((signal, result)),
                    Err(error) => Err(Some(error)),
                },
                None => Err(Some(format!("Unhandled signal: {}", signal))),
            },
        })
        .map(|result| match result {
            Err(error) => (Err(error), None),
            Ok((signal, (result, effect))) => (Ok(result), effect.map(|effect| (signal, effect))),
        })
        .unzip();

    for (signal, effect) in effects.into_iter().filter_map(|effect| effect) {
        apply_signal_effect(signal, effect, commands)
    }
    results
}

fn apply_signal_effect(signal: &Signal, effect: RuntimeEffect, commands: &CommandChannel) {
    let id = signal.id();
    let commands = commands.clone();
    match effect {
        RuntimeEffect::Assignment(updates) => {
            tokio::spawn(async move {
                for (id, update) in updates {
                    emit_update(id, update, &commands).await
                }
            });
        }
        RuntimeEffect::Async(effect) => {
            tokio::spawn(async move {
                let value = effect.await;
                emit_update(id, StateUpdate::Value(value), &commands).await
            });
        }
        RuntimeEffect::Stream(mut effect) => {
            tokio::spawn(async move {
                while let Some(value) = effect.next().await {
                    emit_update(id, StateUpdate::Value(value), &commands).await
                }
            });
        }
    }
}

async fn emit_update(id: StateToken, update: StateUpdate, commands: &CommandChannel) {
    let (send, receive) = oneshot::channel();
    commands
        .send(Command::update(vec![(id, update)], send))
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
