// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::{
    core::{Expression, Signal, SignalTerm, StateToken, Term},
    hash::Hashable,
    stdlib::{signal::SignalType, value::ValueTerm},
    store::Store,
};
use std::{collections::HashMap, future::Future, pin::Pin, sync::Mutex};
use tokio::sync::{broadcast, mpsc, oneshot, watch};
use tokio_stream::{
    wrappers::{BroadcastStream, WatchStream},
    Stream, StreamExt,
};

mod handlers;
pub use handlers::builtin_signal_handlers;
pub(crate) mod utils;

pub type SignalHandler =
    Box<dyn Fn(Option<&[ValueTerm]>) -> Result<SignalResult, String> + Send + Sync + 'static>;
pub type SignalResult = (Expression, Option<SignalEffect>);

pub fn create_signal_handler(
    signal_type: &'static str,
    handler: impl Fn(Option<&[ValueTerm]>) -> Result<SignalResult, String> + Send + Sync + 'static,
) -> (&'static str, SignalHandler) {
    (signal_type, Box::new(handler))
}

pub type SubscriptionResult = Result<Expression, Vec<String>>;

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

type SubscriptionId = usize;

type CommandChannel = mpsc::Sender<Command>;
type ResultsChannel = broadcast::Sender<(SubscriptionId, SubscriptionResult)>;

type SubscribeCommand =
    StreamOperation<Expression, (SubscriptionId, Option<SubscriptionResult>), SubscriptionResult>;
type UnsubscribeCommand = Operation<SubscriptionId, bool>;
type UpdateCommand = Operation<Vec<(StateToken, Expression)>, ()>;

type SignalHandlers = HashMap<String, SignalHandler>;
type SignalEffect = Pin<Box<dyn Future<Output = Expression> + Send + 'static>>;

enum Command {
    Subscribe(SubscribeCommand),
    Unsubscribe(UnsubscribeCommand),
    Update(UpdateCommand),
}
impl Command {
    fn subscribe(
        expression: Expression,
        response: oneshot::Sender<(SubscriptionId, Option<SubscriptionResult>)>,
        update: watch::Sender<SubscriptionResult>,
    ) -> Self {
        Self::Subscribe(StreamOperation::new(expression, response, update))
    }
    fn unsubscribe(id: SubscriptionId, response: oneshot::Sender<bool>) -> Self {
        Self::Unsubscribe(Operation::new(id, response))
    }
    fn update(updates: Vec<(StateToken, Expression)>, response: oneshot::Sender<()>) -> Self {
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
}
impl Runtime {
    pub fn new(
        signal_handlers: impl IntoIterator<Item = (&'static str, SignalHandler)>,
        command_buffer_size: usize,
        result_buffer_size: usize,
    ) -> Self {
        Self {
            commands: create_store(signal_handlers, command_buffer_size, result_buffer_size),
        }
    }
    pub async fn subscribe<'a>(
        &'a self,
        expression: Expression,
    ) -> Result<StreamSubscription<'a, impl Stream<Item = SubscriptionResult>>, String> {
        create_subscription(expression, &self.commands).await
    }
}

fn create_store(
    signal_handlers: impl IntoIterator<Item = (&'static str, SignalHandler)>,
    command_buffer_size: usize,
    result_buffer_size: usize,
) -> CommandChannel {
    let signal_handlers = signal_handlers
        .into_iter()
        .map(|(signal_type, handler)| (String::from(signal_type), handler))
        .collect::<HashMap<_, _>>();
    let (messages_tx, mut messages_rx) = mpsc::channel::<Command>(command_buffer_size);
    let signal_messages = messages_tx.clone();
    tokio::spawn(async move {
        let mut store = Mutex::new(Store::new(None));
        let (results_tx, _) = broadcast::channel(result_buffer_size);
        while let Some(command) = messages_rx.recv().await {
            match command {
                Command::Subscribe(command) => process_subscribe_command(
                    command,
                    &mut store,
                    &signal_messages,
                    &results_tx,
                    &signal_handlers,
                ),
                Command::Unsubscribe(command) => {
                    process_unsubscribe_command(command, &mut store, &signal_messages, &results_tx)
                }
                Command::Update(command) => process_update_command(
                    command,
                    &mut store,
                    &signal_messages,
                    &results_tx,
                    &signal_handlers,
                ),
            }
        }
    });
    messages_tx
}

fn process_subscribe_command(
    command: SubscribeCommand,
    store: &mut Mutex<Store>,
    commands_tx: &CommandChannel,
    results_tx: &ResultsChannel,
    signal_handlers: &SignalHandlers,
) {
    let expression = command.payload;
    let (subscription_id, results) = {
        store.get_mut().unwrap().subscribe(expression, |signals| {
            handle_signals(signals, commands_tx, signal_handlers)
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
            update_stream.send(update).unwrap();
        }
    });
}

fn process_unsubscribe_command(
    command: UnsubscribeCommand,
    store: &mut Mutex<Store>,
    _commands_tx: &CommandChannel,
    _results_tx: &ResultsChannel,
) {
    let id = command.payload;
    let result = { store.get_mut().unwrap().unsubscribe(id) };
    let _ = command.response.send(result);
}

fn process_update_command(
    command: UpdateCommand,
    store: &mut Mutex<Store>,
    commands_tx: &CommandChannel,
    results_tx: &ResultsChannel,
    signal_handlers: &SignalHandlers,
) {
    let updates = command.payload;
    let results = {
        store.get_mut().unwrap().update(updates, |signals| {
            handle_signals(signals, commands_tx, signal_handlers)
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
    let (update_send, update_receive) = watch::channel(Ok(create_pending_expression()));
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
            WatchStream::new(update_receive).filter(|result| match result {
                Ok(expression) => match expression.value() {
                    Term::Signal(signal) if signal.is_type(SignalType::Pending) => false,
                    _ => true,
                },
                _ => true,
            }),
        )),
    }
}

fn handle_signals(
    signals: &[Signal],
    commands: &CommandChannel,
    signal_handlers: &SignalHandlers,
) -> Result<Vec<Expression>, Option<Vec<String>>> {
    let results = signals
        .iter()
        .map(|signal| match signal.get_type() {
            SignalType::Error => Err(Some(parse_error_signal_message(signal.args()))),
            SignalType::Pending => Err(None),
            SignalType::Custom(signal_type) => {
                let handler = signal_handlers.get(signal_type);
                match handler {
                    None => Err(Some(format!("Unhandled signal: {}", signal_type))),
                    Some(handler) => match handler(match signal.args() {
                        Some(args) => Some(args),
                        None => None,
                    }) {
                        Ok(result) => Ok(result),
                        Err(error) => Err(Some(error)),
                    },
                }
            }
        })
        // TODO: Allow partial errors when processing signals
        .collect::<Result<Vec<_>, _>>()
        .or_else(|error| Err(error.map(|error| vec![error])));
    match results {
        Err(errors) => Err(errors),
        Ok(results) => {
            let (results, effects) = extract_signal_effects(signals, results);
            for (id, effect) in effects {
                let commands = commands.clone();
                tokio::spawn(async move {
                    let value = effect.await;
                    let (send, receive) = oneshot::channel();
                    commands
                        .send(Command::update(vec![(id, value)], send))
                        .await
                        .ok()
                        .unwrap();
                    receive.await.unwrap();
                });
            }
            Ok(results)
        }
    }
}

fn extract_signal_effects<'a>(
    signals: impl IntoIterator<Item = &'a Signal>,
    results: impl IntoIterator<Item = SignalResult>,
) -> (Vec<Expression>, Vec<(StateToken, SignalEffect)>) {
    let (pure_results, stateful_results): (Vec<_>, Vec<_>) = results
        .into_iter()
        .zip(signals.into_iter().map(|signal| signal.hash()))
        .map(|((result, effect), id)| (result, effect.map(|effect| (id, effect))))
        .partition(|(_, effect)| effect.is_none());
    let (stateful_results, effects): (Vec<_>, Vec<_>) = stateful_results
        .into_iter()
        .filter_map(|(result, effect)| effect.map(|effect| (result, effect)))
        .unzip();
    let results = pure_results
        .into_iter()
        .map(|(result, _)| result)
        .chain(stateful_results.into_iter())
        .collect::<Vec<_>>();
    (results, effects)
}

fn parse_error_signal_message(args: Option<&Vec<ValueTerm>>) -> String {
    let args = match args {
        None => Vec::new(),
        Some(args) => args
            .iter()
            .map(|arg| match arg {
                ValueTerm::String(message) => String::from(message),
                arg => format!("{}", arg),
            })
            .collect::<Vec<_>>(),
    };
    args.join(" ")
}

fn create_pending_expression() -> Expression {
    Expression::new(Term::Signal(SignalTerm::new(Signal::new(
        SignalType::Pending,
        Vec::new(),
    ))))
}

fn strip_pending_results(
    result: Result<Expression, Option<Vec<String>>>,
) -> Option<SubscriptionResult> {
    match result {
        Ok(result) => Some(Ok(result)),
        Err(Some(errors)) => Some(Err(errors)),
        Err(None) => None,
    }
}
