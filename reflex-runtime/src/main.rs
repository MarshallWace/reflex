// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use hyper::{
    service::{make_service_fn, service_fn},
    StatusCode, Uri,
};
use hyper::{Body, Request, Response, Server};
use reflex::{
    core::{Expression, Signal, SignalTerm, StateToken, Term},
    hash::Hashable,
    stdlib::{signal::SignalType, value::ValueTerm},
    store::Store,
};
use reflex_js::{builtin_globals, builtin_imports, parse, stringify, Env};
use std::{
    convert::Infallible,
    future::Future,
    net::SocketAddr,
    pin::Pin,
    sync::{Arc, Mutex},
};
use tokio::sync::{broadcast, mpsc, oneshot, watch};
use tokio_stream::{
    wrappers::{BroadcastStream, WatchStream},
    Stream, StreamExt,
};

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
type SubscriptionResult = Result<Expression, Vec<String>>;

type CommandChannel = mpsc::Sender<Command>;
type ResultsChannel = broadcast::Sender<(SubscriptionId, SubscriptionResult)>;

type SubscribeCommand =
    StreamOperation<Expression, (SubscriptionId, Option<SubscriptionResult>), SubscriptionResult>;
type UnsubscribeCommand = Operation<SubscriptionId, bool>;
type UpdateCommand = Operation<Vec<(StateToken, Expression)>, ()>;

type SignalResult = (Expression, Option<SignalEffect>);
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
struct StreamSubscription<'a, T: Stream<Item = SubscriptionResult> + Unpin> {
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
    async fn next(&mut self) -> Option<SubscriptionResult> {
        match self.initial_value.take() {
            Some(result) => Some(result),
            None => self.updates.next().await,
        }
    }
    async fn unsubscribe(&self) -> Result<bool, String> {
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

struct HttpResponse {
    status: StatusCode,
    body: String,
}
impl HttpResponse {
    fn new(status: StatusCode, body: String) -> Self {
        Self { status, body }
    }
}

#[tokio::main]
pub async fn main() {
    let command_buffer_size = 32;
    let result_buffer_size = 32;
    let env = Env::new()
        .with_globals(builtin_globals())
        .with_imports(builtin_imports());
    let env = Arc::new(env);

    let command_channel = create_shared_store(command_buffer_size, result_buffer_size);

    let address = SocketAddr::from(([127, 0, 0, 1], 3000));
    let server = Server::bind(&address).serve(make_service_fn(|_conn| {
        let command_channel = command_channel.clone();
        let env = Arc::clone(&env);
        async move {
            Ok::<_, Infallible>(service_fn(move |req| {
                let subscription_channel = command_channel.clone();
                let env = Arc::clone(&env);
                async move {
                    let response = match parse_request(req).await {
                        Err(response) => Ok(response),
                        Ok(source) => match parse_expression(source, &env) {
                            Err(response) => Ok(response),
                            Ok(expression) => {
                                match create_subscription(expression, &subscription_channel).await {
                                    Err(error) => Err(error),
                                    Ok(mut results) => match results.next().await {
                                        None => Err(String::from("Empty result stream")),
                                        Some(result) => match results.unsubscribe().await {
                                            Ok(_) => Ok(format_http_response(result)),
                                            Err(error) => Err(error),
                                        },
                                    },
                                }
                            }
                        },
                    };
                    let response = match response {
                        Ok(response) => response,
                        Err(error) => HttpResponse::new(StatusCode::INTERNAL_SERVER_ERROR, error),
                    };
                    Ok::<Response<Body>, Infallible>({
                        let status = response.status;
                        let mut response = Response::new(Body::from(response.body));
                        *response.status_mut() = status;
                        response
                    })
                }
            }))
        }
    }));
    if let Err(e) = server.await {
        eprintln!("server error: {}", e);
    }
}

fn create_shared_store(command_buffer_size: usize, result_buffer_size: usize) -> CommandChannel {
    let (messages_tx, mut messages_rx) = mpsc::channel::<Command>(command_buffer_size);
    let signal_messages = messages_tx.clone();
    tokio::spawn(async move {
        let mut store = Mutex::new(Store::new(None));
        let (results_tx, _) = broadcast::channel(result_buffer_size);
        while let Some(command) = messages_rx.recv().await {
            match command {
                Command::Subscribe(command) => {
                    process_subscribe_command(command, &mut store, &signal_messages, &results_tx)
                }
                Command::Unsubscribe(command) => {
                    process_unsubscribe_command(command, &mut store, &signal_messages, &results_tx)
                }
                Command::Update(command) => {
                    process_update_command(command, &mut store, &signal_messages, &results_tx)
                }
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
) {
    let expression = command.payload;
    let (subscription_id, results) = {
        store
            .get_mut()
            .unwrap()
            .subscribe(expression, |signals| handle_signals(signals, commands_tx))
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
) {
    let updates = command.payload;
    let results = {
        store
            .get_mut()
            .unwrap()
            .update(updates, |signals| handle_signals(signals, commands_tx))
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
) -> Result<Vec<Expression>, Option<Vec<String>>> {
    let results = signals
        .iter()
        .map(|signal| match signal.get_type() {
            SignalType::Error => Err(Some(parse_error_message(signal.args()))),
            SignalType::Pending => Err(None),
            SignalType::Custom(signal_type) => {
                match custom_signal_handler(signal_type, signal.args()) {
                    Ok(result) => Ok(result),
                    Err(error) => Err(Some(error)),
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

async fn parse_request(req: Request<Body>) -> Result<String, HttpResponse> {
    let body = match hyper::body::to_bytes(req.into_body()).await {
        Ok(body) => match String::from_utf8(body.into_iter().collect()) {
            Ok(body) => Ok(body),
            Err(_) => Err(None),
        },
        Err(_) => Err(None),
    };
    body.or_else(|error| {
        Err(HttpResponse::new(
            StatusCode::BAD_REQUEST,
            error.unwrap_or_else(|| String::from("Bad request")),
        ))
    })
}

fn parse_expression(source: String, env: &Env) -> Result<Expression, HttpResponse> {
    match parse(&source, &env) {
        Err(error) => Err(HttpResponse::new(StatusCode::BAD_REQUEST, error)),
        Ok(expression) => Ok(expression),
    }
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

fn format_http_response(result: SubscriptionResult) -> HttpResponse {
    match result {
        Ok(result) => match stringify(result.value()) {
            Ok(output) => HttpResponse::new(StatusCode::OK, output),
            Err(error) => HttpResponse::new(
                StatusCode::BAD_REQUEST,
                format!("Invalid result: {}", error),
            ),
        },
        Err(errors) => HttpResponse::new(
            StatusCode::INTERNAL_SERVER_ERROR,
            format!(
                "{}",
                errors
                    .iter()
                    .map(|error| format!("Error: {}", error))
                    .collect::<Vec<_>>()
                    .join("\n")
            ),
        ),
    }
}

fn custom_signal_handler(
    signal_type: &str,
    args: Option<&Vec<ValueTerm>>,
) -> Result<SignalResult, String> {
    match signal_type {
        "fetch" => handle_fetch_signal(args),
        _ => Err(format!("Unhandled signal: {}", signal_type)),
    }
}

fn handle_fetch_signal(args: Option<&Vec<ValueTerm>>) -> Result<SignalResult, String> {
    let url = match args {
        Some(args) if args.len() == 1 => args.into_iter().next().and_then(parse_string_arg),
        _ => None,
    };
    match url {
        Some(url) => Ok((
            Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Pending,
                Vec::new(),
            )))),
            Some(Box::pin(async move {
                match fetch_url(url).await {
                    Ok(data) => Expression::new(Term::Value(ValueTerm::String(data))),
                    Err(error) => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                        SignalType::Error,
                        vec![ValueTerm::String(error)],
                    )))),
                }
            })),
        )),
        None => Err(String::from("Invalid fetch arguments")),
    }
}

async fn fetch_url(url: String) -> Result<String, String> {
    let client = hyper::Client::new();
    match url.parse::<Uri>() {
        Ok(url) => match client.get(url).await {
            Ok(result) => match hyper::body::to_bytes(result.into_body()).await {
                Ok(body) => match String::from_utf8(body.into_iter().collect()) {
                    Ok(data) => Ok(data),
                    Err(error) => Err(format!("{}", error)),
                },
                Err(error) => Err(format!("{}", error)),
            },
            Err(error) => Err(format!("{}", error)),
        },
        Err(error) => Err(format!("{}", error)),
    }
}

fn parse_string_arg(value: &ValueTerm) -> Option<String> {
    match value {
        ValueTerm::String(value) => Some(String::from(value)),
        _ => None,
    }
}

fn parse_error_message(args: Option<&Vec<ValueTerm>>) -> String {
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
