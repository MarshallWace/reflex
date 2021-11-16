// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{hash_map::Entry, HashMap},
    iter::once,
    sync::Mutex,
};

use futures_util::{
    future,
    sink::{Sink, SinkExt},
    stream, Future, FutureExt, StreamExt,
};
use reflex_json::{json_object, JsonValue};
use tokio::{
    net::TcpStream,
    sync::{broadcast, mpsc, oneshot, watch},
};
use tokio_stream::{
    wrappers::{BroadcastStream, WatchStream},
    Stream,
};

use reflex_graphql::{
    subscriptions::{
        deserialize_graphql_server_message, GraphQlSubscriptionClientMessage,
        GraphQlSubscriptionServerMessage, SubscriptionId,
    },
    GraphQlOperationPayload,
};
use tokio_tungstenite::{
    tungstenite::{handshake::client::Request, Error, Message},
    MaybeTlsStream, WebSocketStream,
};
use uuid::Uuid;

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

type ConnectionUrl = String;

pub(crate) fn create_json_error_object(message: String) -> JsonValue {
    json_object(once((String::from("message"), JsonValue::String(message))))
}

enum WebSocketConnectionManagerCommand {
    Subscribe(
        StreamOperation<
            (ConnectionUrl, GraphQlOperationPayload),
            Result<SubscriptionId, String>,
            Result<JsonValue, Vec<JsonValue>>,
        >,
    ),
    Unsubscribe(AsyncOperation<SubscriptionId, Result<(), String>>),
}
impl WebSocketConnectionManagerCommand {
    fn subscribe(
        url: ConnectionUrl,
        operation: GraphQlOperationPayload,
        response: oneshot::Sender<Result<SubscriptionId, String>>,
        updates: watch::Sender<Option<Result<JsonValue, Vec<JsonValue>>>>,
    ) -> Self {
        let payload = (url, operation);
        Self::Subscribe(StreamOperation::new(payload, response, updates))
    }
    fn unsubscribe(
        subscription_id: SubscriptionId,
        response: oneshot::Sender<Result<(), String>>,
    ) -> Self {
        let payload = subscription_id;
        Self::Unsubscribe(AsyncOperation::new(payload, response))
    }
}

pub struct WebSocketConnectionManager {
    commands: mpsc::Sender<WebSocketConnectionManagerCommand>,
}
impl Clone for WebSocketConnectionManager {
    fn clone(&self) -> Self {
        Self {
            commands: self.commands.clone(),
        }
    }
}
impl Default for WebSocketConnectionManager {
    fn default() -> Self {
        let (messages_tx, mut messages_rx) =
            mpsc::channel::<WebSocketConnectionManagerCommand>(1024);
        tokio::spawn({
            let mut connections = Mutex::new(ConnectionCache::default());
            async move {
                while let Some(command) = messages_rx.recv().await {
                    match command {
                        WebSocketConnectionManagerCommand::Subscribe(command) => {
                            let (url, operation) = command.payload;
                            let subscription_id = Uuid::new_v4().to_hyphenated().to_string();
                            match connections.get_mut().unwrap().subscribe(
                                url,
                                subscription_id.clone(),
                                operation,
                            ) {
                                Err(error) => {
                                    let _ = command.response.send(Err(error));
                                }
                                Ok(results) => {
                                    let _ = command.response.send(Ok(subscription_id));
                                    let update_tx = command.updates;
                                    let mut results = Box::pin(results);
                                    tokio::spawn(async move {
                                        while let Some(result) = results.next().await {
                                            let _ = update_tx.send(Some(result));
                                        }
                                    });
                                }
                            }
                        }
                        WebSocketConnectionManagerCommand::Unsubscribe(command) => {
                            let subscription_id = command.payload;
                            match connections.get_mut().unwrap().unsubscribe(subscription_id) {
                                Err(error) => {
                                    let _ = command.response.send(Err(error));
                                }
                                Ok(dispose) => {
                                    let response_tx = command.response;
                                    tokio::spawn(async move {
                                        let result = dispose.await;
                                        let _ = response_tx.send(result);
                                    });
                                }
                            };
                        }
                    }
                }
            }
        });
        Self {
            commands: messages_tx,
        }
    }
}
impl WebSocketConnectionManager {
    pub async fn subscribe(
        self,
        url: String,
        operation: GraphQlOperationPayload,
    ) -> Result<
        (
            SubscriptionId,
            impl Stream<Item = Result<JsonValue, Vec<JsonValue>>> + Send + 'static,
        ),
        String,
    > {
        let (response_tx, response_rx) = oneshot::channel();
        let (update_tx, update_rx) = watch::channel(None);
        let command =
            WebSocketConnectionManagerCommand::subscribe(url, operation, response_tx, update_tx);
        let _ = self.commands.send(command).await;
        match response_rx.await {
            Err(error) => Err(format!("{}", error)),
            Ok(subscription) => match subscription {
                Err(error) => Err(error),
                Ok(subscription_id) => {
                    let results =
                        WatchStream::new(update_rx).filter_map(|update| future::ready(update));
                    Ok((subscription_id, results))
                }
            },
        }
    }
    pub async fn unsubscribe(self, subscription_id: SubscriptionId) -> Result<(), String> {
        let (response_tx, response_rx) = oneshot::channel();
        let command = WebSocketConnectionManagerCommand::unsubscribe(subscription_id, response_tx);
        let _ = self.commands.send(command).await;
        match response_rx.await {
            Err(error) => Err(format!("{}", error)),
            Ok(result) => result,
        }
    }
}

#[derive(Default)]
struct ConnectionCache {
    connections: HashMap<ConnectionUrl, (WebSocketConnection, usize)>,
    subscriptions: HashMap<SubscriptionId, ConnectionUrl>,
}
impl ConnectionCache {
    fn subscribe(
        &mut self,
        url: ConnectionUrl,
        subscription_id: SubscriptionId,
        operation: GraphQlOperationPayload,
    ) -> Result<impl Stream<Item = Result<JsonValue, Vec<JsonValue>>> + Send + Sync, String> {
        match self.subscriptions.entry(subscription_id.clone()) {
            Entry::Occupied(_) => Err(format!(
                "[GraphQL] Subscription ID already exists: {}",
                subscription_id
            )),
            Entry::Vacant(entry) => {
                entry.insert(url.clone());
                Ok(())
            }
        }?;
        let connection = match self.connections.entry(url.clone()) {
            Entry::Occupied(mut entry) => {
                {
                    let (_, subscription_count) = entry.get_mut();
                    *subscription_count += 1;
                }
                let (connection, _) = entry.get();
                connection.clone()
            }
            Entry::Vacant(entry) => {
                let connection = WebSocketConnection::new(url);
                let subscription_count = 1;
                let (connection, _) = entry.insert((connection, subscription_count));
                connection.clone()
            }
        };
        Ok(connection
            .subscribe(subscription_id, operation)
            .into_stream()
            .flatten())
    }
    fn unsubscribe(
        &mut self,
        subscription_id: SubscriptionId,
    ) -> Result<impl Future<Output = Result<(), String>>, String> {
        let connection_url = match self.subscriptions.entry(subscription_id.clone()) {
            Entry::Vacant(entry) => Err(format!(
                "[GraphQL] Subscription ID not found: {}",
                entry.key(),
            )),
            Entry::Occupied(entry) => {
                let connection_url = entry.remove();
                Ok(connection_url)
            }
        }?;
        match self.connections.entry(connection_url) {
            Entry::Vacant(entry) => {
                Err(format!("[GraphQL] Connection not found: {}", entry.key(),))
            }
            Entry::Occupied(mut entry) => {
                let subscription_count = {
                    let (_, subscription_count) = entry.get();
                    *subscription_count
                };
                if subscription_count == 1 {
                    let (connection, _) = entry.remove();
                    Ok(connection.close().left_future())
                } else {
                    let (connection, subscription_count) = entry.get_mut();
                    *subscription_count -= 1;
                    Ok(connection
                        .clone()
                        .unsubscribe(subscription_id)
                        .right_future())
                }
            }
        }
    }
}

enum WebSocketConnectionCommand {
    Subscribe(AsyncOperation<(SubscriptionId, GraphQlOperationPayload), Result<(), String>>),
    Unsubscribe(AsyncOperation<SubscriptionId, Result<(), String>>),
    Close(AsyncOperation<(), Result<(), String>>),
}
impl WebSocketConnectionCommand {
    fn subscribe(
        subscription_id: SubscriptionId,
        operation: GraphQlOperationPayload,
        response: oneshot::Sender<Result<(), String>>,
    ) -> Self {
        let payload = (subscription_id, operation);
        Self::Subscribe(AsyncOperation::new(payload, response))
    }
    fn unsubscribe(
        subscription_id: SubscriptionId,
        response: oneshot::Sender<Result<(), String>>,
    ) -> Self {
        let payload = subscription_id;
        Self::Unsubscribe(AsyncOperation::new(payload, response))
    }
    fn close(response: oneshot::Sender<Result<(), String>>) -> Self {
        let payload = ();
        Self::Close(AsyncOperation::new(payload, response))
    }
}

#[derive(Clone)]
struct WebSocketConnection {
    commands: mpsc::Sender<WebSocketConnectionCommand>,
    results: broadcast::Sender<(
        Option<SubscriptionId>,
        Option<Result<JsonValue, Vec<JsonValue>>>,
    )>,
}
impl WebSocketConnection {
    fn new(url: String) -> Self {
        let (commands_tx, mut commands_rx) = mpsc::channel(1024);
        let (results_tx, _) = broadcast::channel(1024);
        tokio::spawn({
            let results_tx = results_tx.clone();
            async move {
                let connection = create_websocket_connection(&url).await;
                match connection {
                    Err(error) => {
                        while let Some(command) = commands_rx.recv().await {
                            match command {
                                WebSocketConnectionCommand::Subscribe(command) => {
                                    let _ = command.response.send(Err(error.clone()));
                                }
                                WebSocketConnectionCommand::Unsubscribe(command) => {
                                    let _ = command.response.send(Err(error.clone()));
                                }
                                WebSocketConnectionCommand::Close(command) => {
                                    let _ = command.response.send(Ok(()));
                                }
                            }
                        }
                    }
                    Ok(connection) => {
                        let (mut connection_tx, connection_rx) = connection.split();
                        tokio::spawn({
                            let mut results =
                                Box::pin(watch_websocket_subscription_results(connection_rx));
                            async move {
                                while let Some(result) = results.next().await {
                                    let _ = results_tx.send(result);
                                }
                            }
                        });
                        while let Some(command) = commands_rx.recv().await {
                            match command {
                                WebSocketConnectionCommand::Subscribe(command) => {
                                    let (subscription_id, operation) = command.payload;
                                    eprintln!(
                                        "[GraphQL] {} Start subscription {}",
                                        url, subscription_id
                                    );
                                    let message = GraphQlSubscriptionClientMessage::start(
                                        subscription_id,
                                        operation,
                                    );
                                    let result =
                                        send_websocket_message(&mut connection_tx, message).await;
                                    let _ = command.response.send(result);
                                }
                                WebSocketConnectionCommand::Unsubscribe(command) => {
                                    let subscription_id = command.payload;
                                    eprintln!(
                                        "[GraphQL] {} Stop subscription {}",
                                        url, subscription_id
                                    );
                                    let message =
                                        GraphQlSubscriptionClientMessage::stop(subscription_id);
                                    let result =
                                        send_websocket_message(&mut connection_tx, message).await;
                                    let _ = command.response.send(result);
                                }
                                WebSocketConnectionCommand::Close(command) => {
                                    eprintln!("[GraphQL] {} Terminate connection", url);
                                    let message =
                                        GraphQlSubscriptionClientMessage::connection_terminate();
                                    let _ =
                                        send_websocket_message(&mut connection_tx, message).await;
                                    let result = connection_tx.send(Message::Close(None)).await;
                                    let _ = command
                                        .response
                                        .send(result.map_err(|err| format!("{}", err)));
                                }
                            }
                        }
                    }
                }
            }
        });
        Self {
            commands: commands_tx,
            results: results_tx,
        }
    }
    async fn subscribe(
        self,
        subscription_id: SubscriptionId,
        operation: GraphQlOperationPayload,
    ) -> impl Stream<Item = Result<JsonValue, Vec<JsonValue>>> {
        let (response_tx, response_rx) = oneshot::channel();
        let _ = self
            .commands
            .send(WebSocketConnectionCommand::subscribe(
                subscription_id.clone(),
                operation,
                response_tx,
            ))
            .await;
        match response_rx.await {
            Err(error) => stream::iter(once(Err(vec![create_json_error_object(format!(
                "{}",
                error
            ))])))
            .left_stream(),
            Ok(_) => BroadcastStream::new(self.results.subscribe())
                .filter_map({
                    let subscription_id = subscription_id;
                    move |result| {
                        future::ready(match result {
                            Err(error) => Some(Some(Err(vec![create_json_error_object(format!(
                                "{}",
                                error
                            ))]))),
                            Ok((id, result)) => match id {
                                Some(id) if id != subscription_id => None,
                                _ => Some(result),
                            },
                        })
                    }
                })
                .take_while(|result| future::ready(result.is_some()))
                .filter_map(|result| future::ready(result))
                .right_stream(),
        }
    }
    async fn unsubscribe(self, subscription_id: SubscriptionId) -> Result<(), String> {
        let (response_tx, response_rx) = oneshot::channel();
        let _ = self
            .commands
            .send(WebSocketConnectionCommand::unsubscribe(
                subscription_id,
                response_tx,
            ))
            .await;
        match response_rx.await {
            Err(error) => Err(format!("{}", error)),
            Ok(result) => result,
        }
    }
    async fn close(self) -> Result<(), String> {
        let (response_tx, response_rx) = oneshot::channel();
        let _ = self
            .commands
            .send(WebSocketConnectionCommand::close(response_tx))
            .await;
        match response_rx.await {
            Err(error) => Err(format!("{}", error)),
            Ok(result) => result,
        }
    }
}

async fn create_websocket_connection(
    url: &str,
) -> Result<WebSocketStream<MaybeTlsStream<TcpStream>>, String> {
    match Request::builder()
        .header("Sec-WebSocket-Protocol", "graphql-ws")
        .uri(url)
        .body(())
    {
        Err(error) => Err(format!(
            "Failed to create WebSocket upgrade request: {}",
            error
        )),
        Ok(request) => match tokio_tungstenite::connect_async(request).await {
            Err(error) => Err(format!("WebSocket connection error: {}", error)),
            Ok((stream, _response)) => Ok(stream),
        },
    }
}

async fn send_websocket_message(
    socket: &mut (impl Sink<Message, Error = Error> + Unpin),
    message: GraphQlSubscriptionClientMessage,
) -> Result<(), String> {
    match message.into_serialized() {
        Err(error) => Err(format!("GraphQL serialization failed: {}", error)),
        Ok(message) => match socket.send(Message::Text(message)).await {
            Err(error) => Err(format!("{}", error)),
            Ok(_) => Ok(()),
        },
    }
}

fn watch_websocket_subscription_results(
    socket: impl Stream<Item = Result<Message, Error>>,
) -> impl Stream<
    Item = (
        Option<SubscriptionId>,
        Option<Result<JsonValue, Vec<JsonValue>>>,
    ),
> {
    socket.filter_map(|message| {
        future::ready(match message {
            Err(error) => Some((
                None,
                Some(Err(vec![create_json_error_object(format!("{}", error))])),
            )),
            Ok(message) => {
                let message = match message {
                    Message::Text(message) => Ok(Some(message)),
                    Message::Binary(data) => match String::from_utf8(data) {
                        Ok(message) => Ok(Some(message)),
                        Err(_) => Err(String::from("Invalid WebSocket message format")),
                    },
                    Message::Close(_) | Message::Ping(_) | Message::Pong(_) => Ok(None),
                };
                match message {
                    Err(error) => Some((None, Some(Err(vec![create_json_error_object(error)])))),
                    Ok(None) => None,
                    Ok(Some(message)) => match deserialize_graphql_server_message(&message) {
                        Err(error) => {
                            Some((None, Some(Err(vec![create_json_error_object(error)]))))
                        }
                        Ok(message) => match message {
                            GraphQlSubscriptionServerMessage::ConnectionError(error) => Some((
                                None,
                                Some(Err(match error {
                                    JsonValue::Array(errors) => errors,
                                    _ => vec![error],
                                })),
                            )),
                            GraphQlSubscriptionServerMessage::Error(subscription_id, error) => {
                                Some((
                                    Some(subscription_id),
                                    Some(Err({
                                        match error {
                                            JsonValue::Array(errors) => Ok(errors),
                                            JsonValue::Object(mut response) => {
                                                if let Some(JsonValue::Array(errors)) =
                                                    response.remove("errors")
                                                {
                                                    Ok(errors)
                                                } else {
                                                    Err(JsonValue::Object(response))
                                                }
                                            }
                                            _ => Err(error),
                                        }
                                        .unwrap_or_else(|error| vec![error])
                                    })),
                                ))
                            }
                            GraphQlSubscriptionServerMessage::Data(subscription_id, result) => {
                                Some((Some(subscription_id), Some(Ok(result))))
                            }
                            GraphQlSubscriptionServerMessage::Complete(subscription_id) => {
                                Some((Some(subscription_id), None))
                            }
                            _ => None,
                        },
                    },
                }
            }
        })
    })
}
