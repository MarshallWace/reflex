// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use futures_util::{
    sink::{Sink, SinkExt},
    StreamExt,
};
use reflex::serialize::SerializedTerm;
use tokio::io::{AsyncRead, AsyncWrite};
use tokio_stream::Stream;

use reflex_graphql::{
    subscriptions::{
        deserialize_graphql_server_message, GraphQlSubscriptionClientMessage,
        GraphQlSubscriptionServerMessage, SubscriptionId,
    },
    GraphQlOperationPayload,
};
use tokio_tungstenite::{
    tungstenite::{handshake::client::Request, Error, Message},
    WebSocketStream,
};

pub(crate) async fn create_websocket_connection(
    url: &str,
) -> Result<WebSocketStream<impl AsyncRead + AsyncWrite + Unpin>, String> {
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

pub(crate) async fn subscribe_websocket_operation<T>(
    socket: T,
    subscription_id: SubscriptionId,
    operation: GraphQlOperationPayload,
) -> Result<impl Stream<Item = Result<SerializedTerm, String>>, String>
where
    T: Stream<Item = Result<Message, Error>> + Sink<Message, Error = Error>,
{
    let (mut write, read) = socket.split();
    let message = GraphQlSubscriptionClientMessage::start(subscription_id.clone(), operation);
    match message.serialize() {
        Err(error) => Err(format!("GraphQL serialization failed: {}", error)),
        Ok(message) => match write.send(Message::Text(message)).await {
            Err(error) => Err(format!(
                "Failed to initiate GraphQL subscription: {}",
                error
            )),
            Ok(_) => Ok(read.filter_map(move |message| {
                let result = match message {
                    Err(error) => Err(format!("{}", error)),
                    Ok(message) => {
                        let message = match message {
                            Message::Text(message) => Ok(Some(message)),
                            Message::Binary(data) => match String::from_utf8(data) {
                                Ok(message) => Ok(Some(message)),
                                Err(_) => Err(String::from("Invalid WebSocket message format")),
                            },
                            // TODO: Terminate GraphQL WebSocket stream when close message received
                            Message::Close(_) | Message::Ping(_) | Message::Pong(_) => Ok(None),
                        };
                        match message {
                            Err(error) => Err(error),
                            Ok(None) => Ok(None),
                            Ok(Some(message)) => {
                                match deserialize_graphql_server_message(&message) {
                                    Err(error) => Err(error),
                                    Ok(message) => match message {
                                        GraphQlSubscriptionServerMessage::ConnectionError(
                                            error,
                                        ) => Err(error),
                                        GraphQlSubscriptionServerMessage::Error(id, error)
                                            if id == subscription_id =>
                                        {
                                            Err(error)
                                        }
                                        GraphQlSubscriptionServerMessage::Data(id, result)
                                            if id == subscription_id =>
                                        {
                                            Ok(Some(result))
                                        }
                                        _ => Ok(None),
                                    },
                                }
                            }
                        }
                    }
                };
                async move {
                    match result {
                        Err(error) => Some(Err(error)),
                        Ok(Some(result)) => Some(Ok(result)),
                        Ok(None) => None,
                    }
                }
            })),
        },
    }
}
