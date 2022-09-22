// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::net::SocketAddr;
use std::time::Duration;

use futures_util::stream::{SplitSink, SplitStream};
use futures_util::{SinkExt, StreamExt};
use reflex_graphql::subscriptions::{
    deserialize_graphql_server_message, GraphQlSubscriptionClientMessage,
    GraphQlSubscriptionServerMessage,
};
use reflex_graphql::{GraphQlExtensions, GraphQlOperationPayload};
use serde_json::{Map, Value};
use tokio::net::TcpStream;
use tokio_tungstenite::tungstenite::Message;
use tokio_tungstenite::{connect_async, MaybeTlsStream, WebSocketStream};

pub struct GraphQlConnection {
    write: SplitSink<WebSocketStream<MaybeTlsStream<TcpStream>>, Message>,
    read: SplitStream<WebSocketStream<MaybeTlsStream<TcpStream>>>,
}
impl GraphQlConnection {
    pub async fn new(addr: &SocketAddr) -> Self {
        let url = url::Url::parse(&format!("ws://{}", addr)).unwrap();
        let (ws_stream, _) = connect_async(url).await.expect("Unable to connect");

        let (mut write, mut read) = ws_stream.split();
        let connect_ack = send_client_message_and_receive(
            &mut write,
            &mut read,
            GraphQlSubscriptionClientMessage::connection_init(None),
        )
        .await;
        assert_eq!(connect_ack, GraphQlSubscriptionServerMessage::ConnectionAck);
        Self { write, read }
    }

    pub async fn send_client_message_and_receive(
        &mut self,
        message: GraphQlSubscriptionClientMessage,
    ) -> GraphQlSubscriptionServerMessage {
        send_client_message_and_receive(&mut self.write, &mut self.read, message).await
    }

    pub fn start_query_message(
        id: i32,
        query: &str,
        variables: Option<Value>,
    ) -> GraphQlSubscriptionClientMessage {
        GraphQlSubscriptionClientMessage::start(
            id.to_string(),
            GraphQlOperationPayload {
                query: query.to_string(),
                variables: variables
                    .map(|value| value.as_object().unwrap().clone())
                    .unwrap_or(Map::new()),
                operation_name: None,
                extensions: GraphQlExtensions::default(),
            },
        )
    }

    pub fn update_query_message(id: i32, variables: Value) -> GraphQlSubscriptionClientMessage {
        GraphQlSubscriptionClientMessage::update(
            id.to_string(),
            variables.as_object().unwrap().clone(),
        )
    }

    pub fn data_message(id: i32, data: Value) -> GraphQlSubscriptionServerMessage {
        GraphQlSubscriptionServerMessage::Data(id.to_string(), data)
    }
}

async fn send_and_get_response_with_timeout(
    write: &mut SplitSink<WebSocketStream<MaybeTlsStream<TcpStream>>, Message>,
    read: &mut SplitStream<WebSocketStream<MaybeTlsStream<TcpStream>>>,
    message: &str,
    timeout: Duration,
) -> String {
    write
        .send(Message::Text(message.to_string()))
        .await
        .unwrap();

    tokio::time::timeout(timeout, read.next())
        .await
        .unwrap()
        .unwrap()
        .unwrap()
        .to_string()
}

async fn send_client_message_and_receive(
    write: &mut SplitSink<WebSocketStream<MaybeTlsStream<TcpStream>>, Message>,
    read: &mut SplitStream<WebSocketStream<MaybeTlsStream<TcpStream>>>,
    message: GraphQlSubscriptionClientMessage,
) -> GraphQlSubscriptionServerMessage {
    let message = serde_json::to_string(&message.into_json()).unwrap();
    let response =
        send_and_get_response_with_timeout(write, read, &message, Duration::from_millis(10 * 1000))
            .await;
    deserialize_graphql_server_message(&response).unwrap()
}
