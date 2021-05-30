// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use futures::{SinkExt, StreamExt};
use hyper::{
    header::{self, HeaderValue},
    upgrade::Upgraded,
    Body, Request, Response,
};
use hyper_tungstenite::{
    tungstenite::{error::ProtocolError, Message},
    WebSocketStream,
};
use reflex::{core::Expression, serialize};
use reflex_graphql::{
    parse,
    subscriptions::{
        deserialize_graphql_client_message, GraphQlSubscriptionClientMessage,
        GraphQlSubscriptionServerMessage, SubscriptionId,
    },
    wrap_graphql_error_response, wrap_graphql_success_response, QueryTransform,
};
use reflex_runtime::Runtime;
use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};
use tokio::sync::mpsc;

pub(crate) async fn handle_graphql_ws_request(
    req: Request<Body>,
    store: Arc<Runtime>,
    root: Expression,
) -> Result<Response<Body>, ProtocolError> {
    let (mut response, websocket) = hyper_tungstenite::upgrade(req, None)?;
    tokio::spawn(async move {
        match websocket.await {
            Err(error) => {
                eprintln!("Websocket connection error: {}", error);
            }
            Ok(websocket) => match handle_websocket_connection(websocket, store, root).await {
                Ok(_) => {}
                Err(error) => {
                    eprintln!("Websocket error: {}", error);
                }
            },
        }
    });
    response.headers_mut().insert(
        header::SEC_WEBSOCKET_PROTOCOL,
        HeaderValue::from_static("graphql-ws"),
    );
    Ok(response)
}

async fn handle_websocket_connection(
    websocket: WebSocketStream<Upgraded>,
    store: Arc<Runtime>,
    root: Expression,
) -> Result<(), Box<dyn std::error::Error>> {
    let output_buffer_size = 32;
    let (messages_tx, mut messages_rx) =
        mpsc::channel::<GraphQlSubscriptionServerMessage>(output_buffer_size);
    let mut subscriptions =
        Mutex::new(HashMap::<SubscriptionId, reflex_runtime::SubscriptionId>::new());
    let (mut websocket_output, mut websocket_input) = websocket.split();
    let output = tokio::spawn(async move {
        while let Some(message) = messages_rx.recv().await {
            match message.serialize() {
                Err(error) => {
                    match GraphQlSubscriptionServerMessage::ConnectionError(error).serialize() {
                        Ok(message) => {
                            let _ = websocket_output.send(Message::Text(message)).await;
                        }
                        Err(_) => {}
                    }
                }
                Ok(message) => {
                    let _ = websocket_output.send(Message::Text(message)).await;
                }
            }
        }
    });
    while let Some(message) = websocket_input.next().await {
        let messages_tx = messages_tx.clone();
        let message = match message? {
            Message::Text(data) => deserialize_graphql_client_message(&data).map(Some),
            Message::Close(_) => Ok(Some(GraphQlSubscriptionClientMessage::ConnectionTerminate)),
            Message::Binary(_) => Err(String::from("Unsupported message encoding")),
            Message::Ping(_) | Message::Pong(_) => Ok(None),
        };
        match message {
            Ok(Some(message)) => match message {
                GraphQlSubscriptionClientMessage::ConnectionInit => {
                    let _ = messages_tx
                        .send(GraphQlSubscriptionServerMessage::ConnectionAck)
                        .await;
                }
                GraphQlSubscriptionClientMessage::Start(message) => {
                    match parse(message.query(), &root) {
                        Err(error) => {
                            let _ = messages_tx
                                .send(GraphQlSubscriptionServerMessage::ConnectionError(format!(
                                    "Invalid query: {}",
                                    error
                                )))
                                .await;
                        }
                        Ok((expression, transform)) => {
                            let subscription_id = message.subscription_id();
                            if subscriptions
                                .get_mut()
                                .unwrap()
                                .contains_key(subscription_id)
                            {
                                let _ = messages_tx
                                    .send(GraphQlSubscriptionServerMessage::ConnectionError(
                                        format!(
                                            "Subscription ID already exists: {}",
                                            subscription_id
                                        ),
                                    ))
                                    .await;
                            } else {
                                match store.start_subscription(expression).await {
                                    Err(error) => {
                                        let _ = messages_tx
                                            .send(GraphQlSubscriptionServerMessage::Error(
                                                subscription_id.clone(),
                                                format!("Subscription error: {}", error),
                                            ))
                                            .await;
                                    }
                                    Ok((store_id, initial_result)) => {
                                        subscriptions
                                            .get_mut()
                                            .unwrap()
                                            .insert(subscription_id.clone(), store_id);
                                        let initial_result = match initial_result {
                                            None => None,
                                            Some(initial_result) => Some(match initial_result {
                                                Err(errors) => Err(errors),
                                                Ok(result) => {
                                                    match parse_query_result(&result, &transform) {
                                                        Ok(result) => Ok(result),
                                                        Err(error) => Err(vec![error]),
                                                    }
                                                }
                                            }),
                                        };
                                        if let Some(initial_value) = initial_result {
                                            let _ = messages_tx
                                                .send(format_subscription_result_message(
                                                    message.subscription_id(),
                                                    initial_value,
                                                ))
                                                .await;
                                        }
                                        let store = Arc::clone(&store);
                                        // TODO: Dispose subscription threads
                                        tokio::spawn(async move {
                                            while let Some(result) =
                                                store.watch_subscription(store_id).next().await
                                            {
                                                let result = result.and_then(|result| {
                                                    match parse_query_result(&result, &transform) {
                                                        Ok(result) => Ok(result),
                                                        Err(error) => Err(vec![error]),
                                                    }
                                                });
                                                let _ = messages_tx
                                                    .send(format_subscription_result_message(
                                                        message.subscription_id(),
                                                        result,
                                                    ))
                                                    .await;
                                            }
                                        });
                                    }
                                }
                            }
                        }
                    }
                }
                GraphQlSubscriptionClientMessage::Stop(message) => {
                    let subscription_id = message.subscription_id();
                    match subscriptions.get_mut().unwrap().remove(subscription_id) {
                        None => {
                            let _ = messages_tx
                                .send(GraphQlSubscriptionServerMessage::ConnectionError(format!(
                                    "Subscription ID not found: {}",
                                    subscription_id
                                )))
                                .await;
                        }
                        Some(store_id) => match store.stop_subscription(store_id).await {
                            Ok(result) => {
                                if result {
                                    let _ = messages_tx
                                        .send(GraphQlSubscriptionServerMessage::Complete(
                                            subscription_id.clone(),
                                        ))
                                        .await;
                                } else {
                                    let _ = messages_tx
                                        .send(GraphQlSubscriptionServerMessage::ConnectionError(
                                            format!(
                                                "Subscription ID already unsubscribed: {}",
                                                subscription_id
                                            ),
                                        ))
                                        .await;
                                }
                            }
                            Err(error) => {
                                subscriptions
                                    .get_mut()
                                    .unwrap()
                                    .insert(subscription_id.clone(), store_id);
                                let _ = messages_tx
                                    .send(GraphQlSubscriptionServerMessage::ConnectionError(
                                        format!("Unsubscribe error: {}", error),
                                    ))
                                    .await;
                            }
                        },
                    }
                }
                GraphQlSubscriptionClientMessage::ConnectionTerminate => {
                    let store_ids = subscriptions
                        .get_mut()
                        .unwrap()
                        .drain()
                        .map(|(_, store_id)| store_id);
                    for id in store_ids {
                        let _ = store.stop_subscription(id).await;
                    }
                }
            },
            Err(error) => {
                let _ = messages_tx.send(GraphQlSubscriptionServerMessage::ConnectionError(
                    format!("Invalid message: {}", error),
                ));
            }
            Ok(None) => {}
        }
    }
    output.abort();
    Ok(())
}

fn parse_query_result(
    result: &Expression,
    transform: &QueryTransform,
) -> Result<Expression, String> {
    match serialize(result.value()) {
        Ok(value) => transform(&value),
        _ => Err(format!("Invalid result type: {}", result)),
    }
}

fn format_subscription_result_message(
    subscription_id: &SubscriptionId,
    value: Result<Expression, Vec<String>>,
) -> GraphQlSubscriptionServerMessage {
    let result = match value {
        Err(errors) => Err(errors),
        Ok(result) => match serialize(result.value()) {
            Ok(result) => Ok(result),
            Err(error) => Err(vec![error]),
        },
    };
    let payload = match result {
        Ok(data) => wrap_graphql_success_response(data),
        Err(errors) => wrap_graphql_error_response(errors),
    };
    GraphQlSubscriptionServerMessage::Data(subscription_id.clone(), payload)
}
