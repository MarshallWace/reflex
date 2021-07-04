// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use futures::{stream, SinkExt, Stream, StreamExt};
use hyper::{
    header::{self, HeaderValue},
    upgrade::Upgraded,
    Body, Request, Response,
};
use hyper_tungstenite::{
    tungstenite::{error::ProtocolError, Message},
    WebSocketStream,
};
use reflex::{
    core::Expression,
    serialize::{serialize, SerializedListTerm, SerializedObjectTerm, SerializedTerm},
    stdlib::value::ValueTerm,
};
use reflex_graphql::{
    parse,
    subscriptions::{
        deserialize_graphql_client_message, GraphQlSubscriptionClientMessage,
        GraphQlSubscriptionServerMessage, GraphQlSubscriptionStartMessage, SubscriptionId,
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
                    let variables = message
                        .variables()
                        .entries()
                        .iter()
                        .map(|(key, value)| (key.as_str(), value.deserialize()));
                    match parse(message.query(), variables, &root) {
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

                                        let store = Arc::clone(&store);
                                        let results = stream::iter(initial_result.into_iter())
                                            .chain(store.watch_subscription(store_id))
                                            .map(move |result| {
                                                result.and_then(|result| {
                                                    match parse_query_result(&result, &transform) {
                                                        Ok(result) => Ok(result),
                                                        Err(error) => Err(vec![error]),
                                                    }
                                                })
                                            });
                                        let should_diff = is_diff_subscription(&message);
                                        let mut results = match should_diff {
                                            false => results
                                                .map(|result| result.map(|value| (value, false)))
                                                .left_stream(),
                                            true => create_diff_stream(results).right_stream(),
                                        };
                                        // TODO: Dispose subscription threads
                                        tokio::spawn(async move {
                                            while let Some(result) = results.next().await {
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
) -> Result<SerializedTerm, String> {
    match serialize(result.value()) {
        Ok(value) => transform(&value).and_then(|result| serialize(result.value())),
        _ => Err(format!("Invalid result type: {}", result)),
    }
}

fn is_diff_subscription(message: &GraphQlSubscriptionStartMessage) -> bool {
    find_serialized_object_property(message.extensions(), "diff")
        .map(|value| match value {
            SerializedTerm::Value(ValueTerm::Boolean(value)) => *value == true,
            _ => false,
        })
        .unwrap_or(false)
}

fn find_serialized_object_property<'a>(
    value: &'a SerializedObjectTerm,
    key: &str,
) -> Option<&'a SerializedTerm> {
    value.entries().iter().find_map(|(existing_key, value)| {
        if key == existing_key {
            Some(value)
        } else {
            None
        }
    })
}

fn create_diff_stream(
    results: impl Stream<Item = Result<SerializedTerm, Vec<String>>>,
) -> impl Stream<Item = Result<(SerializedTerm, bool), Vec<String>>> {
    results
        .scan(None, |previous, value| {
            let result = match (&previous, &value) {
                (Some(previous_value), Ok(current_value)) => {
                    let diff = diff_results(previous_value, current_value);
                    *previous = value.ok();
                    Ok(diff.map(|value| (value, true)))
                }
                _ => {
                    *previous = value.as_ref().ok().cloned();
                    value.map(|value| Some((value, false)))
                }
            };
            Box::pin(async { Some(result) })
        })
        .filter_map(|result| {
            let result = match result {
                Ok(None) => None,
                Ok(Some(result)) => Some(Ok(result)),
                Err(error) => Some(Err(error)),
            };
            Box::pin(async { result })
        })
}

fn diff_results(previous: &SerializedTerm, current: &SerializedTerm) -> Option<SerializedTerm> {
    if current == previous {
        return None;
    }
    match previous {
        SerializedTerm::Value(_) => Some(current.clone()),
        SerializedTerm::Object(previous) => match current {
            SerializedTerm::Object(current) => diff_objects(previous, current),
            _ => Some(current.clone()),
        },
        SerializedTerm::List(previous) => match current {
            SerializedTerm::List(current) => diff_lists(previous, current),
            _ => Some(current.clone()),
        },
    }
}

fn diff_objects(
    previous: &SerializedObjectTerm,
    current: &SerializedObjectTerm,
) -> Option<SerializedTerm> {
    let previous_entries = previous
        .entries()
        .iter()
        .map(|(key, value)| (key, value))
        .collect::<HashMap<_, _>>();
    let updates =
        SerializedObjectTerm::new(current.entries().iter().filter_map(|(key, current_value)| {
            previous_entries.get(key).and_then(|previous_value| {
                diff_results(previous_value, current_value).map(|value| (key.clone(), value))
            })
        }));
    if updates.entries().is_empty() {
        None
    } else {
        Some(SerializedTerm::Object(updates))
    }
}

fn diff_lists(
    previous: &SerializedListTerm,
    current: &SerializedListTerm,
) -> Option<SerializedTerm> {
    let updates = current
        .items()
        .iter()
        .zip(previous.items().iter())
        .map(|(current, previous)| diff_results(previous, current))
        .chain(
            current
                .items()
                .iter()
                .skip(previous.items().len())
                .map(|item| Some(item.clone())),
        )
        .collect::<Vec<_>>();
    let updates = SerializedObjectTerm::new(
        updates
            .into_iter()
            .enumerate()
            .filter_map(|(index, item)| item.map(|value| (index.to_string(), value))),
    );
    if updates.entries().is_empty() {
        None
    } else {
        Some(SerializedTerm::Object(updates))
    }
}

fn format_subscription_result_message(
    subscription_id: &SubscriptionId,
    result: Result<(SerializedTerm, bool), Vec<String>>,
) -> GraphQlSubscriptionServerMessage {
    match result {
        Ok((data, is_diff)) => {
            let payload = wrap_graphql_success_response(data);
            if is_diff {
                GraphQlSubscriptionServerMessage::Patch(subscription_id.clone(), payload)
            } else {
                GraphQlSubscriptionServerMessage::Data(subscription_id.clone(), payload)
            }
        }
        Err(errors) => {
            let payload = wrap_graphql_error_response(errors);
            GraphQlSubscriptionServerMessage::Data(subscription_id.clone(), payload)
        }
    }
}
