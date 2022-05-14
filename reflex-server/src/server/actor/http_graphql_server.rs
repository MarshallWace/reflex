// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{hash_map::Entry, HashMap},
    iter::once,
    marker::PhantomData,
    string::FromUtf8Error,
};

use bytes::Bytes;
use http::{
    header::{self, HeaderName, HeaderValue, InvalidHeaderValue},
    HeaderMap, Request, StatusCode,
};
use metrics::{
    decrement_gauge, describe_counter, describe_gauge, increment_counter, increment_gauge, Unit,
};
use reflex::{
    core::{Expression, ExpressionFactory, Uuid},
    hash::HashId,
};
use reflex_dispatcher::{
    Action, Actor, ActorTransition, HandlerContext, InboundAction, MessageData, OutboundAction,
    StateOperation, StateTransition,
};
use reflex_graphql::{
    create_graphql_error_response, create_graphql_success_response, deserialize_graphql_operation,
    serialize_graphql_result_payload, GraphQlOperationPayload, GraphQlQuery,
};
use reflex_json::{json_object, JsonValue};

use crate::server::{
    action::{
        graphql_server::{
            GraphQlServerEmitAction, GraphQlServerParseErrorAction, GraphQlServerSubscribeAction,
            GraphQlServerUnsubscribeAction,
        },
        http_server::{HttpServerRequestAction, HttpServerResponseAction},
    },
    utils::{create_http_response, create_json_http_response},
};

#[derive(Clone, Copy, Debug)]
pub struct HttpGraphQlServerMetricNames {
    pub graphql_http_total_request_count: &'static str,
    pub graphql_http_active_request_count: &'static str,
}
impl HttpGraphQlServerMetricNames {
    fn init(self) -> Self {
        describe_counter!(
            self.graphql_http_total_request_count,
            Unit::Count,
            "Total GraphQL HTTP request count"
        );
        describe_gauge!(
            self.graphql_http_active_request_count,
            Unit::Count,
            "Active GraphQL HTTP request count"
        );
        self
    }
}
impl Default for HttpGraphQlServerMetricNames {
    fn default() -> Self {
        Self {
            graphql_http_total_request_count: "graphql_http_total_request_count",
            graphql_http_active_request_count: "graphql_http_active_request_count",
        }
    }
}

pub trait HttpGraphQlServerQueryTransform {
    fn transform(
        &self,
        operation: GraphQlOperationPayload,
        request: &Request<Bytes>,
    ) -> Result<GraphQlOperationPayload, (StatusCode, String)>;
}
impl<T> HttpGraphQlServerQueryTransform for T
where
    T: Fn(
        GraphQlOperationPayload,
        &Request<Bytes>,
    ) -> Result<GraphQlOperationPayload, (StatusCode, String)>,
{
    fn transform(
        &self,
        operation: GraphQlOperationPayload,
        request: &Request<Bytes>,
    ) -> Result<GraphQlOperationPayload, (StatusCode, String)> {
        self(operation, request)
    }
}

pub struct NoopHttpGraphQlServerQueryTransform;
impl HttpGraphQlServerQueryTransform for NoopHttpGraphQlServerQueryTransform {
    fn transform(
        &self,
        operation: GraphQlOperationPayload,
        _request: &Request<Bytes>,
    ) -> Result<GraphQlOperationPayload, (StatusCode, String)> {
        Ok(operation)
    }
}

pub trait HttpGraphQlServerAction<T: Expression>:
    Action
    + InboundAction<HttpServerRequestAction>
    + InboundAction<GraphQlServerParseErrorAction<T>>
    + InboundAction<GraphQlServerEmitAction<T>>
    + OutboundAction<GraphQlServerSubscribeAction<T>>
    + OutboundAction<GraphQlServerUnsubscribeAction<T>>
    + OutboundAction<HttpServerResponseAction>
{
}
impl<T: Expression, TAction> HttpGraphQlServerAction<T> for TAction where
    Self: Action
        + InboundAction<HttpServerRequestAction>
        + InboundAction<GraphQlServerParseErrorAction<T>>
        + InboundAction<GraphQlServerEmitAction<T>>
        + OutboundAction<GraphQlServerSubscribeAction<T>>
        + OutboundAction<GraphQlServerUnsubscribeAction<T>>
        + OutboundAction<HttpServerResponseAction>
{
}

pub(crate) struct HttpGraphQlServer<T, TFactory, TTransform, TQueryMetricLabels>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TTransform: HttpGraphQlServerQueryTransform,
    TQueryMetricLabels: Fn(&GraphQlOperationPayload, &HeaderMap) -> Vec<(String, String)>,
{
    factory: TFactory,
    transform: TTransform,
    metric_names: HttpGraphQlServerMetricNames,
    get_query_metric_labels: TQueryMetricLabels,
    _expression: PhantomData<T>,
}
impl<T, TFactory, TTransform, TQueryMetricLabels>
    HttpGraphQlServer<T, TFactory, TTransform, TQueryMetricLabels>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TTransform: HttpGraphQlServerQueryTransform,
    TQueryMetricLabels: Fn(&GraphQlOperationPayload, &HeaderMap) -> Vec<(String, String)>,
{
    pub(crate) fn new(
        factory: TFactory,
        transform: TTransform,
        metric_names: HttpGraphQlServerMetricNames,
        get_query_metric_labels: TQueryMetricLabels,
    ) -> Self {
        Self {
            factory,
            transform,
            metric_names: metric_names.init(),
            get_query_metric_labels,
            _expression: Default::default(),
        }
    }
}

#[derive(Default)]
pub struct HttpGraphQlServerState {
    requests: HashMap<Uuid, HttpGraphQlRequest>,
}
struct HttpGraphQlRequest {
    etag: Option<String>,
    metric_labels: Vec<(String, String)>,
}

impl<T, TFactory, TTransform, TQueryMetricLabels, TAction> Actor<TAction>
    for HttpGraphQlServer<T, TFactory, TTransform, TQueryMetricLabels>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TTransform: HttpGraphQlServerQueryTransform,
    TQueryMetricLabels: Fn(&GraphQlOperationPayload, &HeaderMap) -> Vec<(String, String)>,
    TAction: HttpGraphQlServerAction<T>,
{
    type State = HttpGraphQlServerState;
    fn init(&self) -> Self::State {
        Default::default()
    }
    fn handle(
        &self,
        state: Self::State,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> ActorTransition<Self::State, TAction> {
        let mut state = state;
        let actions = if let Some(action) = action.match_type() {
            self.handle_http_server_request(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_graphql_parse_error(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_graphql_emit(&mut state, action, metadata, context)
        } else {
            None
        }
        .unwrap_or_default();
        ActorTransition::new(state, actions)
    }
}
impl<T, TFactory, TTransform, TQueryMetricLabels>
    HttpGraphQlServer<T, TFactory, TTransform, TQueryMetricLabels>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TTransform: HttpGraphQlServerQueryTransform,
    TQueryMetricLabels: Fn(&GraphQlOperationPayload, &HeaderMap) -> Vec<(String, String)>,
{
    fn handle_http_server_request<TAction>(
        &self,
        state: &mut HttpGraphQlServerState,
        action: &HttpServerRequestAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action
            + OutboundAction<HttpServerResponseAction>
            + OutboundAction<GraphQlServerSubscribeAction<T>>,
    {
        let HttpServerRequestAction {
            request_id,
            request,
        } = action;
        let request_id = *request_id;
        let entry = match state.requests.entry(request_id) {
            Entry::Occupied(_) => None,
            Entry::Vacant(entry) => Some(entry),
        }?;
        let operation = match parse_graphql_request(request) {
            Err(message) => Err((StatusCode::BAD_REQUEST, message)),
            Ok(operation) => self.transform.transform(operation, request),
        };
        match operation {
            Err((status_code, message)) => Some(StateTransition::new(once(StateOperation::Send(
                context.pid(),
                HttpServerResponseAction {
                    request_id,
                    response: create_json_http_response(
                        status_code,
                        None,
                        &JsonValue::from(message),
                    ),
                }
                .into(),
            )))),
            Ok(operation) => {
                let metric_labels = (self.get_query_metric_labels)(&operation, request.headers());
                increment_counter!(
                    self.metric_names.graphql_http_total_request_count,
                    &metric_labels,
                );
                increment_gauge!(
                    self.metric_names.graphql_http_active_request_count,
                    1.0,
                    &metric_labels,
                );
                entry.insert(HttpGraphQlRequest {
                    etag: parse_request_etag(&request),
                    metric_labels,
                });
                Some(StateTransition::new(once(StateOperation::Send(
                    context.pid(),
                    GraphQlServerSubscribeAction {
                        subscription_id: request_id,
                        operation,
                        _expression: Default::default(),
                    }
                    .into(),
                ))))
            }
        }
    }
    fn handle_graphql_parse_error<TAction>(
        &self,
        state: &mut HttpGraphQlServerState,
        action: &GraphQlServerParseErrorAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + OutboundAction<HttpServerResponseAction>,
    {
        let GraphQlServerParseErrorAction {
            subscription_id,
            message,
            operation,
            ..
        } = action;
        let request = state.requests.remove(subscription_id)?;
        let HttpGraphQlRequest { metric_labels, .. } = request;
        decrement_gauge!(
            self.metric_names.graphql_http_active_request_count,
            1.0,
            &metric_labels
        );
        let response = create_json_http_response(
            StatusCode::BAD_REQUEST,
            None,
            &create_graphql_error_response(Some(json_object([
                (
                    String::from("message"),
                    JsonValue::String(String::from(message)),
                ),
                (String::from("operation"), operation.clone().into_json()),
            ]))),
        );
        Some(StateTransition::new(once(StateOperation::Send(
            context.pid(),
            HttpServerResponseAction {
                request_id: *subscription_id,
                response,
            }
            .into(),
        ))))
    }
    fn handle_graphql_emit<TAction>(
        &self,
        state: &mut HttpGraphQlServerState,
        action: &GraphQlServerEmitAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action
            + OutboundAction<GraphQlServerUnsubscribeAction<T>>
            + OutboundAction<HttpServerResponseAction>,
    {
        let GraphQlServerEmitAction {
            subscription_id,
            result,
        } = action;
        let request = state.requests.remove(subscription_id)?;
        let HttpGraphQlRequest {
            metric_labels,
            etag,
        } = request;
        decrement_gauge!(
            self.metric_names.graphql_http_active_request_count,
            1.0,
            &metric_labels
        );
        let response_etag = format_response_etag(result.id());
        let response = match etag {
            Some(request_etag) if request_etag == response_etag => {
                create_http_response(StatusCode::NOT_MODIFIED, None, None)
            }
            _ => {
                let payload = serialize_graphql_result_payload(result, &self.factory);
                create_json_http_response(
                    StatusCode::OK,
                    create_etag_header(&response_etag).ok(),
                    &match payload {
                        Ok(payload) => create_graphql_success_response(payload),
                        Err(errors) => create_graphql_error_response(errors),
                    },
                )
            }
        };
        Some(StateTransition::new([
            StateOperation::Send(
                context.pid(),
                GraphQlServerUnsubscribeAction {
                    subscription_id: *subscription_id,
                    _expression: Default::default(),
                }
                .into(),
            ),
            StateOperation::Send(
                context.pid(),
                HttpServerResponseAction {
                    request_id: *subscription_id,
                    response,
                }
                .into(),
            ),
        ]))
    }
}

fn parse_graphql_request(request: &Request<Bytes>) -> Result<GraphQlOperationPayload, String> {
    let content_type_header = request
        .headers()
        .get(header::CONTENT_TYPE)
        .ok_or_else(|| String::from("Missing Content-Type header"))?;
    let content_type = content_type_header
        .to_str()
        .map_err(|_| String::from("Invalid Content-Type header"))?;
    match content_type {
        "application/graphql" => parse_plain_graphql_request_body(
            request.body(),
            request
                .headers()
                .get(HeaderName::from_static("x-graphql-operation-name"))
                .and_then(|value| value.to_str().ok().map(String::from)),
        ),
        "application/json" => parse_json_graphql_request_body(request.body()),
        _ => Err(String::from("Unsupported Content-Type header")),
    }
}

fn parse_plain_graphql_request_body(
    body: &Bytes,
    operation_name: Option<String>,
) -> Result<GraphQlOperationPayload, String> {
    let body = parse_request_body(body).map_err(|_| String::from("Invalid request body"))?;
    Ok(GraphQlOperationPayload::new(
        GraphQlQuery::Source(body),
        operation_name,
        None,
        None,
    ))
}

fn parse_json_graphql_request_body(body: &Bytes) -> Result<GraphQlOperationPayload, String> {
    let body = parse_request_body(body).map_err(|_| String::from("Invalid request body"))?;
    match deserialize_graphql_operation(&body) {
        Err(error) => Err(error),
        Ok(operation) => match operation.operation_name() {
            Some("IntrospectionQuery") => {
                Err(String::from("Introspection query not yet implemented"))
            }
            _ => Ok(operation),
        },
    }
}

fn parse_request_etag(req: &Request<Bytes>) -> Option<String> {
    req.headers()
        .get(header::IF_NONE_MATCH)
        .and_then(|header| header.to_str().map(|value| String::from(value)).ok())
}

fn format_response_etag(hash: HashId) -> String {
    format!("\"{:x}\"", hash)
}

fn parse_request_body(body: &Bytes) -> Result<String, FromUtf8Error> {
    String::from_utf8(body.iter().copied().collect())
}

fn create_etag_header(value: &str) -> Result<(HeaderName, HeaderValue), InvalidHeaderValue> {
    Ok((header::ETAG, HeaderValue::from_str(value)?))
}
