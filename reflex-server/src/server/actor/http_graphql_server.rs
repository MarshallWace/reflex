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
    HeaderMap, Request, Response, StatusCode,
};
use metrics::{
    decrement_gauge, describe_counter, describe_gauge, increment_counter, increment_gauge, Unit,
};
use reflex::{
    core::{Expression, ExpressionFactory, Uuid},
    hash::HashId,
};
use reflex_dispatcher::{
    Action, ActorInitContext, HandlerContext, MessageData, NoopDisposeCallback, ProcessId,
    SchedulerCommand, SchedulerMode, SchedulerTransition, TaskFactory, TaskInbox,
};
use reflex_graphql::{
    create_graphql_error_response, create_graphql_success_response, deserialize_graphql_operation,
    serialize_graphql_result_payload, validate::validate_graphql_result, GraphQlOperation,
    GraphQlOperationPayload, GraphQlQuery, GraphQlQueryTransform, GraphQlSchemaTypes,
};
use reflex_json::{json_object, JsonValue};
use reflex_macros::{dispatcher, Named};

use crate::{
    server::{
        action::{
            graphql_server::{
                GraphQlServerEmitAction, GraphQlServerParseErrorAction,
                GraphQlServerSubscribeAction, GraphQlServerUnsubscribeAction,
            },
            http_server::{HttpServerRequestAction, HttpServerResponseAction},
        },
        utils::{create_http_response, create_json_http_response},
    },
    utils::transform::apply_graphql_query_transform,
};

const HEADER_NAME_GRAPHQL_HTTP_RESPONSE_STATUS: &'static str = "x-graphql-response-type";
const HEADER_VALUE_GRAPHQL_HTTP_RESPONSE_STATUS_SUCCESS: &'static str = "success";
const HEADER_VALUE_GRAPHQL_HTTP_RESPONSE_STATUS_ERROR: &'static str = "error";

pub fn get_http_response_graphql_status<T>(
    response: &Response<T>,
) -> Option<HttpGraphQlServerResponseStatus> {
    response
        .headers()
        .get(HEADER_NAME_GRAPHQL_HTTP_RESPONSE_STATUS)
        .and_then(|value| value.as_bytes().try_into().ok())
}

pub enum HttpGraphQlServerResponseStatus {
    Success,
    Error,
}
impl Into<(HeaderName, HeaderValue)> for HttpGraphQlServerResponseStatus {
    fn into(self) -> (HeaderName, HeaderValue) {
        (
            HeaderName::from_static(HEADER_NAME_GRAPHQL_HTTP_RESPONSE_STATUS),
            match self {
                Self::Success => {
                    HeaderValue::from_static(HEADER_VALUE_GRAPHQL_HTTP_RESPONSE_STATUS_SUCCESS)
                }
                Self::Error => {
                    HeaderValue::from_static(HEADER_VALUE_GRAPHQL_HTTP_RESPONSE_STATUS_ERROR)
                }
            },
        )
    }
}
impl<'a> TryFrom<&'a [u8]> for HttpGraphQlServerResponseStatus {
    type Error = ();
    fn try_from(value: &'a [u8]) -> Result<Self, ()> {
        let value = std::str::from_utf8(value).map_err(|_| ())?;
        match value {
            HEADER_VALUE_GRAPHQL_HTTP_RESPONSE_STATUS_SUCCESS => {
                Ok(HttpGraphQlServerResponseStatus::Success)
            }
            HEADER_VALUE_GRAPHQL_HTTP_RESPONSE_STATUS_ERROR => {
                Ok(HttpGraphQlServerResponseStatus::Error)
            }
            _ => Err(()),
        }
    }
}

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

pub trait HttpGraphQlServerQueryMetricLabels {
    fn labels(&self, operation: &GraphQlOperation, headers: &HeaderMap) -> Vec<(String, String)>;
}
impl<T> HttpGraphQlServerQueryMetricLabels for T
where
    T: Fn(&GraphQlOperation, &HeaderMap) -> Vec<(String, String)>,
{
    fn labels(&self, operation: &GraphQlOperation, headers: &HeaderMap) -> Vec<(String, String)> {
        (self)(operation, headers)
    }
}

pub trait HttpGraphQlServerQueryTransform {
    fn transform(
        &self,
        operation: GraphQlOperation,
        request: &Request<Bytes>,
    ) -> Result<GraphQlOperation, (StatusCode, String)>;
}
impl<T> HttpGraphQlServerQueryTransform for T
where
    T: GraphQlQueryTransform,
{
    fn transform(
        &self,
        operation: GraphQlOperation,
        _request: &Request<Bytes>,
    ) -> Result<GraphQlOperation, (StatusCode, String)> {
        apply_graphql_query_transform(operation, self)
    }
}
pub struct NoopHttpGraphQlServerQueryTransform;
impl HttpGraphQlServerQueryTransform for NoopHttpGraphQlServerQueryTransform {
    fn transform(
        &self,
        operation: GraphQlOperation,
        _request: &Request<Bytes>,
    ) -> Result<GraphQlOperation, (StatusCode, String)> {
        Ok(operation)
    }
}
pub enum EitherHttpGraphQlServerQueryTransform<
    T1: HttpGraphQlServerQueryTransform,
    T2: HttpGraphQlServerQueryTransform,
> {
    Left(T1),
    Right(T2),
}
impl<T1, T2> Clone for EitherHttpGraphQlServerQueryTransform<T1, T2>
where
    T1: HttpGraphQlServerQueryTransform + Clone,
    T2: HttpGraphQlServerQueryTransform + Clone,
{
    fn clone(&self) -> Self {
        match self {
            Self::Left(inner) => Self::Left(inner.clone()),
            Self::Right(inner) => Self::Right(inner.clone()),
        }
    }
}
impl<T1, T2> HttpGraphQlServerQueryTransform for EitherHttpGraphQlServerQueryTransform<T1, T2>
where
    T1: HttpGraphQlServerQueryTransform,
    T2: HttpGraphQlServerQueryTransform,
{
    fn transform(
        &self,
        operation: GraphQlOperation,
        request: &Request<Bytes>,
    ) -> Result<GraphQlOperation, (StatusCode, String)> {
        match self {
            Self::Left(inner) => inner.transform(operation, request),
            Self::Right(inner) => inner.transform(operation, request),
        }
    }
}
pub struct ChainedHttpGraphQlServerQueryTransform<
    T1: HttpGraphQlServerQueryTransform,
    T2: HttpGraphQlServerQueryTransform,
> {
    pub left: T1,
    pub right: T2,
}
impl<T1, T2> Clone for ChainedHttpGraphQlServerQueryTransform<T1, T2>
where
    T1: HttpGraphQlServerQueryTransform + Clone,
    T2: HttpGraphQlServerQueryTransform + Clone,
{
    fn clone(&self) -> Self {
        Self {
            left: self.left.clone(),
            right: self.right.clone(),
        }
    }
}
impl<T1, T2> HttpGraphQlServerQueryTransform for ChainedHttpGraphQlServerQueryTransform<T1, T2>
where
    T1: HttpGraphQlServerQueryTransform,
    T2: HttpGraphQlServerQueryTransform,
{
    fn transform(
        &self,
        operation: GraphQlOperation,
        request: &Request<Bytes>,
    ) -> Result<GraphQlOperation, (StatusCode, String)> {
        let operation = self.left.transform(operation, request)?;
        self.right.transform(operation, request)
    }
}

#[derive(Named, Clone)]
pub struct HttpGraphQlServer<T, TFactory, TTransform, TQueryMetricLabels>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TTransform: HttpGraphQlServerQueryTransform,
    TQueryMetricLabels: HttpGraphQlServerQueryMetricLabels,
{
    schema_types: Option<GraphQlSchemaTypes<'static, String>>,
    factory: TFactory,
    transform: TTransform,
    metric_names: HttpGraphQlServerMetricNames,
    get_query_metric_labels: TQueryMetricLabels,
    main_pid: ProcessId,
    _expression: PhantomData<T>,
}
impl<T, TFactory, TTransform, TQueryMetricLabels>
    HttpGraphQlServer<T, TFactory, TTransform, TQueryMetricLabels>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TTransform: HttpGraphQlServerQueryTransform,
    TQueryMetricLabels: HttpGraphQlServerQueryMetricLabels,
{
    pub fn new(
        schema_types: Option<GraphQlSchemaTypes<'static, String>>,
        factory: TFactory,
        transform: TTransform,
        metric_names: HttpGraphQlServerMetricNames,
        get_query_metric_labels: TQueryMetricLabels,
        main_pid: ProcessId,
    ) -> Self {
        Self {
            schema_types,
            factory,
            transform,
            metric_names: metric_names.init(),
            get_query_metric_labels,
            main_pid,
            _expression: Default::default(),
        }
    }
}

#[derive(Default)]
pub struct HttpGraphQlServerState {
    // TODO: Use newtypes for state hashmap keys
    requests: HashMap<Uuid, HttpGraphQlRequest>,
}
struct HttpGraphQlRequest {
    query: Option<GraphQlQuery>,
    etag: Option<String>,
    metric_labels: Vec<(String, String)>,
}

dispatcher!({
    pub enum HttpGraphQlServerAction<T: Expression> {
        Inbox(HttpServerRequestAction),
        Inbox(GraphQlServerParseErrorAction<T>),
        Inbox(GraphQlServerEmitAction<T>),

        Outbox(GraphQlServerSubscribeAction<T>),
        Outbox(GraphQlServerUnsubscribeAction<T>),
        Outbox(HttpServerResponseAction),
    }

    impl<T, TFactory, TTransform, TQueryMetricLabels, TAction, TTask> Dispatcher<TAction, TTask>
        for HttpGraphQlServer<T, TFactory, TTransform, TQueryMetricLabels>
    where
        T: Expression,
        TFactory: ExpressionFactory<T>,
        TTransform: HttpGraphQlServerQueryTransform,
        TQueryMetricLabels: HttpGraphQlServerQueryMetricLabels,
        TAction: Action,
        TTask: TaskFactory<TAction, TTask>,
    {
        type State = HttpGraphQlServerState;
        type Events<TInbox: TaskInbox<TAction>> = TInbox;
        type Dispose = NoopDisposeCallback;

        fn init<TInbox: TaskInbox<TAction>>(
            &self,
            inbox: TInbox,
            context: &impl ActorInitContext,
        ) -> (Self::State, Self::Events<TInbox>, Self::Dispose) {
            (Default::default(), inbox, Default::default())
        }

        fn accept(&self, _action: &HttpServerRequestAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &HttpServerRequestAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &HttpServerRequestAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_http_server_request(state, action, metadata, context)
        }

        fn accept(&self, _action: &GraphQlServerParseErrorAction<T>) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &GraphQlServerParseErrorAction<T>,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &GraphQlServerParseErrorAction<T>,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_graphql_parse_error(state, action, metadata, context)
        }

        fn accept(&self, _action: &GraphQlServerEmitAction<T>) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &GraphQlServerEmitAction<T>,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &GraphQlServerEmitAction<T>,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_graphql_server_emit(state, action, metadata, context)
        }
    }
});

impl<T, TFactory, TTransform, TQueryMetricLabels>
    HttpGraphQlServer<T, TFactory, TTransform, TQueryMetricLabels>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TTransform: HttpGraphQlServerQueryTransform,
    TQueryMetricLabels: HttpGraphQlServerQueryMetricLabels,
{
    fn handle_http_server_request<TAction, TTask>(
        &self,
        state: &mut HttpGraphQlServerState,
        action: &HttpServerRequestAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<HttpServerResponseAction> + From<GraphQlServerSubscribeAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
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
            Err((status_code, message)) => {
                Some(SchedulerTransition::new(once(SchedulerCommand::Send(
                    self.main_pid,
                    HttpServerResponseAction {
                        request_id,
                        response: create_json_http_response(
                            status_code,
                            once(HttpGraphQlServerResponseStatus::Error.into()),
                            &JsonValue::from(message),
                        ),
                    }
                    .into(),
                ))))
            }
            Ok(operation) => {
                let metric_labels = self
                    .get_query_metric_labels
                    .labels(&operation, request.headers());
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
                    query: self
                        .schema_types
                        .as_ref()
                        .map(|_| operation.query().clone()),
                    etag: parse_request_etag(&request),
                    metric_labels,
                });
                Some(SchedulerTransition::new(once(SchedulerCommand::Send(
                    self.main_pid,
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
    fn handle_graphql_parse_error<TAction, TTask>(
        &self,
        state: &mut HttpGraphQlServerState,
        action: &GraphQlServerParseErrorAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<HttpServerResponseAction>,
        TTask: TaskFactory<TAction, TTask>,
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
        Some(SchedulerTransition::new(once(SchedulerCommand::Send(
            self.main_pid,
            HttpServerResponseAction {
                request_id: *subscription_id,
                response,
            }
            .into(),
        ))))
    }
    fn handle_graphql_server_emit<TAction, TTask>(
        &self,
        state: &mut HttpGraphQlServerState,
        action: &GraphQlServerEmitAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<GraphQlServerUnsubscribeAction<T>> + From<HttpServerResponseAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let GraphQlServerEmitAction {
            subscription_id,
            result,
        } = action;
        let request = state.requests.remove(subscription_id)?;
        let HttpGraphQlRequest {
            metric_labels,
            etag,
            query,
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
                let payload =
                    serialize_graphql_result_payload(result, &self.factory).and_then(|payload| {
                        let query = query.as_ref();
                        let schema_types = self.schema_types.as_ref();
                        match (query, schema_types) {
                            (Some(query), Some(schema_types)) => {
                                validate_graphql_result(&payload, query, schema_types)
                                    .map(|_| payload)
                            }
                            _ => Ok(payload),
                        }
                    });
                create_json_http_response(
                    StatusCode::OK,
                    create_etag_header(&response_etag)
                        .ok()
                        .into_iter()
                        .chain(once(
                            (if matches!(&payload, Err(_)) {
                                HttpGraphQlServerResponseStatus::Error
                            } else {
                                HttpGraphQlServerResponseStatus::Success
                            })
                            .into(),
                        )),
                    &match payload {
                        Ok(payload) => create_graphql_success_response(payload),
                        Err(errors) => create_graphql_error_response(errors),
                    },
                )
            }
        };
        Some(SchedulerTransition::new([
            SchedulerCommand::Send(
                self.main_pid,
                GraphQlServerUnsubscribeAction {
                    subscription_id: *subscription_id,
                    _expression: Default::default(),
                }
                .into(),
            ),
            SchedulerCommand::Send(
                self.main_pid,
                HttpServerResponseAction {
                    request_id: *subscription_id,
                    response,
                }
                .into(),
            ),
        ]))
    }
}

fn parse_graphql_request(request: &Request<Bytes>) -> Result<GraphQlOperation, String> {
    let content_type_header = request
        .headers()
        .get(header::CONTENT_TYPE)
        .ok_or_else(|| String::from("Missing Content-Type header"))?;
    let content_type = content_type_header
        .to_str()
        .map_err(|_| String::from("Invalid Content-Type header"))?;
    let operation = match content_type {
        "application/graphql" => parse_plain_graphql_request_body(
            request.body(),
            request
                .headers()
                .get(HeaderName::from_static("x-graphql-operation-name"))
                .and_then(|value| value.to_str().ok().map(String::from)),
        ),
        "application/json" => parse_json_graphql_request_body(request.body()),
        _ => Err(String::from("Unsupported Content-Type header")),
    }?;
    let GraphQlOperationPayload {
        query,
        operation_name,
        variables,
        extensions,
    } = operation;
    let query = reflex_graphql::parse_graphql_query(&query).map_err(|err| format!("{}", err))?;
    Ok(GraphQlOperation::new(
        query,
        operation_name,
        variables,
        extensions,
    ))
}

fn parse_plain_graphql_request_body(
    body: &Bytes,
    operation_name: Option<String>,
) -> Result<GraphQlOperationPayload, String> {
    let query = parse_request_body(body).map_err(|_| String::from("Invalid request body"))?;
    Ok(GraphQlOperationPayload {
        query,
        operation_name,
        variables: Default::default(),
        extensions: Default::default(),
    })
}

fn parse_json_graphql_request_body(body: &Bytes) -> Result<GraphQlOperationPayload, String> {
    let json = parse_request_body(body).map_err(|_| String::from("Invalid request body"))?;
    match deserialize_graphql_operation(&json) {
        Err(error) => Err(error),
        Ok(operation) => match operation
            .operation_name
            .as_ref()
            .map(|value| value.as_str())
        {
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
