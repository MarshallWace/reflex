// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{hash_map::Entry, HashMap},
    pin::Pin,
    time::SystemTime,
};

use bytes::Bytes;
use futures::Future;
use http::{header::HeaderName, HeaderValue, Request, Response};
use hyper::Body;
use opentelemetry::{
    trace::{SpanContext, TraceContextExt, TraceFlags, TraceState, Tracer},
    Context, KeyValue,
};
use opentelemetry_otlp::WithExportConfig;
use reflex_dispatcher::{
    Action, Actor, HandlerContext, InboundAction, MessageData, StateTransition,
};
use reflex_handlers::utils::tls::{
    create_https_client,
    hyper::{body::HttpBody, client::connect::Connect},
    native_tls::{self, Certificate},
};

use crate::{
    middleware::action::telemetry::{
        TelemetryMiddlewareTransactionEndAction, TelemetryMiddlewareTransactionStartAction,
        TelemetryTransaction,
    },
    utils::traceparent::Traceparent,
};

#[derive(Debug)]
pub enum OpenTelemetryClientError {
    Certificate(native_tls::Error),
    Tracer(opentelemetry::trace::TraceError),
}
impl std::fmt::Display for OpenTelemetryClientError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Certificate(err) => std::fmt::Display::fmt(err, f),
            Self::Tracer(err) => std::fmt::Display::fmt(err, f),
        }
    }
}
impl std::error::Error for OpenTelemetryClientError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Certificate(err) => err.source(),
            Self::Tracer(err) => err.source(),
        }
    }
}

pub fn create_http_otlp_tracer(
    resource_attributes: impl IntoIterator<Item = KeyValue>,
    endpoint: impl Into<String>,
    http_headers: impl IntoIterator<Item = (HeaderName, HeaderValue)>,
    tls_cert: Option<Certificate>,
) -> Result<opentelemetry::sdk::trace::Tracer, OpenTelemetryClientError> {
    let client =
        create_https_client::<Body>(tls_cert).map_err(OpenTelemetryClientError::Certificate)?;
    let headers = http_headers
        .into_iter()
        .filter_map(|(key, value)| {
            value
                .to_str()
                .ok()
                .map(|value| (String::from(key.as_str()), String::from(value)))
        })
        .collect();
    let exporter = opentelemetry_otlp::new_exporter()
        .http()
        .with_endpoint(endpoint)
        .with_headers(headers)
        .with_http_client(OpenTelemetryHyperClient(client));
    opentelemetry_otlp::new_pipeline()
        .tracing()
        .with_exporter(exporter)
        .with_trace_config(
            opentelemetry::sdk::trace::config()
                .with_resource(opentelemetry::sdk::Resource::new(resource_attributes)),
        )
        .install_batch(opentelemetry::runtime::Tokio)
        .map_err(OpenTelemetryClientError::Tracer)
}

#[derive(Clone)]
struct OpenTelemetryHyperClient<C, B>(hyper::Client<C, B>);
impl<C, B> opentelemetry_http::HttpClient for OpenTelemetryHyperClient<C, B>
where
    C: Connect + Clone + Send + Sync + 'static,
    B: HttpBody + From<Vec<u8>> + Send + 'static,
    B::Data: Send,
    B::Error: Into<Box<dyn std::error::Error + Send + Sync>>,
{
    fn send<'a: 'async_trait, 'async_trait>(
        &'a self,
        request: Request<Vec<u8>>,
    ) -> Pin<
        Box<
            dyn Future<Output = Result<Response<Bytes>, opentelemetry_http::HttpError>>
                + Send
                + 'async_trait,
        >,
    >
    where
        Self: 'async_trait,
    {
        Box::pin(async move {
            let Self(client) = self;
            let result = client
                .request(request.map(|body| body.into()))
                .await
                .map_err(Box::new)?;
            let (response_headers, response_body) = result.into_parts();
            let response_body = hyper::body::to_bytes(response_body)
                .await
                .map_err(Box::new)?;
            Ok(Response::from_parts(response_headers, response_body))
        })
    }
}
impl<C, B> std::fmt::Debug for OpenTelemetryHyperClient<C, B> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
    }
}

pub trait OpenTelemetryMiddlewareAction:
    Action
    + InboundAction<TelemetryMiddlewareTransactionStartAction>
    + InboundAction<TelemetryMiddlewareTransactionEndAction>
{
}
impl<TAction> OpenTelemetryMiddlewareAction for TAction where
    Self: Action
        + InboundAction<TelemetryMiddlewareTransactionStartAction>
        + InboundAction<TelemetryMiddlewareTransactionEndAction>
{
}

pub struct OpenTelemetryMiddleware<T: Tracer> {
    tracer: T,
    state: OpenTelemetryMiddlewareState,
}
impl<T: Tracer> OpenTelemetryMiddleware<T> {
    pub fn new(tracer: T) -> Self {
        Self {
            tracer,
            state: Default::default(),
        }
    }
}

struct OpenTelemetryMiddlewareState {
    active_spans: HashMap<Traceparent, Context>,
}
impl Default for OpenTelemetryMiddlewareState {
    fn default() -> Self {
        Self {
            active_spans: Default::default(),
        }
    }
}

impl<TAction, T: Tracer> Actor<TAction> for OpenTelemetryMiddleware<T>
where
    T::Span: Send + Sync + 'static,
    TAction: Action
        + InboundAction<TelemetryMiddlewareTransactionStartAction>
        + InboundAction<TelemetryMiddlewareTransactionEndAction>,
{
    fn handle(
        &mut self,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> StateTransition<TAction> {
        if let Some(action) = action.match_type() {
            self.handle_transaction_start(action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_transaction_end(action, metadata, context)
        } else {
            None
        }
        .unwrap_or_default()
    }
}
impl<T: Tracer> OpenTelemetryMiddleware<T>
where
    T::Span: Send + Sync + 'static,
{
    fn handle_transaction_start<TAction>(
        &mut self,
        action: &TelemetryMiddlewareTransactionStartAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action,
    {
        let TelemetryMiddlewareTransactionStartAction { transactions } = action;
        for transaction in transactions.iter() {
            let TelemetryTransaction {
                transaction_id,
                parent_ids,
                name,
                attributes,
            } = transaction;
            // TODO: support spans with multiple parents
            let parent_id = parent_ids.first();
            let context = match parent_id {
                None => Context::new(),
                Some(traceparent) => match self.state.active_spans.get(traceparent) {
                    Some(context) => context.clone(),
                    None => Context::new().with_remote_span_context(SpanContext::new(
                        traceparent.trace_id,
                        traceparent.span_id,
                        TraceFlags::SAMPLED,
                        true,
                        TraceState::default(),
                    )),
                },
            };
            if let Entry::Vacant(entry) = self.state.active_spans.entry(*transaction_id) {
                let span = self
                    .tracer
                    .span_builder(String::from(name))
                    .with_trace_id(transaction_id.trace_id)
                    .with_span_id(transaction_id.span_id)
                    .with_attributes(
                        attributes
                            .iter()
                            .map(|(key, value)| {
                                KeyValue::new(String::from(key), String::from(value))
                            })
                            .collect(),
                    )
                    .with_start_time(SystemTime::now())
                    .start_with_context(&self.tracer, &context);
                entry.insert(context.with_span(span));
            }
        }
        None
    }
    fn handle_transaction_end<TAction>(
        &mut self,
        action: &TelemetryMiddlewareTransactionEndAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action,
    {
        let TelemetryMiddlewareTransactionEndAction { transaction_ids } = action;
        for transaction_id in transaction_ids.iter() {
            if let Some(context) = self.state.active_spans.remove(transaction_id) {
                context.span().end();
            }
        }
        None
    }
}
