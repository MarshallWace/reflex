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
use opentelemetry::{
    trace::{SpanContext, TraceContextExt, TraceError, TraceFlags, TraceState, Tracer},
    Context, KeyValue,
};
use opentelemetry_otlp::WithExportConfig;
use reflex_dispatcher::{
    Action, Actor, HandlerContext, InboundAction, MessageData, StateTransition,
};

use crate::{
    middleware::action::telemetry::{
        TelemetryMiddlewareTransactionEndAction, TelemetryMiddlewareTransactionStartAction,
        TelemetryTransaction,
    },
    utils::traceparent::Traceparent,
};

pub fn create_http_otlp_tracer(
    resource_attributes: impl IntoIterator<Item = KeyValue>,
    endpoint: impl Into<String>,
    http_headers: impl IntoIterator<Item = (HeaderName, HeaderValue)>,
) -> Result<opentelemetry::sdk::trace::Tracer, TraceError> {
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
        .with_http_client(HyperHttpClient);
    opentelemetry_otlp::new_pipeline()
        .tracing()
        .with_exporter(exporter)
        .with_trace_config(
            opentelemetry::sdk::trace::config()
                .with_resource(opentelemetry::sdk::Resource::new(resource_attributes)),
        )
        .install_batch(opentelemetry::runtime::Tokio)
}

#[derive(Debug)]
struct HyperHttpClient;
impl opentelemetry_http::HttpClient for HyperHttpClient {
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
        Box::pin({
            let https = hyper_tls::HttpsConnector::new();
            let client = hyper::Client::builder().build::<_, hyper::Body>(https);
            async move {
                let result = client
                    .request(request.map(|body| hyper::Body::from(body)))
                    .await
                    .map_err(Box::new)?;
                let (response_headers, response_body) = result.into_parts();
                let response_body = hyper::body::to_bytes(response_body)
                    .await
                    .map_err(Box::new)?;
                Ok(Response::from_parts(response_headers, response_body))
            }
        })
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
            StateTransition::new(None)
        }
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
    ) -> StateTransition<TAction>
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
        StateTransition::new(None)
    }
    fn handle_transaction_end<TAction>(
        &mut self,
        action: &TelemetryMiddlewareTransactionEndAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> StateTransition<TAction>
    where
        TAction: Action,
    {
        let TelemetryMiddlewareTransactionEndAction { transaction_ids } = action;
        for transaction_id in transaction_ids.iter() {
            if let Some(context) = self.state.active_spans.remove(transaction_id) {
                context.span().end();
            }
        }
        StateTransition::new(None)
    }
}
