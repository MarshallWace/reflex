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
    sdk::Resource,
    trace::{SpanContext, TraceContextExt, TraceFlags, TraceState, Tracer},
    Context, KeyValue,
};
use opentelemetry_otlp::{SpanExporterBuilder, WithExportConfig};
use reflex_dispatcher::{
    Action, ActorEvents, HandlerContext, MessageData, NoopDisposeCallback, SchedulerMode,
    SchedulerTransition, TaskFactory, TaskInbox,
};
use reflex_handlers::utils::tls::{
    create_https_client,
    hyper::{body::HttpBody, client::connect::Connect},
    tokio_native_tls::native_tls,
};
use reflex_macros::{dispatcher, Named};
use tonic::{self, transport::ClientTlsConfig};

use crate::{
    server::action::telemetry::{
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

pub fn create_grpc_otlp_tracer(
    endpoint: impl Into<String>,
    tls_cert: Option<tonic::transport::Certificate>,
    resource_attributes: Option<Resource>,
) -> Result<opentelemetry::sdk::trace::Tracer, OpenTelemetryClientError> {
    let exporter = opentelemetry_otlp::new_exporter()
        .tonic()
        .with_endpoint(endpoint);
    let exporter = if let Some(tls_cert) = tls_cert {
        exporter.with_tls_config(ClientTlsConfig::new().ca_certificate(tls_cert))
    } else {
        exporter
    };
    create_otlp_tracer(exporter, resource_attributes)
}

pub fn create_http_otlp_tracer(
    endpoint: impl Into<String>,
    http_headers: impl IntoIterator<Item = (HeaderName, HeaderValue)>,
    tls_cert: Option<native_tls::Certificate>,
    resource_attributes: Option<Resource>,
) -> Result<opentelemetry::sdk::trace::Tracer, OpenTelemetryClientError> {
    let client =
        create_https_client::<Body>(tls_cert).map_err(OpenTelemetryClientError::Certificate)?;
    let http_headers = http_headers
        .into_iter()
        .filter_map(|(key, value)| {
            value
                .to_str()
                .ok()
                .map(|value| (String::from(key.as_str()), String::from(value)))
        })
        .collect();
    create_otlp_tracer(
        opentelemetry_otlp::new_exporter()
            .http()
            .with_endpoint(endpoint)
            .with_headers(http_headers)
            .with_http_client(OpenTelemetryHyperClient(client)),
        resource_attributes,
    )
}

fn create_otlp_tracer(
    exporter: impl Into<SpanExporterBuilder>,
    resource_attributes: Option<Resource>,
) -> Result<opentelemetry::sdk::trace::Tracer, OpenTelemetryClientError> {
    opentelemetry_otlp::new_pipeline()
        .tracing()
        .with_exporter(exporter)
        .with_trace_config({
            let config = opentelemetry::sdk::trace::config();
            if let Some(resource_attributes) = resource_attributes {
                config.with_resource(resource_attributes)
            } else {
                config
            }
        })
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
            let response = client
                .request(request.map(|body| body.into()))
                .await
                .map_err(Box::new)?;
            let (response_headers, response_body) = response.into_parts();
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

#[derive(Named, Clone)]
pub struct OpenTelemetryActor<T: Tracer> {
    tracer: T,
}
impl<T: Tracer> OpenTelemetryActor<T> {
    pub fn new(tracer: T) -> Self {
        Self { tracer }
    }
}

pub struct OpenTelemetryActorState {
    active_spans: HashMap<Traceparent, Context>,
}
impl Default for OpenTelemetryActorState {
    fn default() -> Self {
        Self {
            active_spans: Default::default(),
        }
    }
}

dispatcher!({
    pub enum OpenTelemetryAction {
        Inbox(TelemetryMiddlewareTransactionStartAction),
        Inbox(TelemetryMiddlewareTransactionEndAction),
    }

    impl<TTracer, TAction, TTask> Dispatcher<TAction, TTask> for OpenTelemetryActor<TTracer>
    where
        TTracer: Tracer,
        TTracer::Span: Send + Sync + 'static,
        TAction: Action,
        TTask: TaskFactory<TAction, TTask>,
    {
        type State = OpenTelemetryActorState;
        type Events<TInbox: TaskInbox<TAction>> = TInbox;
        type Dispose = NoopDisposeCallback;

        fn init(&self) -> Self::State {
            Default::default()
        }
        fn events<TInbox: TaskInbox<TAction>>(
            &self,
            inbox: TInbox,
        ) -> ActorEvents<TInbox, Self::Events<TInbox>, Self::Dispose> {
            ActorEvents::Sync(inbox)
        }

        fn accept(&self, _action: &TelemetryMiddlewareTransactionStartAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &TelemetryMiddlewareTransactionStartAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &TelemetryMiddlewareTransactionStartAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_telemetry_middleware_transaction_start(state, action, metadata, context)
        }

        fn accept(&self, _action: &TelemetryMiddlewareTransactionEndAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &TelemetryMiddlewareTransactionEndAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &TelemetryMiddlewareTransactionEndAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_telemetry_middleware_transaction_end(state, action, metadata, context)
        }
    }
});

impl<TTracer> OpenTelemetryActor<TTracer>
where
    TTracer: Tracer,
    TTracer::Span: Send + Sync + 'static,
{
    fn handle_telemetry_middleware_transaction_start<TAction, TTask>(
        &self,
        state: &mut OpenTelemetryActorState,
        action: &TelemetryMiddlewareTransactionStartAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action,
        TTask: TaskFactory<TAction, TTask>,
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
                Some(traceparent) => match state.active_spans.get(traceparent) {
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
            if let Entry::Vacant(entry) = state.active_spans.entry(*transaction_id) {
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
    fn handle_telemetry_middleware_transaction_end<TAction, TTask>(
        &self,
        state: &mut OpenTelemetryActorState,
        action: &TelemetryMiddlewareTransactionEndAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action,
        TTask: TaskFactory<TAction, TTask>,
    {
        let TelemetryMiddlewareTransactionEndAction { transaction_ids } = action;
        for transaction_id in transaction_ids.iter() {
            if let Some(context) = state.active_spans.remove(transaction_id) {
                context.span().end();
            }
        }
        None
    }
}
