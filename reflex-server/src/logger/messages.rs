// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::marker::PhantomData;

use reflex::core::{Expression, ExpressionFactory};
use reflex_dispatcher::{Action, Matcher};
use reflex_grpc::action::{
    GrpcHandlerConnectErrorAction, GrpcHandlerConnectSuccessAction, GrpcHandlerTransportErrorAction,
};
use reflex_handlers::action::graphql::{
    GraphQlHandlerWebSocketConnectSuccessAction, GraphQlHandlerWebSocketConnectionErrorAction,
};
use reflex_macros::blanket_trait;
use reflex_runtime::{
    action::effect::{
        EffectEmitAction, EffectSubscribeAction, EffectThrottleEmitAction, EffectUnsubscribeAction,
    },
    actor::evaluate_handler::is_evaluate_effect_type,
};

use crate::{
    cli::reflex_server::OpenTelemetryConfig,
    server::action::{
        graphql_server::GraphQlServerSubscribeAction,
        init::{
            InitGraphRootAction, InitHttpServerAction, InitOpenTelemetryAction,
            InitPrometheusMetricsAction,
        },
        opentelemetry::OpenTelemetryMiddlewareErrorAction,
    },
};

use crate::logger::formatter::{LogFormatter, LogWriter};

blanket_trait!(
    pub trait DefaultActionFormatterAction<T: Expression>:
        Matcher<InitGraphRootAction>
        + Matcher<InitHttpServerAction>
        + Matcher<InitOpenTelemetryAction>
        + Matcher<InitPrometheusMetricsAction>
        + Matcher<OpenTelemetryMiddlewareErrorAction>
        + Matcher<GraphQlServerSubscribeAction<T>>
        + Matcher<EffectSubscribeAction<T>>
        + Matcher<EffectUnsubscribeAction<T>>
        + Matcher<EffectEmitAction<T>>
        + Matcher<EffectThrottleEmitAction>
        + Matcher<GraphQlHandlerWebSocketConnectSuccessAction>
        + Matcher<GraphQlHandlerWebSocketConnectionErrorAction>
        + Matcher<GrpcHandlerConnectSuccessAction>
        + Matcher<GrpcHandlerConnectErrorAction>
        + Matcher<GrpcHandlerTransportErrorAction>
    {
    }
);

#[derive(Debug)]
pub struct DefaultActionFormatter<T, TFactory, TAction>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAction: Action,
{
    factory: TFactory,
    _expression: PhantomData<T>,
    _action: PhantomData<TAction>,
}
impl<T, TFactory, TAction> DefaultActionFormatter<T, TFactory, TAction>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAction: Action,
{
    pub fn new(factory: TFactory) -> Self {
        Self {
            factory,
            _expression: PhantomData,
            _action: PhantomData,
        }
    }
}
impl<T, TFactory, TAction> Clone for DefaultActionFormatter<T, TFactory, TAction>
where
    T: Expression,
    TFactory: ExpressionFactory<T> + Clone,
    TAction: Action,
{
    fn clone(&self) -> Self {
        Self {
            factory: self.factory.clone(),
            _expression: PhantomData,
            _action: PhantomData,
        }
    }
}
impl<T, TFactory, TAction> LogFormatter for DefaultActionFormatter<T, TFactory, TAction>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAction: Action + DefaultActionFormatterAction<T>,
{
    type Message = TAction;
    type Writer<'a> = DefaultActionFormatWriter<'a, T>
    where
        Self: 'a,
        TAction: 'a;
    fn format<'a>(&self, message: &'a Self::Message) -> Option<Self::Writer<'a>>
    where
        Self: 'a,
    {
        if let Option::<&InitPrometheusMetricsAction>::Some(action) = message.match_type() {
            Some(DefaultActionFormatWriter::InitPrometheusMetrics(action))
        } else if let Option::<&InitOpenTelemetryAction>::Some(action) = message.match_type() {
            Some(DefaultActionFormatWriter::InitOpenTelemetry(action))
        } else if let Option::<&InitGraphRootAction>::Some(action) = message.match_type() {
            Some(DefaultActionFormatWriter::InitGraphRoot(action))
        } else if let Option::<&InitHttpServerAction>::Some(action) = message.match_type() {
            Some(DefaultActionFormatWriter::InitHttpServer(action))
        } else if let Option::<&OpenTelemetryMiddlewareErrorAction>::Some(action) =
            message.match_type()
        {
            Some(DefaultActionFormatWriter::OpenTelemetryMiddlewareError(
                action,
            ))
        } else if let Option::<&GraphQlServerSubscribeAction<T>>::Some(action) =
            message.match_type()
        {
            Some(DefaultActionFormatWriter::GraphQlServerSubscribe(action))
        } else if let Option::<&GraphQlHandlerWebSocketConnectSuccessAction>::Some(action) =
            message.match_type()
        {
            Some(DefaultActionFormatWriter::GraphQlHandlerWebSocketConnectSuccess(action))
        } else if let Option::<&GraphQlHandlerWebSocketConnectionErrorAction>::Some(action) =
            message.match_type()
        {
            Some(DefaultActionFormatWriter::GraphQlHandlerWebSocketConnectionError(action))
        } else if let Option::<&GrpcHandlerConnectSuccessAction>::Some(action) =
            message.match_type()
        {
            Some(DefaultActionFormatWriter::GrpcHandlerConnectSuccess(action))
        } else if let Option::<&GrpcHandlerConnectErrorAction>::Some(action) = message.match_type()
        {
            Some(DefaultActionFormatWriter::GrpcHandlerConnectError(action))
        } else if let Option::<&GrpcHandlerTransportErrorAction>::Some(action) =
            message.match_type()
        {
            Some(DefaultActionFormatWriter::GrpcHandlerTransportError(action))
        } else if let Option::<&EffectSubscribeAction<T>>::Some(action) = message.match_type() {
            if !is_evaluate_effect_type(&action.effect_type, &self.factory) {
                Some(DefaultActionFormatWriter::EffectSubscribe(action))
            } else {
                None
            }
        } else if let Option::<&EffectUnsubscribeAction<T>>::Some(action) = message.match_type() {
            if !is_evaluate_effect_type(&action.effect_type, &self.factory) {
                Some(DefaultActionFormatWriter::EffectUnsubscribe(action))
            } else {
                None
            }
        } else if let Option::<&EffectEmitAction<T>>::Some(action) = message.match_type() {
            Some(DefaultActionFormatWriter::EffectEmit(action))
        } else if let Option::<&EffectThrottleEmitAction>::Some(action) = message.match_type() {
            Some(DefaultActionFormatWriter::EffectThrottleEmit(action))
        } else {
            None
        }
    }
}

pub enum DefaultActionFormatWriter<'a, T>
where
    T: Expression,
{
    InitPrometheusMetrics(&'a InitPrometheusMetricsAction),
    InitOpenTelemetry(&'a InitOpenTelemetryAction),
    InitGraphRoot(&'a InitGraphRootAction),
    InitHttpServer(&'a InitHttpServerAction),
    OpenTelemetryMiddlewareError(&'a OpenTelemetryMiddlewareErrorAction),
    GraphQlServerSubscribe(&'a GraphQlServerSubscribeAction<T>),
    GraphQlHandlerWebSocketConnectSuccess(&'a GraphQlHandlerWebSocketConnectSuccessAction),
    GraphQlHandlerWebSocketConnectionError(&'a GraphQlHandlerWebSocketConnectionErrorAction),
    GrpcHandlerConnectSuccess(&'a GrpcHandlerConnectSuccessAction),
    GrpcHandlerConnectError(&'a GrpcHandlerConnectErrorAction),
    GrpcHandlerTransportError(&'a GrpcHandlerTransportErrorAction),
    EffectSubscribe(&'a EffectSubscribeAction<T>),
    EffectUnsubscribe(&'a EffectUnsubscribeAction<T>),
    EffectEmit(&'a EffectEmitAction<T>),
    EffectThrottleEmit(&'a EffectThrottleEmitAction),
}
impl<'a, T> LogWriter for DefaultActionFormatWriter<'a, T>
where
    T: Expression,
{
    fn write(&self, f: &mut impl std::io::Write) -> std::io::Result<()> {
        match self {
            Self::InitPrometheusMetrics(inner) => inner.write(f),
            Self::InitOpenTelemetry(inner) => inner.write(f),
            Self::InitGraphRoot(inner) => inner.write(f),
            Self::InitHttpServer(inner) => inner.write(f),
            Self::OpenTelemetryMiddlewareError(inner) => inner.write(f),
            Self::GraphQlServerSubscribe(inner) => inner.write(f),
            Self::GraphQlHandlerWebSocketConnectSuccess(inner) => inner.write(f),
            Self::GraphQlHandlerWebSocketConnectionError(inner) => inner.write(f),
            Self::GrpcHandlerConnectSuccess(inner) => inner.write(f),
            Self::GrpcHandlerConnectError(inner) => inner.write(f),
            Self::GrpcHandlerTransportError(inner) => inner.write(f),
            Self::EffectSubscribe(inner) => inner.write(f),
            Self::EffectUnsubscribe(inner) => inner.write(f),
            Self::EffectEmit(inner) => inner.write(f),
            Self::EffectThrottleEmit(inner) => inner.write(f),
        }
    }
}

impl LogWriter for InitPrometheusMetricsAction {
    fn write(&self, f: &mut impl std::io::Write) -> std::io::Result<()> {
        write!(
            f,
            "Publishing to Prometheus metrics endpoint http://{}/",
            self.address
        )
    }
}

impl LogWriter for InitOpenTelemetryAction {
    fn write(&self, f: &mut impl std::io::Write) -> std::io::Result<()> {
        write!(
            f,
            "Publishing to OpenTelemetry {} collector {}",
            match &self.config {
                OpenTelemetryConfig::Http(_) => "HTTP",
                OpenTelemetryConfig::Grpc(_) => "gRPC",
            },
            match &self.config {
                OpenTelemetryConfig::Http(config) => &config.endpoint,
                OpenTelemetryConfig::Grpc(config) => &config.endpoint,
            },
        )
    }
}

impl LogWriter for InitGraphRootAction {
    fn write(&self, f: &mut impl std::io::Write) -> std::io::Result<()> {
        write!(f, "Graph root compiled in {:?}", self.compiler_duration)
    }
}

impl LogWriter for InitHttpServerAction {
    fn write(&self, f: &mut impl std::io::Write) -> std::io::Result<()> {
        write!(
            f,
            "Listening for incoming GraphQL HTTP requests on http://{}/",
            self.address,
        )
    }
}

impl LogWriter for OpenTelemetryMiddlewareErrorAction {
    fn write(&self, f: &mut impl std::io::Write) -> std::io::Result<()> {
        write!(f, "OpenTelemetry error: {}", self.error)
    }
}

impl<T: Expression> LogWriter for GraphQlServerSubscribeAction<T> {
    fn write(&self, f: &mut impl std::io::Write) -> std::io::Result<()> {
        write!(
            f,
            "Handling GraphQL operation: {}",
            self.operation.operation_name().unwrap_or("<anonymous>"),
        )
    }
}

impl LogWriter for GraphQlHandlerWebSocketConnectSuccessAction {
    fn write(&self, f: &mut impl std::io::Write) -> std::io::Result<()> {
        write!(f, "Connected to GraphQL WebSocket server {}", self.url)
    }
}

impl LogWriter for GraphQlHandlerWebSocketConnectionErrorAction {
    fn write(&self, f: &mut impl std::io::Write) -> std::io::Result<()> {
        write!(
            f,
            "GraphQL WebSocket connection error {}: {}",
            self.url, self.message,
        )
    }
}

impl LogWriter for GrpcHandlerConnectSuccessAction {
    fn write(&self, f: &mut impl std::io::Write) -> std::io::Result<()> {
        write!(f, "Connected to gRPC server {}", self.url)
    }
}

impl LogWriter for GrpcHandlerConnectErrorAction {
    fn write(&self, f: &mut impl std::io::Write) -> std::io::Result<()> {
        write!(
            f,
            "Failed to connect to gRPC server {}: {}",
            self.url, self.message,
        )
    }
}

impl LogWriter for GrpcHandlerTransportErrorAction {
    fn write(&self, f: &mut impl std::io::Write) -> std::io::Result<()> {
        write!(
            f,
            "gRPC server transport error: {}.{} Error {}: {}: {}",
            self.service_name,
            self.method_name,
            self.status.code as i32,
            self.status.code,
            self.status.message,
        )
    }
}

impl<T: Expression> LogWriter for EffectSubscribeAction<T> {
    fn write(&self, f: &mut impl std::io::Write) -> std::io::Result<()> {
        write!(f, "Effect subscribed: {}", self.effect_type)?;
        if self.effects.len() > 1 {
            write!(f, " x {}", self.effects.len())?;
        }
        Ok(())
    }
}

impl<T: Expression> LogWriter for EffectUnsubscribeAction<T> {
    fn write(&self, f: &mut impl std::io::Write) -> std::io::Result<()> {
        write!(f, "Effect unsubscribed: {}", self.effect_type)?;
        if self.effects.len() > 1 {
            write!(f, " x {}", self.effects.len())?;
        }
        Ok(())
    }
}

impl<T: Expression> LogWriter for EffectEmitAction<T> {
    fn write(&self, f: &mut impl std::io::Write) -> std::io::Result<()> {
        if self.effect_types.len() > 1 {
            write!(f, "Effect emitted:")?;
            for batch in self.effect_types.iter() {
                write!(f, "\n {} x {}", batch.effect_type, batch.updates.len())?
            }
            Ok(())
        } else if let Some(batch) = self.effect_types.iter().next() {
            write!(
                f,
                "Effect emitted: {} x {}",
                batch.effect_type,
                batch.updates.len()
            )
        } else {
            Ok(())
        }
    }
}

impl LogWriter for EffectThrottleEmitAction {
    fn write(&self, f: &mut impl std::io::Write) -> std::io::Result<()> {
        write!(f, "Emitting throttled effects")
    }
}
