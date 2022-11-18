// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::marker::PhantomData;

use reflex::core::Expression;
use reflex_dispatcher::{
    Action, Matcher, MessageData, MiddlewareContext, SchedulerCommand, TaskFactory,
};
use reflex_grpc::action::{
    GrpcHandlerConnectErrorAction, GrpcHandlerConnectSuccessAction, GrpcHandlerTransportErrorAction,
};
use reflex_handlers::action::graphql::{
    GraphQlHandlerWebSocketConnectSuccessAction, GraphQlHandlerWebSocketConnectionErrorAction,
};
use reflex_macros::blanket_trait;
use reflex_runtime::{
    action::effect::{EffectEmitAction, EffectSubscribeAction, EffectUnsubscribeAction},
    actor::evaluate_handler::EFFECT_TYPE_EVALUATE,
};

use crate::{
    cli::reflex_server::OpenTelemetryConfig,
    logger::ActionLogger,
    server::action::{
        graphql_server::GraphQlServerSubscribeAction,
        init::{
            InitGraphRootAction, InitHttpServerAction, InitOpenTelemetryAction,
            InitPrometheusMetricsAction,
        },
        opentelemetry::OpenTelemetryMiddlewareErrorAction,
    },
};

blanket_trait!(
    pub trait FormattedLoggerAction<T: Expression>:
        Action
        + Matcher<InitGraphRootAction>
        + Matcher<InitHttpServerAction>
        + Matcher<InitOpenTelemetryAction>
        + Matcher<InitPrometheusMetricsAction>
        + Matcher<OpenTelemetryMiddlewareErrorAction>
        + Matcher<GraphQlServerSubscribeAction<T>>
        + Matcher<EffectSubscribeAction<T>>
        + Matcher<EffectUnsubscribeAction<T>>
        + Matcher<EffectEmitAction<T>>
        + Matcher<GraphQlHandlerWebSocketConnectSuccessAction>
        + Matcher<GraphQlHandlerWebSocketConnectionErrorAction>
        + Matcher<GrpcHandlerConnectSuccessAction>
        + Matcher<GrpcHandlerConnectErrorAction>
        + Matcher<GrpcHandlerTransportErrorAction>
    {
    }
);

pub struct FormattedLogger<T: Expression, TOut, TAction, TTask>
where
    TOut: std::io::Write,
    TAction: FormattedLoggerAction<T>,
    TTask: TaskFactory<TAction, TTask>,
{
    prefix: String,
    output: TOut,
    _expression: PhantomData<T>,
    _action: PhantomData<TAction>,
    _task: PhantomData<TTask>,
}
impl<T: Expression, TOut, TAction, TTask> FormattedLogger<T, TOut, TAction, TTask>
where
    TOut: std::io::Write,
    TAction: FormattedLoggerAction<T>,
    TTask: TaskFactory<TAction, TTask>,
{
    pub fn new(output: TOut, prefix: impl Into<String>) -> Self {
        Self {
            prefix: prefix.into(),
            output,
            _expression: PhantomData,
            _action: PhantomData,
            _task: PhantomData,
        }
    }
}
impl<T: Expression, TAction, TTask> FormattedLogger<T, std::io::Stdout, TAction, TTask>
where
    TAction: FormattedLoggerAction<T>,
    TTask: TaskFactory<TAction, TTask>,
{
    pub fn stdout(prefix: impl Into<String>) -> Self {
        Self::new(std::io::stdout(), prefix)
    }
}
impl<T: Expression, TAction, TTask> Clone for FormattedLogger<T, std::io::Stdout, TAction, TTask>
where
    TAction: FormattedLoggerAction<T>,
    TTask: TaskFactory<TAction, TTask>,
{
    fn clone(&self) -> Self {
        Self {
            prefix: self.prefix.clone(),
            output: std::io::stdout(),
            _expression: PhantomData,
            _action: PhantomData,
            _task: PhantomData,
        }
    }
}
impl<T: Expression, TAction, TTask> FormattedLogger<T, std::io::Stderr, TAction, TTask>
where
    TAction: FormattedLoggerAction<T>,
    TTask: TaskFactory<TAction, TTask>,
{
    pub fn stderr(prefix: impl Into<String>) -> Self {
        Self::new(std::io::stderr(), prefix)
    }
}
impl<T: Expression, TAction, TTask> Clone for FormattedLogger<T, std::io::Stderr, TAction, TTask>
where
    TAction: FormattedLoggerAction<T>,
    TTask: TaskFactory<TAction, TTask>,
{
    fn clone(&self) -> Self {
        Self {
            prefix: self.prefix.clone(),
            output: std::io::stderr(),
            _expression: PhantomData,
            _action: PhantomData,
            _task: PhantomData,
        }
    }
}
impl<T: Expression, TOut, TAction, TTask> ActionLogger for FormattedLogger<T, TOut, TAction, TTask>
where
    TOut: std::io::Write,
    TAction: FormattedLoggerAction<T>,
    TTask: TaskFactory<TAction, TTask>,
{
    type Action = TAction;
    type Task = TTask;
    fn log(
        &mut self,
        operation: &SchedulerCommand<Self::Action, Self::Task>,
        _metadata: Option<&MessageData>,
        _context: Option<&MiddlewareContext>,
    ) {
        if let SchedulerCommand::Send(_pid, action) = operation {
            if let Some(message) = format_action_message(action) {
                let _ = writeln!(self.output, "[{}] {}", self.prefix, message);
            }
        }
    }
}

fn format_action_message<T: Expression, TAction: FormattedLoggerAction<T>>(
    action: &TAction,
) -> Option<String> {
    if let Option::<&InitPrometheusMetricsAction>::Some(action) = action.match_type() {
        Some(format!(
            "Publishing to Prometheus metrics endpoint http://{}/",
            action.address,
        ))
    } else if let Option::<&InitOpenTelemetryAction>::Some(action) = action.match_type() {
        Some(format!(
            "Publishing to OpenTelemetry {} collector {}",
            match &action.config {
                OpenTelemetryConfig::Http(_) => "HTTP",
                OpenTelemetryConfig::Grpc(_) => "gRPC",
            },
            match &action.config {
                OpenTelemetryConfig::Http(config) => &config.endpoint,
                OpenTelemetryConfig::Grpc(config) => &config.endpoint,
            },
        ))
    } else if let Option::<&InitGraphRootAction>::Some(action) = action.match_type() {
        Some(format!(
            "Graph root compiled in {:?}",
            action.compiler_duration,
        ))
    } else if let Option::<&InitHttpServerAction>::Some(action) = action.match_type() {
        Some(format!(
            "Listening for incoming GraphQL HTTP requests on http://{}/",
            action.address,
        ))
    } else if let Option::<&OpenTelemetryMiddlewareErrorAction>::Some(action) = action.match_type()
    {
        Some(format!("OpenTelemetry error: {}", action.error))
    } else if let Option::<&GraphQlServerSubscribeAction<T>>::Some(action) = action.match_type() {
        Some(format!(
            "Handling GraphQL operation: {}",
            action.operation.operation_name().unwrap_or("<anonymous>"),
        ))
    } else if let Option::<&GraphQlHandlerWebSocketConnectSuccessAction>::Some(action) =
        action.match_type()
    {
        Some(format!(
            "Connected to GraphQL WebSocket server {}",
            action.url,
        ))
    } else if let Option::<&GraphQlHandlerWebSocketConnectionErrorAction>::Some(action) =
        action.match_type()
    {
        Some(format!(
            "GraphQL WebSocket connection error {}: {}",
            action.url, action.message,
        ))
    } else if let Option::<&GrpcHandlerConnectSuccessAction>::Some(action) = action.match_type() {
        Some(format!("Connected to gRPC server {}", action.url,))
    } else if let Option::<&GrpcHandlerConnectErrorAction>::Some(action) = action.match_type() {
        Some(format!(
            "Failed to connect to gRPC server {}: {}",
            action.url, action.message,
        ))
    } else if let Option::<&GrpcHandlerTransportErrorAction>::Some(action) = action.match_type() {
        Some(format!(
            "gRPC server transport error: {}.{} Error {}: {}: {}",
            action.service_name,
            action.method_name,
            action.status.code as i32,
            action.status.code,
            action.status.message,
        ))
    } else if let Option::<&EffectSubscribeAction<T>>::Some(action) = action.match_type() {
        if action.effect_type.as_str() != EFFECT_TYPE_EVALUATE {
            Some(format!(
                "Effect subscribed: {}{}",
                action.effect_type,
                if action.effects.len() > 1 {
                    format!(" x {}", action.effects.len())
                } else {
                    String::new()
                },
            ))
        } else {
            None
        }
    } else if let Option::<&EffectUnsubscribeAction<T>>::Some(action) = action.match_type() {
        if action.effect_type.as_str() != EFFECT_TYPE_EVALUATE {
            Some(format!(
                "Effect unsubscribed: {}{}",
                action.effect_type,
                if action.effects.len() > 1 {
                    format!(" x {}", action.effects.len())
                } else {
                    String::new()
                },
            ))
        } else {
            None
        }
    } else if let Option::<&EffectEmitAction<T>>::Some(action) = action.match_type() {
        if action.effect_types.len() > 1 {
            Some(format!(
                "Effect emitted:\n{}",
                action
                    .effect_types
                    .iter()
                    .map(|batch| format!(" {} x {}", batch.effect_type, batch.updates.len()))
                    .collect::<Vec<_>>()
                    .join("\n")
            ))
        } else if let Some(batch) = action.effect_types.iter().next() {
            Some(format!(
                "Effect emitted: {} x {}",
                batch.effect_type,
                batch.updates.len()
            ))
        } else {
            None
        }
    } else {
        None
    }
}
