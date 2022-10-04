// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use reflex::core::Expression;
use reflex_dispatcher::{Action, NamedAction, SerializableAction, SerializedAction};
use reflex_grpc::action::*;
use reflex_handlers::action::graphql::*;
use reflex_runtime::action::{
    bytecode_interpreter::*, effect::*, evaluate::*, query::*, RuntimeActions,
};
use serde::{Deserialize, Serialize};

use crate::server::action::{
    graphql_server::*,
    http_server::*,
    init::*,
    opentelemetry::*,
    query_inspector_server::{
        QueryInspectorServerActions, QueryInspectorServerHttpRequestAction,
        QueryInspectorServerHttpResponseAction,
    },
    telemetry::*,
    websocket_server::*,
};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum ServerCliAction<T: Expression> {
    #[serde(bound(
        serialize = "<T as Expression>::Signal<T>: Serialize",
        deserialize = "<T as Expression>::Signal<T>: Deserialize<'de>"
    ))]
    Runtime(RuntimeActions<T>),
    HttpServer(HttpServerActions),
    WebSocketServer(WebSocketServerActions),
    GraphQlServer(GraphQlServerActions<T>),
    BytecodeInterpreter(BytecodeInterpreterActions<T>),
    QueryInspectorServer(QueryInspectorServerActions),
    TelemetryMiddleware(TelemetryMiddlewareActions),
    OpenTelemetryMiddleware(OpenTelemetryMiddlewareActions),
    GraphQlHandler(GraphQlHandlerActions),
    GrpcHandler(GrpcHandlerActions),
    Init(InitActions),
}
impl<T: Expression> Action for ServerCliAction<T> {}
impl<T: Expression> NamedAction for ServerCliAction<T> {
    fn name(&self) -> &'static str {
        match self {
            Self::Runtime(action) => action.name(),
            Self::HttpServer(action) => action.name(),
            Self::WebSocketServer(action) => action.name(),
            Self::GraphQlServer(action) => action.name(),
            Self::BytecodeInterpreter(action) => action.name(),
            Self::QueryInspectorServer(action) => action.name(),
            Self::TelemetryMiddleware(action) => action.name(),
            Self::OpenTelemetryMiddleware(action) => action.name(),
            Self::GraphQlHandler(action) => action.name(),
            Self::GrpcHandler(action) => action.name(),
            Self::Init(action) => action.name(),
        }
    }
}
impl<T: Expression> SerializableAction for ServerCliAction<T> {
    fn to_json(&self) -> SerializedAction {
        match self {
            Self::Runtime(action) => action.to_json(),
            Self::HttpServer(action) => action.to_json(),
            Self::WebSocketServer(action) => action.to_json(),
            Self::GraphQlServer(action) => action.to_json(),
            Self::BytecodeInterpreter(action) => action.to_json(),
            Self::QueryInspectorServer(action) => action.to_json(),
            Self::TelemetryMiddleware(action) => action.to_json(),
            Self::OpenTelemetryMiddleware(action) => action.to_json(),
            Self::GraphQlHandler(action) => action.to_json(),
            Self::GrpcHandler(action) => action.to_json(),
            Self::Init(action) => action.to_json(),
        }
    }
}

impl<T: Expression> From<RuntimeActions<T>> for ServerCliAction<T> {
    fn from(value: RuntimeActions<T>) -> Self {
        Self::Runtime(value)
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<RuntimeActions<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::Runtime(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a RuntimeActions<T>> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::Runtime(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<HttpServerActions> for ServerCliAction<T> {
    fn from(value: HttpServerActions) -> Self {
        Self::HttpServer(value)
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<HttpServerActions> {
    fn from(value: ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::HttpServer(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a HttpServerActions> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::HttpServer(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<WebSocketServerActions> for ServerCliAction<T> {
    fn from(value: WebSocketServerActions) -> Self {
        Self::WebSocketServer(value)
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<WebSocketServerActions> {
    fn from(value: ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::WebSocketServer(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a WebSocketServerActions> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::WebSocketServer(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<GraphQlServerActions<T>> for ServerCliAction<T> {
    fn from(value: GraphQlServerActions<T>) -> Self {
        Self::GraphQlServer(value)
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<GraphQlServerActions<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::GraphQlServer(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a GraphQlServerActions<T>> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::GraphQlServer(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<BytecodeInterpreterActions<T>> for ServerCliAction<T> {
    fn from(value: BytecodeInterpreterActions<T>) -> Self {
        Self::BytecodeInterpreter(value)
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<BytecodeInterpreterActions<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::BytecodeInterpreter(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a BytecodeInterpreterActions<T>> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::BytecodeInterpreter(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<QueryInspectorServerActions> for ServerCliAction<T> {
    fn from(value: QueryInspectorServerActions) -> Self {
        Self::QueryInspectorServer(value)
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<QueryInspectorServerActions> {
    fn from(value: ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::QueryInspectorServer(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a QueryInspectorServerActions> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::QueryInspectorServer(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<TelemetryMiddlewareActions> for ServerCliAction<T> {
    fn from(value: TelemetryMiddlewareActions) -> Self {
        Self::TelemetryMiddleware(value)
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<TelemetryMiddlewareActions> {
    fn from(value: ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::TelemetryMiddleware(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a TelemetryMiddlewareActions> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::TelemetryMiddleware(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<OpenTelemetryMiddlewareActions> for ServerCliAction<T> {
    fn from(value: OpenTelemetryMiddlewareActions) -> Self {
        Self::OpenTelemetryMiddleware(value)
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<OpenTelemetryMiddlewareActions> {
    fn from(value: ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::OpenTelemetryMiddleware(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>>
    for Option<&'a OpenTelemetryMiddlewareActions>
{
    fn from(value: &'a ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::OpenTelemetryMiddleware(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<GraphQlHandlerActions> for ServerCliAction<T> {
    fn from(value: GraphQlHandlerActions) -> Self {
        Self::GraphQlHandler(value)
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<GraphQlHandlerActions> {
    fn from(value: ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::GraphQlHandler(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a GraphQlHandlerActions> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::GraphQlHandler(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<GrpcHandlerActions> for ServerCliAction<T> {
    fn from(value: GrpcHandlerActions) -> Self {
        Self::GrpcHandler(value)
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<GrpcHandlerActions> {
    fn from(value: ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::GrpcHandler(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a GrpcHandlerActions> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::GrpcHandler(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<InitActions> for ServerCliAction<T> {
    fn from(value: InitActions) -> Self {
        Self::Init(value)
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<InitActions> {
    fn from(value: ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::Init(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a InitActions> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::Init(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<EffectActions<T>> for ServerCliAction<T> {
    fn from(value: EffectActions<T>) -> Self {
        RuntimeActions::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<EffectActions<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<RuntimeActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a EffectActions<T>> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a RuntimeActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EffectSubscribeAction<T>> for ServerCliAction<T> {
    fn from(value: EffectSubscribeAction<T>) -> Self {
        EffectActions::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<EffectSubscribeAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<EffectActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a EffectSubscribeAction<T>> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a EffectActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EffectUnsubscribeAction<T>> for ServerCliAction<T> {
    fn from(value: EffectUnsubscribeAction<T>) -> Self {
        EffectActions::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<EffectUnsubscribeAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<EffectActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a EffectUnsubscribeAction<T>> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a EffectActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EffectEmitAction<T>> for ServerCliAction<T> {
    fn from(value: EffectEmitAction<T>) -> Self {
        EffectActions::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<EffectEmitAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<EffectActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a EffectEmitAction<T>> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a EffectActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EvaluateActions<T>> for ServerCliAction<T> {
    fn from(value: EvaluateActions<T>) -> Self {
        RuntimeActions::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<EvaluateActions<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<RuntimeActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a EvaluateActions<T>> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a RuntimeActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EvaluateStartAction<T>> for ServerCliAction<T> {
    fn from(value: EvaluateStartAction<T>) -> Self {
        EvaluateActions::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<EvaluateStartAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<EvaluateActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a EvaluateStartAction<T>> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a EvaluateActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EvaluateUpdateAction<T>> for ServerCliAction<T> {
    fn from(value: EvaluateUpdateAction<T>) -> Self {
        EvaluateActions::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<EvaluateUpdateAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<EvaluateActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a EvaluateUpdateAction<T>> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a EvaluateActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EvaluateStopAction> for ServerCliAction<T> {
    fn from(value: EvaluateStopAction) -> Self {
        EvaluateActions::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<EvaluateStopAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<EvaluateActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a EvaluateStopAction> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a EvaluateActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EvaluateResultAction<T>> for ServerCliAction<T> {
    fn from(value: EvaluateResultAction<T>) -> Self {
        EvaluateActions::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<EvaluateResultAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<EvaluateActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a EvaluateResultAction<T>> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a EvaluateActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<QueryActions<T>> for ServerCliAction<T> {
    fn from(value: QueryActions<T>) -> Self {
        RuntimeActions::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<QueryActions<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<RuntimeActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a QueryActions<T>> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a RuntimeActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<QuerySubscribeAction<T>> for ServerCliAction<T> {
    fn from(value: QuerySubscribeAction<T>) -> Self {
        QueryActions::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<QuerySubscribeAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<QueryActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a QuerySubscribeAction<T>> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a QueryActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<QueryUnsubscribeAction<T>> for ServerCliAction<T> {
    fn from(value: QueryUnsubscribeAction<T>) -> Self {
        QueryActions::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<QueryUnsubscribeAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<QueryActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a QueryUnsubscribeAction<T>> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a QueryActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<QueryEmitAction<T>> for ServerCliAction<T> {
    fn from(value: QueryEmitAction<T>) -> Self {
        QueryActions::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<QueryEmitAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<QueryActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a QueryEmitAction<T>> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a QueryActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<HttpServerRequestAction> for ServerCliAction<T> {
    fn from(value: HttpServerRequestAction) -> Self {
        HttpServerActions::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<HttpServerRequestAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<HttpServerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a HttpServerRequestAction> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a HttpServerActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<HttpServerResponseAction> for ServerCliAction<T> {
    fn from(value: HttpServerResponseAction) -> Self {
        HttpServerActions::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<HttpServerResponseAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<HttpServerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a HttpServerResponseAction> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a HttpServerActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<WebSocketServerConnectAction> for ServerCliAction<T> {
    fn from(value: WebSocketServerConnectAction) -> Self {
        WebSocketServerActions::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<WebSocketServerConnectAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<WebSocketServerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a WebSocketServerConnectAction> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a WebSocketServerActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<WebSocketServerReceiveAction> for ServerCliAction<T> {
    fn from(value: WebSocketServerReceiveAction) -> Self {
        WebSocketServerActions::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<WebSocketServerReceiveAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<WebSocketServerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a WebSocketServerReceiveAction> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a WebSocketServerActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<WebSocketServerSendAction> for ServerCliAction<T> {
    fn from(value: WebSocketServerSendAction) -> Self {
        WebSocketServerActions::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<WebSocketServerSendAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<WebSocketServerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a WebSocketServerSendAction> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a WebSocketServerActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<WebSocketServerDisconnectAction> for ServerCliAction<T> {
    fn from(value: WebSocketServerDisconnectAction) -> Self {
        WebSocketServerActions::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<WebSocketServerDisconnectAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<WebSocketServerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>>
    for Option<&'a WebSocketServerDisconnectAction>
{
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a WebSocketServerActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<WebSocketServerThrottleTimeoutAction> for ServerCliAction<T> {
    fn from(value: WebSocketServerThrottleTimeoutAction) -> Self {
        WebSocketServerActions::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<WebSocketServerThrottleTimeoutAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<WebSocketServerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>>
    for Option<&'a WebSocketServerThrottleTimeoutAction>
{
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a WebSocketServerActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GraphQlServerSubscribeAction<T>> for ServerCliAction<T> {
    fn from(value: GraphQlServerSubscribeAction<T>) -> Self {
        GraphQlServerActions::<T>::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<GraphQlServerSubscribeAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<GraphQlServerActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>>
    for Option<&'a GraphQlServerSubscribeAction<T>>
{
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a GraphQlServerActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GraphQlServerUnsubscribeAction<T>> for ServerCliAction<T> {
    fn from(value: GraphQlServerUnsubscribeAction<T>) -> Self {
        GraphQlServerActions::<T>::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<GraphQlServerUnsubscribeAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<GraphQlServerActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>>
    for Option<&'a GraphQlServerUnsubscribeAction<T>>
{
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a GraphQlServerActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GraphQlServerModifyAction<T>> for ServerCliAction<T> {
    fn from(value: GraphQlServerModifyAction<T>) -> Self {
        GraphQlServerActions::<T>::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<GraphQlServerModifyAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<GraphQlServerActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a GraphQlServerModifyAction<T>> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a GraphQlServerActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GraphQlServerParseSuccessAction<T>> for ServerCliAction<T> {
    fn from(value: GraphQlServerParseSuccessAction<T>) -> Self {
        GraphQlServerActions::<T>::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<GraphQlServerParseSuccessAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<GraphQlServerActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>>
    for Option<&'a GraphQlServerParseSuccessAction<T>>
{
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a GraphQlServerActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GraphQlServerParseErrorAction<T>> for ServerCliAction<T> {
    fn from(value: GraphQlServerParseErrorAction<T>) -> Self {
        GraphQlServerActions::<T>::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<GraphQlServerParseErrorAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<GraphQlServerActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>>
    for Option<&'a GraphQlServerParseErrorAction<T>>
{
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a GraphQlServerActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GraphQlServerEmitAction<T>> for ServerCliAction<T> {
    fn from(value: GraphQlServerEmitAction<T>) -> Self {
        GraphQlServerActions::<T>::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<GraphQlServerEmitAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<GraphQlServerActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a GraphQlServerEmitAction<T>> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a GraphQlServerActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<BytecodeInterpreterEvaluateAction<T>> for ServerCliAction<T> {
    fn from(value: BytecodeInterpreterEvaluateAction<T>) -> Self {
        BytecodeInterpreterActions::<T>::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<BytecodeInterpreterEvaluateAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<BytecodeInterpreterActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>>
    for Option<&'a BytecodeInterpreterEvaluateAction<T>>
{
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a BytecodeInterpreterActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<BytecodeInterpreterResultAction<T>> for ServerCliAction<T> {
    fn from(value: BytecodeInterpreterResultAction<T>) -> Self {
        BytecodeInterpreterActions::<T>::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<BytecodeInterpreterResultAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<BytecodeInterpreterActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>>
    for Option<&'a BytecodeInterpreterResultAction<T>>
{
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a BytecodeInterpreterActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<BytecodeInterpreterGcAction> for ServerCliAction<T> {
    fn from(value: BytecodeInterpreterGcAction) -> Self {
        BytecodeInterpreterActions::<T>::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<BytecodeInterpreterGcAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<BytecodeInterpreterActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a BytecodeInterpreterGcAction> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a BytecodeInterpreterActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<QueryInspectorServerHttpRequestAction> for ServerCliAction<T> {
    fn from(value: QueryInspectorServerHttpRequestAction) -> Self {
        QueryInspectorServerActions::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<QueryInspectorServerHttpRequestAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<QueryInspectorServerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>>
    for Option<&'a QueryInspectorServerHttpRequestAction>
{
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a QueryInspectorServerActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<QueryInspectorServerHttpResponseAction> for ServerCliAction<T> {
    fn from(value: QueryInspectorServerHttpResponseAction) -> Self {
        QueryInspectorServerActions::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<QueryInspectorServerHttpResponseAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<QueryInspectorServerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>>
    for Option<&'a QueryInspectorServerHttpResponseAction>
{
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a QueryInspectorServerActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<TelemetryMiddlewareTransactionStartAction> for ServerCliAction<T> {
    fn from(value: TelemetryMiddlewareTransactionStartAction) -> Self {
        TelemetryMiddlewareActions::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<TelemetryMiddlewareTransactionStartAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<TelemetryMiddlewareActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>>
    for Option<&'a TelemetryMiddlewareTransactionStartAction>
{
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a TelemetryMiddlewareActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<TelemetryMiddlewareTransactionEndAction> for ServerCliAction<T> {
    fn from(value: TelemetryMiddlewareTransactionEndAction) -> Self {
        TelemetryMiddlewareActions::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<TelemetryMiddlewareTransactionEndAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<TelemetryMiddlewareActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>>
    for Option<&'a TelemetryMiddlewareTransactionEndAction>
{
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a TelemetryMiddlewareActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<OpenTelemetryMiddlewareErrorAction> for ServerCliAction<T> {
    fn from(value: OpenTelemetryMiddlewareErrorAction) -> Self {
        OpenTelemetryMiddlewareActions::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<OpenTelemetryMiddlewareErrorAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<OpenTelemetryMiddlewareActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>>
    for Option<&'a OpenTelemetryMiddlewareErrorAction>
{
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a OpenTelemetryMiddlewareActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GraphQlHandlerWebSocketConnectSuccessAction> for ServerCliAction<T> {
    fn from(value: GraphQlHandlerWebSocketConnectSuccessAction) -> Self {
        GraphQlHandlerActions::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>>
    for Option<GraphQlHandlerWebSocketConnectSuccessAction>
{
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<GraphQlHandlerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>>
    for Option<&'a GraphQlHandlerWebSocketConnectSuccessAction>
{
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a GraphQlHandlerActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GraphQlHandlerWebSocketConnectErrorAction> for ServerCliAction<T> {
    fn from(value: GraphQlHandlerWebSocketConnectErrorAction) -> Self {
        GraphQlHandlerActions::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<GraphQlHandlerWebSocketConnectErrorAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<GraphQlHandlerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>>
    for Option<&'a GraphQlHandlerWebSocketConnectErrorAction>
{
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a GraphQlHandlerActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GraphQlHandlerWebSocketServerMessageAction> for ServerCliAction<T> {
    fn from(value: GraphQlHandlerWebSocketServerMessageAction) -> Self {
        GraphQlHandlerActions::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>>
    for Option<GraphQlHandlerWebSocketServerMessageAction>
{
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<GraphQlHandlerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>>
    for Option<&'a GraphQlHandlerWebSocketServerMessageAction>
{
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a GraphQlHandlerActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GrpcHandlerConnectSuccessAction> for ServerCliAction<T> {
    fn from(value: GrpcHandlerConnectSuccessAction) -> Self {
        GrpcHandlerActions::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<GrpcHandlerConnectSuccessAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<GrpcHandlerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>>
    for Option<&'a GrpcHandlerConnectSuccessAction>
{
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a GrpcHandlerActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GrpcHandlerConnectErrorAction> for ServerCliAction<T> {
    fn from(value: GrpcHandlerConnectErrorAction) -> Self {
        GrpcHandlerActions::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<GrpcHandlerConnectErrorAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<GrpcHandlerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a GrpcHandlerConnectErrorAction> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a GrpcHandlerActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<InitPrometheusMetricsAction> for ServerCliAction<T> {
    fn from(value: InitPrometheusMetricsAction) -> Self {
        InitActions::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<InitPrometheusMetricsAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<InitActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a InitPrometheusMetricsAction> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a InitActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<InitOpenTelemetryAction> for ServerCliAction<T> {
    fn from(value: InitOpenTelemetryAction) -> Self {
        InitActions::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<InitOpenTelemetryAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<InitActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a InitOpenTelemetryAction> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a InitActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<InitSessionRecordingAction> for ServerCliAction<T> {
    fn from(value: InitSessionRecordingAction) -> Self {
        InitActions::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<InitSessionRecordingAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<InitActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a InitSessionRecordingAction> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a InitActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<InitGraphRootAction> for ServerCliAction<T> {
    fn from(value: InitGraphRootAction) -> Self {
        InitActions::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<InitGraphRootAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<InitActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a InitGraphRootAction> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a InitActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<InitHttpServerAction> for ServerCliAction<T> {
    fn from(value: InitHttpServerAction) -> Self {
        InitActions::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<InitHttpServerAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<InitActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a InitHttpServerAction> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a InitActions>::from(value).and_then(|value| value.into())
    }
}
