// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::Expression;
use reflex_dispatcher::{Action, NamedAction, SerializableAction, SerializedAction};
use reflex_handlers::action::{graphql::*, grpc::*};
use reflex_runtime::action::{effect::*, evaluate::*, query::*, RuntimeAction};

use crate::{
    middleware::action::{
        opentelemetry::{OpenTelemetryMiddlewareAction, OpenTelemetryMiddlewareErrorAction},
        telemetry::*,
    },
    server::action::{graphql_server::*, http_server::*, init::*, websocket_server::*},
};

#[derive(Clone, Debug)]
pub enum ServerCliAction<T: Expression> {
    Runtime(RuntimeAction<T>),
    HttpServer(HttpServerAction),
    WebSocketServer(WebSocketServerAction),
    GraphQlServer(GraphQlServerAction<T>),
    TelemetryMiddleware(TelemetryMiddlewareAction),
    OpenTelemetryMiddleware(OpenTelemetryMiddlewareAction),
    GraphQlHandler(GraphQlHandlerAction),
    GrpcHandler(GrpcHandlerAction),
    Init(InitAction),
}
impl<T: Expression> Action for ServerCliAction<T> {}
impl<T: Expression> NamedAction for ServerCliAction<T> {
    fn name(&self) -> &'static str {
        match self {
            Self::Runtime(action) => action.name(),
            Self::HttpServer(action) => action.name(),
            Self::WebSocketServer(action) => action.name(),
            Self::GraphQlServer(action) => action.name(),
            Self::TelemetryMiddleware(action) => action.name(),
            Self::OpenTelemetryMiddleware(action) => action.name(),
            Self::GraphQlHandler(action) => action.name(),
            Self::GrpcHandler(action) => action.name(),
            Self::Init(action) => action.name(),
        }
    }
}
impl<T: Expression> SerializableAction for ServerCliAction<T> {
    fn serialize(&self) -> SerializedAction {
        match self {
            Self::Runtime(action) => action.serialize(),
            Self::HttpServer(action) => action.serialize(),
            Self::WebSocketServer(action) => action.serialize(),
            Self::GraphQlServer(action) => action.serialize(),
            Self::TelemetryMiddleware(action) => action.serialize(),
            Self::OpenTelemetryMiddleware(action) => action.serialize(),
            Self::GraphQlHandler(action) => action.serialize(),
            Self::GrpcHandler(action) => action.serialize(),
            Self::Init(action) => action.serialize(),
        }
    }
}

impl<T: Expression> From<RuntimeAction<T>> for ServerCliAction<T> {
    fn from(value: RuntimeAction<T>) -> Self {
        Self::Runtime(value)
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<RuntimeAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::Runtime(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a RuntimeAction<T>> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::Runtime(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<HttpServerAction> for ServerCliAction<T> {
    fn from(value: HttpServerAction) -> Self {
        Self::HttpServer(value)
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<HttpServerAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::HttpServer(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a HttpServerAction> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::HttpServer(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<WebSocketServerAction> for ServerCliAction<T> {
    fn from(value: WebSocketServerAction) -> Self {
        Self::WebSocketServer(value)
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<WebSocketServerAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::WebSocketServer(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a WebSocketServerAction> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::WebSocketServer(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<GraphQlServerAction<T>> for ServerCliAction<T> {
    fn from(value: GraphQlServerAction<T>) -> Self {
        Self::GraphQlServer(value)
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<GraphQlServerAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::GraphQlServer(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a GraphQlServerAction<T>> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::GraphQlServer(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<TelemetryMiddlewareAction> for ServerCliAction<T> {
    fn from(value: TelemetryMiddlewareAction) -> Self {
        Self::TelemetryMiddleware(value)
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<TelemetryMiddlewareAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::TelemetryMiddleware(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a TelemetryMiddlewareAction> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::TelemetryMiddleware(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<OpenTelemetryMiddlewareAction> for ServerCliAction<T> {
    fn from(value: OpenTelemetryMiddlewareAction) -> Self {
        Self::OpenTelemetryMiddleware(value)
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<OpenTelemetryMiddlewareAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::OpenTelemetryMiddleware(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a OpenTelemetryMiddlewareAction> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::OpenTelemetryMiddleware(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<GraphQlHandlerAction> for ServerCliAction<T> {
    fn from(value: GraphQlHandlerAction) -> Self {
        Self::GraphQlHandler(value)
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<GraphQlHandlerAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::GraphQlHandler(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a GraphQlHandlerAction> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::GraphQlHandler(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<GrpcHandlerAction> for ServerCliAction<T> {
    fn from(value: GrpcHandlerAction) -> Self {
        Self::GrpcHandler(value)
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<GrpcHandlerAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::GrpcHandler(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a GrpcHandlerAction> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::GrpcHandler(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<InitAction> for ServerCliAction<T> {
    fn from(value: InitAction) -> Self {
        Self::Init(value)
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<InitAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::Init(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a InitAction> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        match value {
            ServerCliAction::Init(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<EffectAction<T>> for ServerCliAction<T> {
    fn from(value: EffectAction<T>) -> Self {
        RuntimeAction::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<EffectAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<RuntimeAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a EffectAction<T>> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a RuntimeAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EffectSubscribeAction<T>> for ServerCliAction<T> {
    fn from(value: EffectSubscribeAction<T>) -> Self {
        EffectAction::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<EffectSubscribeAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<EffectAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a EffectSubscribeAction<T>> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a EffectAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EffectUnsubscribeAction<T>> for ServerCliAction<T> {
    fn from(value: EffectUnsubscribeAction<T>) -> Self {
        EffectAction::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<EffectUnsubscribeAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<EffectAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a EffectUnsubscribeAction<T>> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a EffectAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EffectEmitAction<T>> for ServerCliAction<T> {
    fn from(value: EffectEmitAction<T>) -> Self {
        EffectAction::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<EffectEmitAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<EffectAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a EffectEmitAction<T>> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a EffectAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EvaluateAction<T>> for ServerCliAction<T> {
    fn from(value: EvaluateAction<T>) -> Self {
        RuntimeAction::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<EvaluateAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<RuntimeAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a EvaluateAction<T>> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a RuntimeAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EvaluateStartAction<T>> for ServerCliAction<T> {
    fn from(value: EvaluateStartAction<T>) -> Self {
        EvaluateAction::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<EvaluateStartAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<EvaluateAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a EvaluateStartAction<T>> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a EvaluateAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EvaluateUpdateAction<T>> for ServerCliAction<T> {
    fn from(value: EvaluateUpdateAction<T>) -> Self {
        EvaluateAction::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<EvaluateUpdateAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<EvaluateAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a EvaluateUpdateAction<T>> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a EvaluateAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EvaluateStopAction> for ServerCliAction<T> {
    fn from(value: EvaluateStopAction) -> Self {
        EvaluateAction::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<EvaluateStopAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<EvaluateAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a EvaluateStopAction> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a EvaluateAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EvaluateResultAction<T>> for ServerCliAction<T> {
    fn from(value: EvaluateResultAction<T>) -> Self {
        EvaluateAction::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<EvaluateResultAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<EvaluateAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a EvaluateResultAction<T>> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a EvaluateAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<QueryAction<T>> for ServerCliAction<T> {
    fn from(value: QueryAction<T>) -> Self {
        RuntimeAction::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<QueryAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<RuntimeAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a QueryAction<T>> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a RuntimeAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<QuerySubscribeAction<T>> for ServerCliAction<T> {
    fn from(value: QuerySubscribeAction<T>) -> Self {
        QueryAction::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<QuerySubscribeAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<QueryAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a QuerySubscribeAction<T>> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a QueryAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<QueryUnsubscribeAction<T>> for ServerCliAction<T> {
    fn from(value: QueryUnsubscribeAction<T>) -> Self {
        QueryAction::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<QueryUnsubscribeAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<QueryAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a QueryUnsubscribeAction<T>> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a QueryAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<QueryEmitAction<T>> for ServerCliAction<T> {
    fn from(value: QueryEmitAction<T>) -> Self {
        QueryAction::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<QueryEmitAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<QueryAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a QueryEmitAction<T>> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a QueryAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<HttpServerRequestAction> for ServerCliAction<T> {
    fn from(value: HttpServerRequestAction) -> Self {
        HttpServerAction::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<HttpServerRequestAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<HttpServerAction>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a HttpServerRequestAction> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a HttpServerAction>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<HttpServerResponseAction> for ServerCliAction<T> {
    fn from(value: HttpServerResponseAction) -> Self {
        HttpServerAction::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<HttpServerResponseAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<HttpServerAction>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a HttpServerResponseAction> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a HttpServerAction>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<WebSocketServerConnectAction> for ServerCliAction<T> {
    fn from(value: WebSocketServerConnectAction) -> Self {
        WebSocketServerAction::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<WebSocketServerConnectAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<WebSocketServerAction>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a WebSocketServerConnectAction> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a WebSocketServerAction>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<WebSocketServerReceiveAction> for ServerCliAction<T> {
    fn from(value: WebSocketServerReceiveAction) -> Self {
        WebSocketServerAction::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<WebSocketServerReceiveAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<WebSocketServerAction>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a WebSocketServerReceiveAction> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a WebSocketServerAction>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<WebSocketServerSendAction> for ServerCliAction<T> {
    fn from(value: WebSocketServerSendAction) -> Self {
        WebSocketServerAction::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<WebSocketServerSendAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<WebSocketServerAction>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a WebSocketServerSendAction> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a WebSocketServerAction>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<WebSocketServerDisconnectAction> for ServerCliAction<T> {
    fn from(value: WebSocketServerDisconnectAction) -> Self {
        WebSocketServerAction::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<WebSocketServerDisconnectAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<WebSocketServerAction>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>>
    for Option<&'a WebSocketServerDisconnectAction>
{
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a WebSocketServerAction>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<WebSocketServerThrottleTimeoutAction> for ServerCliAction<T> {
    fn from(value: WebSocketServerThrottleTimeoutAction) -> Self {
        WebSocketServerAction::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<WebSocketServerThrottleTimeoutAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<WebSocketServerAction>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>>
    for Option<&'a WebSocketServerThrottleTimeoutAction>
{
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a WebSocketServerAction>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GraphQlServerSubscribeAction<T>> for ServerCliAction<T> {
    fn from(value: GraphQlServerSubscribeAction<T>) -> Self {
        GraphQlServerAction::<T>::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<GraphQlServerSubscribeAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<GraphQlServerAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>>
    for Option<&'a GraphQlServerSubscribeAction<T>>
{
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a GraphQlServerAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GraphQlServerUnsubscribeAction<T>> for ServerCliAction<T> {
    fn from(value: GraphQlServerUnsubscribeAction<T>) -> Self {
        GraphQlServerAction::<T>::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<GraphQlServerUnsubscribeAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<GraphQlServerAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>>
    for Option<&'a GraphQlServerUnsubscribeAction<T>>
{
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a GraphQlServerAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GraphQlServerModifyAction<T>> for ServerCliAction<T> {
    fn from(value: GraphQlServerModifyAction<T>) -> Self {
        GraphQlServerAction::<T>::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<GraphQlServerModifyAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<GraphQlServerAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a GraphQlServerModifyAction<T>> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a GraphQlServerAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GraphQlServerParseSuccessAction<T>> for ServerCliAction<T> {
    fn from(value: GraphQlServerParseSuccessAction<T>) -> Self {
        GraphQlServerAction::<T>::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<GraphQlServerParseSuccessAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<GraphQlServerAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>>
    for Option<&'a GraphQlServerParseSuccessAction<T>>
{
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a GraphQlServerAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GraphQlServerParseErrorAction<T>> for ServerCliAction<T> {
    fn from(value: GraphQlServerParseErrorAction<T>) -> Self {
        GraphQlServerAction::<T>::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<GraphQlServerParseErrorAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<GraphQlServerAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>>
    for Option<&'a GraphQlServerParseErrorAction<T>>
{
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a GraphQlServerAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GraphQlServerEmitAction<T>> for ServerCliAction<T> {
    fn from(value: GraphQlServerEmitAction<T>) -> Self {
        GraphQlServerAction::<T>::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<GraphQlServerEmitAction<T>> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<GraphQlServerAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a GraphQlServerEmitAction<T>> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a GraphQlServerAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<TelemetryMiddlewareTransactionStartAction> for ServerCliAction<T> {
    fn from(value: TelemetryMiddlewareTransactionStartAction) -> Self {
        TelemetryMiddlewareAction::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<TelemetryMiddlewareTransactionStartAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<TelemetryMiddlewareAction>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>>
    for Option<&'a TelemetryMiddlewareTransactionStartAction>
{
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a TelemetryMiddlewareAction>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<TelemetryMiddlewareTransactionEndAction> for ServerCliAction<T> {
    fn from(value: TelemetryMiddlewareTransactionEndAction) -> Self {
        TelemetryMiddlewareAction::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<TelemetryMiddlewareTransactionEndAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<TelemetryMiddlewareAction>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>>
    for Option<&'a TelemetryMiddlewareTransactionEndAction>
{
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a TelemetryMiddlewareAction>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<OpenTelemetryMiddlewareErrorAction> for ServerCliAction<T> {
    fn from(value: OpenTelemetryMiddlewareErrorAction) -> Self {
        OpenTelemetryMiddlewareAction::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<OpenTelemetryMiddlewareErrorAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<OpenTelemetryMiddlewareAction>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>>
    for Option<&'a OpenTelemetryMiddlewareErrorAction>
{
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a OpenTelemetryMiddlewareAction>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GraphQlHandlerWebSocketConnectSuccessAction> for ServerCliAction<T> {
    fn from(value: GraphQlHandlerWebSocketConnectSuccessAction) -> Self {
        GraphQlHandlerAction::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>>
    for Option<GraphQlHandlerWebSocketConnectSuccessAction>
{
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<GraphQlHandlerAction>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>>
    for Option<&'a GraphQlHandlerWebSocketConnectSuccessAction>
{
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a GraphQlHandlerAction>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GraphQlHandlerWebSocketConnectErrorAction> for ServerCliAction<T> {
    fn from(value: GraphQlHandlerWebSocketConnectErrorAction) -> Self {
        GraphQlHandlerAction::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<GraphQlHandlerWebSocketConnectErrorAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<GraphQlHandlerAction>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>>
    for Option<&'a GraphQlHandlerWebSocketConnectErrorAction>
{
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a GraphQlHandlerAction>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GraphQlHandlerWebSocketServerMessageAction> for ServerCliAction<T> {
    fn from(value: GraphQlHandlerWebSocketServerMessageAction) -> Self {
        GraphQlHandlerAction::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>>
    for Option<GraphQlHandlerWebSocketServerMessageAction>
{
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<GraphQlHandlerAction>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>>
    for Option<&'a GraphQlHandlerWebSocketServerMessageAction>
{
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a GraphQlHandlerAction>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GrpcHandlerConnectSuccessAction> for ServerCliAction<T> {
    fn from(value: GrpcHandlerConnectSuccessAction) -> Self {
        GrpcHandlerAction::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<GrpcHandlerConnectSuccessAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<GrpcHandlerAction>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>>
    for Option<&'a GrpcHandlerConnectSuccessAction>
{
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a GrpcHandlerAction>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GrpcHandlerConnectErrorAction> for ServerCliAction<T> {
    fn from(value: GrpcHandlerConnectErrorAction) -> Self {
        GrpcHandlerAction::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<GrpcHandlerConnectErrorAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<GrpcHandlerAction>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a GrpcHandlerConnectErrorAction> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a GrpcHandlerAction>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<InitPrometheusMetricsAction> for ServerCliAction<T> {
    fn from(value: InitPrometheusMetricsAction) -> Self {
        InitAction::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<InitPrometheusMetricsAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<InitAction>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a InitPrometheusMetricsAction> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a InitAction>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<InitOpenTelemetryAction> for ServerCliAction<T> {
    fn from(value: InitOpenTelemetryAction) -> Self {
        InitAction::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<InitOpenTelemetryAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<InitAction>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a InitOpenTelemetryAction> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a InitAction>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<InitGraphRootAction> for ServerCliAction<T> {
    fn from(value: InitGraphRootAction) -> Self {
        InitAction::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<InitGraphRootAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<InitAction>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a InitGraphRootAction> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a InitAction>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<InitHttpServerAction> for ServerCliAction<T> {
    fn from(value: InitHttpServerAction) -> Self {
        InitAction::from(value).into()
    }
}
impl<T: Expression> From<ServerCliAction<T>> for Option<InitHttpServerAction> {
    fn from(value: ServerCliAction<T>) -> Self {
        Option::<InitAction>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a ServerCliAction<T>> for Option<&'a InitHttpServerAction> {
    fn from(value: &'a ServerCliAction<T>) -> Self {
        Option::<&'a InitAction>::from(value).and_then(|value| value.into())
    }
}
