// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{hash_map::Entry, HashMap},
    iter::once,
    ops::Deref,
    time::Duration,
};

use futures::{future, stream, Future, FutureExt, Stream, StreamExt};
use hyper::{http::uri::PathAndQuery, Uri};
use reflex::core::Uuid;
use reflex_dispatcher::{
    Action, ActorEvents, BoxedActionStream, HandlerContext, Matcher, MessageData,
    NoopDisposeCallback, ProcessId, SchedulerCommand, SchedulerMode, SchedulerTransition,
    TaskFactory, TaskInbox,
};
use reflex_json::JsonValue;
use reflex_macros::{dispatcher, Named};
use reflex_protobuf::Bytes;
use tokio::task::JoinHandle;
use tokio_stream::wrappers::ReceiverStream;
use tonic::{
    metadata::MetadataMap,
    transport::{Channel, Endpoint},
    Code, Request, Status,
};

use crate::{
    action::{
        GrpcHandlerAbortRequestAction, GrpcHandlerConnectErrorAction,
        GrpcHandlerConnectSuccessAction, GrpcHandlerConnectionTerminateAction,
        GrpcHandlerErrorResponseAction, GrpcHandlerRequestStartAction,
        GrpcHandlerRequestStopAction, GrpcHandlerSuccessResponseAction,
        GrpcHandlerTransportErrorAction, GrpcMetadata, GrpcStatus,
    },
    codec::bytes::BytesCodec,
    utils::{get_transport_error, GrpcMethodName, GrpcServiceName},
};

#[derive(PartialEq, Eq, Clone, Copy, Hash, Debug)]
struct GrpcOperationId(Uuid);
impl GrpcOperationId {
    fn as_uuid(&self) -> Uuid {
        let Self(value) = self;
        *value
    }
}
impl From<Uuid> for GrpcOperationId {
    fn from(value: Uuid) -> Self {
        Self(value)
    }
}
impl std::fmt::Display for GrpcOperationId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.as_uuid(), f)
    }
}

pub trait GrpcHandlerConnectionTaskAction:
    GrpcHandlerConnectionTaskEventsAction + GrpcHandlerConnectionTaskActorAction
{
}
impl<_Self> GrpcHandlerConnectionTaskAction for _Self where
    Self: GrpcHandlerConnectionTaskEventsAction + GrpcHandlerConnectionTaskActorAction
{
}

pub trait GrpcHandlerConnectionTaskEventsAction:
    Matcher<GrpcHandlerRequestStartAction>
    + Matcher<GrpcHandlerRequestStopAction>
    + Matcher<GrpcHandlerAbortRequestAction>
    + Matcher<GrpcHandlerConnectionTerminateAction>
    + From<GrpcHandlerConnectErrorAction>
    + From<GrpcHandlerConnectSuccessAction>
    + From<GrpcHandlerSuccessResponseAction>
    + From<GrpcHandlerErrorResponseAction>
    + From<GrpcHandlerTransportErrorAction>
    + From<GrpcHandlerConnectionTerminateAction>
{
}
impl<_Self> GrpcHandlerConnectionTaskEventsAction for _Self where
    Self: Matcher<GrpcHandlerRequestStartAction>
        + Matcher<GrpcHandlerRequestStopAction>
        + Matcher<GrpcHandlerAbortRequestAction>
        + Matcher<GrpcHandlerConnectionTerminateAction>
        + From<GrpcHandlerConnectErrorAction>
        + From<GrpcHandlerConnectSuccessAction>
        + From<GrpcHandlerSuccessResponseAction>
        + From<GrpcHandlerErrorResponseAction>
        + From<GrpcHandlerTransportErrorAction>
        + From<GrpcHandlerConnectionTerminateAction>
{
}

// TODO: Implement Serialize/Deserialize traits for GrpcHandlerConnectionTaskFactory
#[derive(Named, Clone)]
pub struct GrpcHandlerConnectionTaskFactory {
    pub connection_id: Uuid,
    pub endpoint: tonic::transport::Endpoint,
    pub delay: Option<Duration>,
    pub caller_pid: ProcessId,
}

impl<TAction, TTask> TaskFactory<TAction, TTask> for GrpcHandlerConnectionTaskFactory
where
    TAction: Action + GrpcHandlerConnectionTaskAction + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    type Actor = GrpcHandlerConnectionTaskActor;
    fn create(self) -> Self::Actor {
        let Self {
            connection_id,
            endpoint,
            delay,
            caller_pid,
        } = self;
        GrpcHandlerConnectionTaskActor {
            connection_id,
            endpoint,
            delay,
            caller_pid,
        }
    }
}

#[derive(Clone, Debug)]
enum GrpcClientMessage {
    RequestStart(GrpcOperationId, GrpcClientRequestStartMessage),
    RequestStop(GrpcOperationId),
    InvalidRequest(GrpcOperationId, GrpcClientInvalidRequestMessage),
    ConnectionTerminate,
}

#[derive(Clone, Debug)]
struct GrpcClientRequestStartMessage {
    service_name: GrpcServiceName,
    method_name: GrpcMethodName,
    method_path: String,
    path: PathAndQuery,
    streaming: bool,
    input: JsonValue,
    message: Bytes,
    metadata: GrpcMetadata,
}

#[derive(Clone, Debug)]
struct GrpcClientInvalidRequestMessage {
    service_name: GrpcServiceName,
    method_name: GrpcMethodName,
    method_path: String,
    input: JsonValue,
    metadata: GrpcMetadata,
}

fn create_connection_command_stream<TAction>(
    inbox: impl TaskInbox<TAction>,
) -> impl Stream<Item = GrpcClientMessage> + Unpin
where
    TAction: Action
        + Matcher<GrpcHandlerRequestStartAction>
        + Matcher<GrpcHandlerRequestStopAction>
        + Matcher<GrpcHandlerAbortRequestAction>
        + Matcher<GrpcHandlerConnectionTerminateAction>,
{
    inbox.filter_map(|message| {
        let action = message.deref();
        let result = if let Some(GrpcHandlerRequestStartAction {
            operation_id,
            service_name,
            method_name,
            method_path,
            streaming,
            input,
            metadata,
            message,
            ..
        }) = action.match_type()
        {
            let operation_id = GrpcOperationId::from(*operation_id);
            let service_name = GrpcServiceName(service_name.clone());
            let method_name = GrpcMethodName(method_name.clone());
            let method_path = method_path.clone();
            let input = input.clone();
            let metadata = metadata.clone();
            let streaming = *streaming;
            let message = message.clone();
            match PathAndQuery::try_from(&method_path) {
                Err(_) => Some(GrpcClientMessage::InvalidRequest(
                    operation_id,
                    GrpcClientInvalidRequestMessage {
                        service_name,
                        method_name,
                        method_path,
                        input,
                        metadata,
                    },
                )),
                Ok(path) => Some(GrpcClientMessage::RequestStart(
                    operation_id,
                    GrpcClientRequestStartMessage {
                        service_name,
                        method_name,
                        method_path,
                        path,
                        streaming,
                        input,
                        message,
                        metadata,
                    },
                )),
            }
        } else if let Some(GrpcHandlerRequestStopAction { operation_id, .. }) = action.match_type()
        {
            let operation_id = GrpcOperationId(*operation_id);
            Some(GrpcClientMessage::RequestStop(operation_id))
        } else if let Some(GrpcHandlerAbortRequestAction { operation_id, .. }) = action.match_type()
        {
            let operation_id = GrpcOperationId(*operation_id);
            Some(GrpcClientMessage::RequestStop(operation_id))
        } else if let Some(GrpcHandlerConnectionTerminateAction { .. }) = action.match_type() {
            Some(GrpcClientMessage::ConnectionTerminate)
        } else {
            None
        };
        future::ready(result)
    })
}

fn create_grpc_connection_results_stream<TAction>(
    connection_id: Uuid,
    uri: Uri,
    client: tonic::client::Grpc<Channel>,
    connection_commands: impl Stream<Item = GrpcClientMessage> + Unpin,
) -> (impl Future<Output = ()>, impl Stream<Item = TAction>)
where
    TAction: Action
        + From<GrpcHandlerSuccessResponseAction>
        + From<GrpcHandlerErrorResponseAction>
        + From<GrpcHandlerTransportErrorAction>
        + From<GrpcHandlerConnectionTerminateAction>
        + Send
        + 'static,
{
    let (actions_tx, actions_rx) = tokio::sync::mpsc::channel(1024);
    let send_task = {
        let mut active_streams = HashMap::<GrpcOperationId, JoinHandle<()>>::default();
        let mut client_messages = connection_commands;
        async move {
            while let Some(message) = client_messages.next().await {
                match message {
                    GrpcClientMessage::InvalidRequest(operation_id, message) => {
                        let GrpcClientInvalidRequestMessage {
                            service_name,
                            method_name,
                            method_path,
                            input,
                            metadata,
                        } = message;
                        let status = GrpcStatus {
                            code: Code::NotFound,
                            message: format!("Invalid method: {}", method_name.as_str()),
                        };
                        let _ = actions_tx
                            .send(
                                GrpcHandlerErrorResponseAction {
                                    connection_id,
                                    operation_id: operation_id.as_uuid(),
                                    url: uri.to_string(),
                                    service_name: service_name.into_string(),
                                    method_name: method_name.into_string(),
                                    method_path,
                                    input,
                                    metadata,
                                    status,
                                }
                                .into(),
                            )
                            .await;
                    }
                    GrpcClientMessage::RequestStart(operation_id, message) => {
                        if let Entry::Vacant(entry) = active_streams.entry(operation_id) {
                            let GrpcClientRequestStartMessage {
                                service_name,
                                method_name,
                                method_path,
                                path,
                                streaming,
                                input,
                                message,
                                metadata,
                            } = message;
                            let results = execute_grpc_request(
                                client.clone(),
                                path,
                                message.clone(),
                                metadata.clone().into(),
                                streaming,
                            )
                            .map({
                                let uri = uri.clone();
                                move |result| match result {
                                    Ok(data) => {
                                        Ok(TAction::from(GrpcHandlerSuccessResponseAction {
                                            connection_id,
                                            operation_id: operation_id.as_uuid(),
                                            data,
                                        }))
                                    }
                                    Err(status) => Err({
                                        parse_grpc_operation_error(
                                            status,
                                            &service_name,
                                            &method_name,
                                            &method_path,
                                            connection_id,
                                            &operation_id,
                                            &uri,
                                            &input,
                                            &metadata,
                                        )
                                    }),
                                }
                            });
                            entry.insert(tokio::spawn({
                                let actions_tx = actions_tx.clone();
                                async move {
                                    let mut results = Box::pin(results);
                                    while let Some(result) = results.next().await {
                                        let is_err = result.is_err();
                                        let message = match result {
                                            Ok(action) => action,
                                            Err(action) => action,
                                        };
                                        let _ = actions_tx.send(message).await;
                                        if is_err {
                                            // End the task after relaying the error
                                            break;
                                        }
                                    }
                                }
                            }));
                        }
                    }
                    GrpcClientMessage::RequestStop(operation_id) => {
                        if let Some(handle) = active_streams.remove(&operation_id) {
                            handle.abort();
                        }
                    }
                    GrpcClientMessage::ConnectionTerminate => {
                        for (_operation_id, handle) in active_streams.drain() {
                            handle.abort();
                        }
                        let _ = actions_tx
                            .send(
                                GrpcHandlerConnectionTerminateAction {
                                    connection_id,
                                    url: uri.to_string(),
                                }
                                .into(),
                            )
                            .await;
                    }
                }
            }
        }
    };
    (send_task, ReceiverStream::new(actions_rx))
}

#[derive(Named, Clone)]
pub struct GrpcHandlerConnectionTaskActor {
    connection_id: Uuid,
    endpoint: tonic::transport::Endpoint,
    delay: Option<Duration>,
    caller_pid: ProcessId,
}

#[derive(PartialEq, Eq, Clone, Copy, Default)]
pub struct GrpcHandlerConnectionTaskActorState;

dispatcher!({
    pub enum GrpcHandlerConnectionTaskActorAction {
        Inbox(GrpcHandlerConnectSuccessAction),
        Inbox(GrpcHandlerConnectErrorAction),
        Inbox(GrpcHandlerSuccessResponseAction),
        Inbox(GrpcHandlerErrorResponseAction),
        Inbox(GrpcHandlerTransportErrorAction),
        Inbox(GrpcHandlerConnectionTerminateAction),

        Outbox(GrpcHandlerConnectSuccessAction),
        Outbox(GrpcHandlerConnectErrorAction),
        Outbox(GrpcHandlerSuccessResponseAction),
        Outbox(GrpcHandlerErrorResponseAction),
        Outbox(GrpcHandlerTransportErrorAction),
        Outbox(GrpcHandlerAbortRequestAction),
    }

    impl<TAction, TTask> Dispatcher<TAction, TTask> for GrpcHandlerConnectionTaskActor
    where
        TAction: Action + GrpcHandlerConnectionTaskEventsAction + Send + 'static,
        TTask: TaskFactory<TAction, TTask>,
    {
        type State = GrpcHandlerConnectionTaskActorState;
        type Events<TInbox: TaskInbox<TAction>> = BoxedActionStream<TInbox::Message>;
        type Dispose = NoopDisposeCallback;

        fn init(&self) -> Self::State {
            Default::default()
        }
        fn events<TInbox: TaskInbox<TAction>>(
            &self,
            inbox: TInbox,
        ) -> ActorEvents<TInbox, Self::Events<TInbox>, Self::Dispose> {
            ActorEvents::Async(Box::pin(self.events(inbox)), None)
        }

        fn accept(&self, _action: &GrpcHandlerConnectSuccessAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &GrpcHandlerConnectSuccessAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &GrpcHandlerConnectSuccessAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_grpc_handler_connect_success(state, action, metadata, context)
        }

        fn accept(&self, _action: &GrpcHandlerConnectErrorAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &GrpcHandlerConnectErrorAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &GrpcHandlerConnectErrorAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_grpc_handler_connect_error(state, action, metadata, context)
        }

        fn accept(&self, _action: &GrpcHandlerSuccessResponseAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &GrpcHandlerSuccessResponseAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &GrpcHandlerSuccessResponseAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_grpc_handler_success_response(state, action, metadata, context)
        }

        fn accept(&self, _action: &GrpcHandlerErrorResponseAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &GrpcHandlerErrorResponseAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &GrpcHandlerErrorResponseAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_grpc_handler_error_response(state, action, metadata, context)
        }

        fn accept(&self, _action: &GrpcHandlerTransportErrorAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &GrpcHandlerTransportErrorAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &GrpcHandlerTransportErrorAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_grpc_handler_transport_error(state, action, metadata, context)
        }

        fn accept(&self, _action: &GrpcHandlerConnectionTerminateAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &GrpcHandlerConnectionTerminateAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &GrpcHandlerConnectionTerminateAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_grpc_handler_connection_terminate(state, action, metadata, context)
        }
    }
});

impl GrpcHandlerConnectionTaskActor {
    fn events<TInbox, TAction>(&self, inbox: TInbox) -> impl Stream<Item = TInbox::Message>
    where
        TInbox: TaskInbox<TAction>,
        TAction: Action
            + Matcher<GrpcHandlerRequestStartAction>
            + Matcher<GrpcHandlerRequestStopAction>
            + Matcher<GrpcHandlerAbortRequestAction>
            + Matcher<GrpcHandlerConnectionTerminateAction>
            + From<GrpcHandlerConnectErrorAction>
            + From<GrpcHandlerConnectSuccessAction>
            + From<GrpcHandlerSuccessResponseAction>
            + From<GrpcHandlerErrorResponseAction>
            + From<GrpcHandlerTransportErrorAction>
            + From<GrpcHandlerConnectionTerminateAction>
            + Send
            + 'static,
    {
        let connection_commands = create_connection_command_stream(inbox);
        {
            let connection_id = self.connection_id;
            let endpoint = self.endpoint.clone();
            let delay = self.delay;
            let uri = endpoint.uri().clone();
            let connect_task = create_grpc_connection(endpoint, delay);
            let listen_task = async move {
                match connect_task.await {
                    Err(err) => {
                        let connection_status_action =
                            TAction::from(GrpcHandlerConnectErrorAction {
                                connection_id,
                                url: uri.to_string(),
                                message: format!("Connection error: {}", err),
                            });
                        stream::iter(once(connection_status_action)).left_stream()
                    }
                    Ok(client) => {
                        let connection_status_action =
                            TAction::from(GrpcHandlerConnectSuccessAction {
                                connection_id,
                                url: uri.to_string(),
                            });
                        let (send_task, results) = create_grpc_connection_results_stream(
                            connection_id,
                            uri,
                            client,
                            connection_commands,
                        );
                        // Merge the send task and results stream into a single combined stream
                        let combined_results_stream = stream::select(
                            send_task.into_stream().flat_map(|_| stream::empty()),
                            results,
                        );
                        stream::iter(once(connection_status_action))
                            .chain(combined_results_stream)
                            .right_stream()
                    }
                }
                .map(TInbox::Message::from)
            };
            listen_task.into_stream().flatten()
        }
    }
    fn handle_grpc_handler_connect_success<TAction, TTask>(
        &self,
        _state: &mut GrpcHandlerConnectionTaskActorState,
        _action: &GrpcHandlerConnectSuccessAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<GrpcHandlerConnectSuccessAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        Some(SchedulerTransition::new(once(SchedulerCommand::Forward(
            self.caller_pid,
        ))))
    }
    fn handle_grpc_handler_connect_error<TAction, TTask>(
        &self,
        _state: &mut GrpcHandlerConnectionTaskActorState,
        _action: &GrpcHandlerConnectErrorAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<GrpcHandlerConnectErrorAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        Some(SchedulerTransition::new(once(SchedulerCommand::Forward(
            self.caller_pid,
        ))))
    }
    fn handle_grpc_handler_success_response<TAction, TTask>(
        &self,
        _state: &mut GrpcHandlerConnectionTaskActorState,
        _action: &GrpcHandlerSuccessResponseAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<GrpcHandlerSuccessResponseAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        Some(SchedulerTransition::new(once(SchedulerCommand::Forward(
            self.caller_pid,
        ))))
    }
    fn handle_grpc_handler_error_response<TAction, TTask>(
        &self,
        _state: &mut GrpcHandlerConnectionTaskActorState,
        action: &GrpcHandlerErrorResponseAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction:
            Action + From<GrpcHandlerErrorResponseAction> + From<GrpcHandlerAbortRequestAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let GrpcHandlerErrorResponseAction {
            connection_id,
            operation_id,
            ..
        } = action;
        let abort_action = TAction::from(GrpcHandlerAbortRequestAction {
            connection_id: *connection_id,
            operation_id: *operation_id,
        });
        Some(SchedulerTransition::new([
            SchedulerCommand::Forward(self.caller_pid),
            SchedulerCommand::Send(context.pid(), abort_action),
        ]))
    }
    fn handle_grpc_handler_transport_error<TAction, TTask>(
        &self,
        _state: &mut GrpcHandlerConnectionTaskActorState,
        action: &GrpcHandlerTransportErrorAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction:
            Action + From<GrpcHandlerTransportErrorAction> + From<GrpcHandlerAbortRequestAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let GrpcHandlerTransportErrorAction {
            connection_id,
            operation_id,
            ..
        } = action;
        let abort_action = TAction::from(GrpcHandlerAbortRequestAction {
            connection_id: *connection_id,
            operation_id: *operation_id,
        });
        Some(SchedulerTransition::new([
            SchedulerCommand::Forward(self.caller_pid),
            SchedulerCommand::Send(context.pid(), abort_action),
        ]))
    }
    fn handle_grpc_handler_connection_terminate<TAction, TTask>(
        &self,
        _state: &mut GrpcHandlerConnectionTaskActorState,
        _action: &GrpcHandlerConnectionTerminateAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action,
        TTask: TaskFactory<TAction, TTask>,
    {
        Some(SchedulerTransition::new(once(SchedulerCommand::Kill(
            context.pid(),
        ))))
    }
}

fn create_grpc_connection(
    endpoint: Endpoint,
    delay: Option<Duration>,
) -> impl Future<Output = Result<tonic::client::Grpc<Channel>, tonic::transport::Error>> {
    let connection_delay = match delay {
        None => future::ready(()).left_future(),
        Some(duration) => tokio::time::sleep(duration).right_future(),
    };
    connection_delay.then(|_| async move {
        endpoint
            .connect()
            .map(|result| result.map(tonic::client::Grpc::new))
            .await
    })
}

fn execute_grpc_request(
    client: tonic::client::Grpc<Channel>,
    path: PathAndQuery,
    message: Bytes,
    metadata: MetadataMap,
    streaming: bool,
) -> impl Stream<Item = Result<Bytes, Status>> {
    let request = {
        let mut request = Request::new(message);
        let request_metadata = request.metadata_mut();
        *request_metadata = metadata;
        request
    };
    match streaming {
        true => {
            execute_streaming_grpc_request(client, path, request)
                .map(|result| match result {
                    Err(err) => stream::iter(once(Err(err))).left_stream(),
                    Ok(response) => {
                        let results = response.into_inner();
                        // Filter out error payloads resulting from graceful terminations
                        results.filter(|result| {
                            future::ready(match result.as_ref().map_err(|err| err.code()) {
                                Err(tonic::Code::Cancelled) => false,
                                _ => true,
                            })
                        })
                    }
                    .right_stream(),
                })
                .into_stream()
                .flatten()
                .left_stream()
        }
        false => execute_unary_grpc_request(client, path, request)
            .map(|result| result.map(|response| response.into_inner()))
            .into_stream()
            .right_stream(),
    }
}

fn execute_streaming_grpc_request(
    client: tonic::client::Grpc<Channel>,
    path: PathAndQuery,
    request: impl tonic::IntoRequest<Bytes>,
) -> impl Future<Output = Result<tonic::Response<tonic::codec::Streaming<Bytes>>, Status>> {
    let mut client = client;
    async move {
        // See https://github.com/tower-rs/tower/issues/547
        // Cloned tonic clients must await the ready status before invoking requests to avoid panicking
        let _ = client
            .ready()
            .await
            .map_err(|err| Status::unavailable(err.to_string()))?;
        client
            .server_streaming(request.into_request(), path, BytesCodec)
            .await
    }
}

fn execute_unary_grpc_request(
    client: tonic::client::Grpc<Channel>,
    path: PathAndQuery,
    request: impl tonic::IntoRequest<Bytes>,
) -> impl Future<Output = Result<tonic::Response<Bytes>, Status>> {
    let mut client = client;
    async move {
        // See https://github.com/tower-rs/tower/issues/547
        // Cloned tonic clients must await the ready status before invoking requests to avoid panicking
        let _ = client
            .ready()
            .await
            .map_err(|err| Status::unavailable(err.to_string()))?;
        client.unary(request.into_request(), path, BytesCodec).await
    }
}

fn parse_grpc_operation_error<TAction>(
    status: Status,
    service_name: &GrpcServiceName,
    method_name: &GrpcMethodName,
    method_path: &String,
    connection_id: Uuid,
    operation_id: &GrpcOperationId,
    uri: &Uri,
    input: &JsonValue,
    metadata: &GrpcMetadata,
) -> TAction
where
    TAction: From<GrpcHandlerErrorResponseAction> + From<GrpcHandlerTransportErrorAction>,
{
    let is_transport_error = get_transport_error(&status).is_some();
    if is_transport_error {
        TAction::from(GrpcHandlerTransportErrorAction {
            service_name: service_name.clone().into_string(),
            method_name: method_name.clone().into_string(),
            method_path: method_path.clone(),
            connection_id,
            operation_id: operation_id.as_uuid(),
            url: uri.to_string(),
            input: input.clone(),
            metadata: metadata.clone(),
            status: status.into(),
        })
    } else {
        TAction::from(GrpcHandlerErrorResponseAction {
            service_name: service_name.clone().into_string(),
            method_name: method_name.clone().into_string(),
            method_path: method_path.clone(),
            connection_id,
            operation_id: operation_id.as_uuid(),
            url: uri.to_string(),
            input: input.clone(),
            metadata: metadata.clone(),
            status: status.into(),
        })
    }
}
