// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use futures::{future, FutureExt, Stream};
use hyper::Body;
use reflex::core::Uuid;
use reflex_dispatcher::{
    Action, ActorInitContext, BoxedActionStream, HandlerContext, MessageData, NoopDisposeCallback,
    ProcessId, SchedulerCommand, SchedulerMode, SchedulerTransition, TaskFactory, TaskInbox,
};
use reflex_macros::{blanket_trait, dispatcher, Named};

use crate::{
    action::fetch::{FetchHandlerConnectionErrorAction, FetchHandlerFetchCompleteAction},
    utils::fetch::{fetch, parse_fetch_request, FetchRequest},
};

blanket_trait!(
    pub trait FetchHandlerTask<TConnect>: From<FetchHandlerTaskFactory<TConnect>>
    where
        TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    {
    }
);

// TODO: Implement Serialize/Deserialize traits for FetchHandlerTaskFactory
#[derive(Named, Clone)]
pub struct FetchHandlerTaskFactory<TConnect>
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    pub operation_id: Uuid,
    pub client: hyper::Client<TConnect, Body>,
    pub request: FetchRequest,
    pub caller_pid: ProcessId,
}

impl<TConnect, TAction, TTask> TaskFactory<TAction, TTask> for FetchHandlerTaskFactory<TConnect>
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TAction: Action + FetchHandlerTaskAction + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    type Actor = FetchHandlerTaskActor<TConnect>;
    fn create(self) -> Self::Actor {
        let Self {
            operation_id,
            client,
            request,
            caller_pid,
        } = self;
        FetchHandlerTaskActor {
            operation_id,
            client,
            request,
            caller_pid,
        }
    }
}

#[derive(Named, Clone)]
pub struct FetchHandlerTaskActor<TConnect>
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    operation_id: Uuid,
    client: hyper::Client<TConnect, Body>,
    request: FetchRequest,
    caller_pid: ProcessId,
}

#[derive(PartialEq, Eq, Clone, Copy, Default)]
pub struct FetchHandlerTaskActorState;

dispatcher!({
    pub enum FetchHandlerTaskAction {
        Inbox(FetchHandlerFetchCompleteAction),
        Inbox(FetchHandlerConnectionErrorAction),

        Outbox(FetchHandlerFetchCompleteAction),
        Outbox(FetchHandlerConnectionErrorAction),
    }

    impl<TConnect, TAction, TTask> Dispatcher<TAction, TTask> for FetchHandlerTaskActor<TConnect>
    where
        TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
        TAction: Action + 'static,
        TTask: TaskFactory<TAction, TTask>,
    {
        type State = FetchHandlerTaskActorState;
        type Events<TInbox: TaskInbox<TAction>> = BoxedActionStream<TInbox::Message>;
        type Dispose = NoopDisposeCallback;

        fn init<TInbox: TaskInbox<TAction>>(
            &self,
            inbox: TInbox,
            context: &impl ActorInitContext,
        ) -> (Self::State, Self::Events<TInbox>, Self::Dispose) {
            (
                Default::default(),
                Box::pin(self.events(inbox, context)),
                NoopDisposeCallback,
            )
        }

        fn accept(&self, _action: &FetchHandlerFetchCompleteAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &FetchHandlerFetchCompleteAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &FetchHandlerFetchCompleteAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_fetch_handler_fetch_complete_action(state, action, metadata, context)
        }

        fn accept(&self, _action: &FetchHandlerConnectionErrorAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &FetchHandlerConnectionErrorAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &FetchHandlerConnectionErrorAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_fetch_handler_connection_error_action(state, action, metadata, context)
        }
    }
});

impl<TConnect> FetchHandlerTaskActor<TConnect>
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    fn events<TInbox, TAction>(
        &self,
        _inbox: TInbox,
        _context: &impl ActorInitContext,
    ) -> impl Stream<Item = TInbox::Message>
    where
        TInbox: TaskInbox<TAction>,
        TAction: Action
            + From<FetchHandlerFetchCompleteAction>
            + From<FetchHandlerConnectionErrorAction>,
    {
        match parse_fetch_request(&self.request) {
            Err(err) => future::ready(Err(err)).left_future(),
            Ok(request) => fetch(self.client.clone(), request).right_future(),
        }
        .map({
            let operation_id = self.operation_id;
            let url = self.request.url.clone();
            move |result| match result {
                Ok((status_code, body)) => TAction::from(FetchHandlerFetchCompleteAction {
                    operation_id,
                    url,
                    status_code,
                    body,
                }),
                Err(err) => TAction::from(FetchHandlerConnectionErrorAction {
                    operation_id,
                    url,
                    message: create_fetch_error_message(err),
                }),
            }
        })
        .map(|action| TInbox::Message::from(action))
        .into_stream()
    }
    fn handle_fetch_handler_fetch_complete_action<TAction, TTask>(
        &self,
        _state: &mut FetchHandlerTaskActorState,
        _action: &FetchHandlerFetchCompleteAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<FetchHandlerFetchCompleteAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        Some(SchedulerTransition::new([
            SchedulerCommand::Kill(context.pid()),
            SchedulerCommand::Forward(self.caller_pid),
        ]))
    }
    fn handle_fetch_handler_connection_error_action<TAction, TTask>(
        &self,
        _state: &mut FetchHandlerTaskActorState,
        _action: &FetchHandlerConnectionErrorAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<FetchHandlerConnectionErrorAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        Some(SchedulerTransition::new([
            SchedulerCommand::Kill(context.pid()),
            SchedulerCommand::Forward(self.caller_pid),
        ]))
    }
}

pub(crate) fn create_fetch_error_message(err: impl std::fmt::Display) -> String {
    format!("Fetch error: {}", err)
}
