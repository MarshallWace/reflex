// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use futures::{Future, Stream};
use pin_project::pin_project;
use reflex::core::{Expression, ExpressionFactory, HeapAllocator};
use reflex_dispatcher::{
    Action, Actor, ActorInitContext, Handler, HandlerContext, MessageData, Named, SchedulerMode,
    SchedulerTransition, TaskFactory, TaskInbox, Worker,
};

pub mod bytecode_interpreter;
pub mod evaluate_handler;
pub mod query_inspector;
pub mod query_manager;

use self::evaluate_handler::*;
use self::query_manager::*;

#[derive(Default, Clone, Copy, Debug)]
pub struct RuntimeMetricNames {
    pub query_manager: QueryManagerMetricNames,
    pub evaluate_handler: EvaluateHandlerMetricNames,
}

pub trait RuntimeAction<T: Expression>: QueryManagerAction<T> + EvaluateHandlerAction<T> {}
impl<_Self, T: Expression> RuntimeAction<T> for _Self where
    Self: QueryManagerAction<T> + EvaluateHandlerAction<T>
{
}

#[derive(Clone)]
pub enum RuntimeActor<T, TFactory, TAllocator>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
{
    QueryManager(QueryManager<T, TFactory, TAllocator>),
    EvaluateHandler(EvaluateHandler<T, TFactory, TAllocator>),
}
impl<T, TFactory, TAllocator> Named for RuntimeActor<T, TFactory, TAllocator>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
{
    fn name(&self) -> &'static str {
        match self {
            RuntimeActor::QueryManager(inner) => inner.name(),
            RuntimeActor::EvaluateHandler(inner) => inner.name(),
        }
    }
}

impl<T, TFactory, TAllocator, TAction, TTask> Actor<TAction, TTask>
    for RuntimeActor<T, TFactory, TAllocator>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
    TAction: Action + RuntimeAction<T>,
    TTask: TaskFactory<TAction, TTask>,
{
    type Events<TInbox: TaskInbox<TAction>> =
        RuntimeActorEvents<T, TFactory, TAllocator, TInbox, TAction, TTask>;
    type Dispose = RuntimeActorDispose<T, TFactory, TAllocator, TAction, TTask>;
    fn init<TInbox: TaskInbox<TAction>>(
        &self,
        inbox: TInbox,
        context: &impl ActorInitContext,
    ) -> (Self::State, Self::Events<TInbox>, Self::Dispose) {
        match self {
            Self::QueryManager(actor) => {
                let (state, events, dispose) = <QueryManager<T, TFactory, TAllocator> as Actor<
                    TAction,
                    TTask,
                >>::init(actor, inbox, context);
                (
                    RuntimeActorState::QueryManager(state),
                    RuntimeActorEvents::QueryManager(events),
                    RuntimeActorDispose::QueryManager(dispose),
                )
            }
            Self::EvaluateHandler(actor) => {
                let (state, events, dispose) =
                    <EvaluateHandler<T, TFactory, TAllocator> as Actor<TAction, TTask>>::init(
                        actor, inbox, context,
                    );
                (
                    RuntimeActorState::EvaluateHandler(state),
                    RuntimeActorEvents::EvaluateHandler(events),
                    RuntimeActorDispose::EvaluateHandler(dispose),
                )
            }
        }
    }
}

pub enum RuntimeActorState<T: Expression> {
    QueryManager(QueryManagerState<T>),
    EvaluateHandler(EvaluateHandlerState<T>),
}

#[pin_project(project = RuntimeActorEventsVariant)]
pub enum RuntimeActorEvents<T, TFactory, TAllocator, TInbox, TAction, TTask>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
    TInbox: TaskInbox<TAction>,
    TAction: Action + RuntimeAction<T>,
    TTask: TaskFactory<TAction, TTask>,
{
    QueryManager(
        #[pin] <QueryManager<T, TFactory, TAllocator> as Actor<TAction, TTask>>::Events<TInbox>,
    ),
    EvaluateHandler(
        #[pin] <EvaluateHandler<T, TFactory, TAllocator> as Actor<TAction, TTask>>::Events<TInbox>,
    ),
}
impl<T, TFactory, TAllocator, TInbox, TAction, TTask> Stream
    for RuntimeActorEvents<T, TFactory, TAllocator, TInbox, TAction, TTask>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
    TInbox: TaskInbox<TAction>,
    TAction: Action + RuntimeAction<T>,
    TTask: TaskFactory<TAction, TTask>,
{
    type Item = TInbox::Message;
    fn poll_next(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<Self::Item>> {
        match self.project() {
            RuntimeActorEventsVariant::QueryManager(inner) => inner.poll_next(cx),
            RuntimeActorEventsVariant::EvaluateHandler(inner) => inner.poll_next(cx),
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            Self::QueryManager(inner) => inner.size_hint(),
            Self::EvaluateHandler(inner) => inner.size_hint(),
        }
    }
}

#[pin_project(project = RuntimeActorDisposeVariant)]
pub enum RuntimeActorDispose<T, TFactory, TAllocator, TAction, TTask>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
    TAction: Action + RuntimeAction<T>,
    TTask: TaskFactory<TAction, TTask>,
{
    QueryManager(#[pin] <QueryManager<T, TFactory, TAllocator> as Actor<TAction, TTask>>::Dispose),
    EvaluateHandler(
        #[pin] <EvaluateHandler<T, TFactory, TAllocator> as Actor<TAction, TTask>>::Dispose,
    ),
}
impl<T, TFactory, TAllocator, TAction, TTask> Future
    for RuntimeActorDispose<T, TFactory, TAllocator, TAction, TTask>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
    TAction: Action + RuntimeAction<T>,
    TTask: TaskFactory<TAction, TTask>,
{
    type Output = ();
    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        match self.project() {
            RuntimeActorDisposeVariant::QueryManager(inner) => inner.poll(cx),
            RuntimeActorDisposeVariant::EvaluateHandler(inner) => inner.poll(cx),
        }
    }
}

impl<T, TFactory, TAllocator, TAction, TTask> Worker<TAction, SchedulerTransition<TAction, TTask>>
    for RuntimeActor<T, TFactory, TAllocator>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
    TAction: Action + RuntimeAction<T>,
    TTask: TaskFactory<TAction, TTask>,
{
    fn accept(&self, message: &TAction) -> bool {
        match self {
            Self::QueryManager(inner) => <QueryManager<T, TFactory, TAllocator> as Worker<
                TAction,
                SchedulerTransition<TAction, TTask>,
            >>::accept(inner, message),
            Self::EvaluateHandler(inner) => <EvaluateHandler<T, TFactory, TAllocator> as Worker<
                TAction,
                SchedulerTransition<TAction, TTask>,
            >>::accept(inner, message),
        }
    }
    fn schedule(&self, message: &TAction, state: &Self::State) -> Option<SchedulerMode> {
        match (self, state) {
            (Self::QueryManager(actor), RuntimeActorState::QueryManager(state)) => {
                <QueryManager<T, TFactory, TAllocator> as Worker<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::schedule(actor, message, state)
            }
            (Self::EvaluateHandler(actor), RuntimeActorState::EvaluateHandler(state)) => {
                <EvaluateHandler<T, TFactory, TAllocator> as Worker<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::schedule(actor, message, state)
            }
            _ => unreachable!(),
        }
    }
}

impl<T, TFactory, TAllocator, TAction, TTask> Handler<TAction, SchedulerTransition<TAction, TTask>>
    for RuntimeActor<T, TFactory, TAllocator>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
    TAction: Action + RuntimeAction<T>,
    TTask: TaskFactory<TAction, TTask>,
{
    type State = RuntimeActorState<T>;
    fn handle(
        &self,
        state: &mut Self::State,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>> {
        match (self, state) {
            (Self::QueryManager(inner), RuntimeActorState::QueryManager(state)) => {
                <QueryManager<T, TFactory, TAllocator> as Handler<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::handle(inner, state, action, metadata, context)
            }
            (Self::EvaluateHandler(inner), RuntimeActorState::EvaluateHandler(state)) => {
                <EvaluateHandler<T, TFactory, TAllocator> as Handler<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::handle(inner, state, action, metadata, context)
            }
            _ => unreachable!(),
        }
    }
}
