// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::pin::Pin;

use futures::{Future, Stream};
use pin_project::pin_project;
use reflex::core::{
    Applicable, Expression, ExpressionFactory, HeapAllocator, Reducible, Rewritable,
};
use reflex_dispatcher::{
    Action, Actor, ActorInitContext, Handler, HandlerContext, MessageData, Named, SchedulerMode,
    SchedulerTransition, TaskFactory, TaskInbox, Worker,
};
use reflex_interpreter::compiler::Compile;

pub mod bytecode_worker;

use crate::{
    task::bytecode_worker::{
        BytecodeWorker, BytecodeWorkerAction, BytecodeWorkerTask, BytecodeWorkerTaskFactory,
    },
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator,
};

pub trait RuntimeTaskAction<T: Expression>: BytecodeWorkerAction<T> {}
impl<_Self, T: Expression> RuntimeTaskAction<T> for _Self where Self: BytecodeWorkerAction<T> {}

pub trait RuntimeTask<T, TFactory, TAllocator>:
    BytecodeWorkerTask<T, TFactory, TAllocator>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
{
}
impl<_Self, T, TFactory, TAllocator> RuntimeTask<T, TFactory, TAllocator> for _Self
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
    Self: BytecodeWorkerTask<T, TFactory, TAllocator>,
{
}

// TODO: Implement Serialize/Deserialize traits for RuntimeTaskFactory
#[derive(Clone)]
pub enum RuntimeTaskFactory<T, TFactory, TAllocator>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
{
    BytecodeWorker(BytecodeWorkerTaskFactory<T, TFactory, TAllocator>),
}
impl<T, TFactory, TAllocator> Named for RuntimeTaskFactory<T, TFactory, TAllocator>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
{
    fn name(&self) -> &'static str {
        match self {
            Self::BytecodeWorker(inner) => inner.name(),
        }
    }
}

impl<T, TFactory, TAllocator, TAction, TTask> TaskFactory<TAction, TTask>
    for RuntimeTaskFactory<T, TFactory, TAllocator>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TAction: Action + RuntimeTaskAction<T> + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    type Actor = RuntimeTaskActor<T, TFactory, TAllocator>;
    fn create(self) -> Self::Actor {
        match self {
            Self::BytecodeWorker(inner) => RuntimeTaskActor::BytecodeWorker(
                <BytecodeWorkerTaskFactory<T, TFactory, TAllocator> as TaskFactory<
                    TAction,
                    TTask,
                >>::create(inner),
            ),
        }
    }
}

#[derive(Clone)]
pub enum RuntimeTaskActor<T, TFactory, TAllocator>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
{
    BytecodeWorker(BytecodeWorker<T, TFactory, TAllocator>),
}
impl<T, TFactory, TAllocator> Named for RuntimeTaskActor<T, TFactory, TAllocator>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
{
    fn name(&self) -> &'static str {
        match self {
            Self::BytecodeWorker(inner) => inner.name(),
        }
    }
}

impl<T, TFactory, TAllocator, TAction, TTask> Actor<TAction, TTask>
    for RuntimeTaskActor<T, TFactory, TAllocator>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TAction: Action + RuntimeTaskAction<T> + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    type Events<TInbox: TaskInbox<TAction>> =
        RuntimeTaskEvents<T, TFactory, TAllocator, TInbox, TAction, TTask>;
    type Dispose = RuntimeTaskDispose<T, TFactory, TAllocator, TAction, TTask>;
    fn init<TInbox: TaskInbox<TAction>>(
        &self,
        inbox: TInbox,
        context: &impl ActorInitContext,
    ) -> (Self::State, Self::Events<TInbox>, Self::Dispose) {
        match self {
            Self::BytecodeWorker(actor) => {
                let (state, events, dispose) = <BytecodeWorker<T, TFactory, TAllocator> as Actor<
                    TAction,
                    TTask,
                >>::init(actor, inbox, context);
                (
                    RuntimeTaskActorState::BytecodeWorker(state),
                    RuntimeTaskEvents::BytecodeWorker(events),
                    RuntimeTaskDispose::BytecodeWorker(dispose),
                )
            }
        }
    }
}

impl<T, TFactory, TAllocator, TAction, TTask> Worker<TAction, SchedulerTransition<TAction, TTask>>
    for RuntimeTaskActor<T, TFactory, TAllocator>
where
    T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
    TAction: Action + RuntimeTaskAction<T> + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    fn accept(&self, message: &TAction) -> bool {
        match self {
            Self::BytecodeWorker(inner) => <BytecodeWorker<T, TFactory, TAllocator> as Worker<
                TAction,
                SchedulerTransition<TAction, TTask>,
            >>::accept(inner, message),
        }
    }
    fn schedule(&self, message: &TAction, state: &Self::State) -> Option<SchedulerMode> {
        match (self, state) {
            (Self::BytecodeWorker(actor), RuntimeTaskActorState::BytecodeWorker(state)) => {
                <BytecodeWorker<T, TFactory, TAllocator> as Worker<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::schedule(actor, message, state)
            }
        }
    }
}

impl<T, TFactory, TAllocator, TAction, TTask> Handler<TAction, SchedulerTransition<TAction, TTask>>
    for RuntimeTaskActor<T, TFactory, TAllocator>
where
    T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
    TAction: Action + RuntimeTaskAction<T> + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    type State = RuntimeTaskActorState<T, TFactory, TAllocator, TAction, TTask>;
    fn handle(
        &self,
        state: &mut Self::State,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>> {
        match (self, state) {
            (Self::BytecodeWorker(actor), RuntimeTaskActorState::BytecodeWorker(state)) => {
                <BytecodeWorker<T, TFactory, TAllocator> as Handler<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::handle(actor, state, action, metadata, context)
            }
        }
    }
}

pub enum RuntimeTaskActorState<T, TFactory, TAllocator, TAction, TTask>
where
    T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
    TAction: Action + RuntimeTaskAction<T> + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    BytecodeWorker(
        <BytecodeWorker<T, TFactory, TAllocator> as Handler<
            TAction,
            SchedulerTransition<TAction, TTask>,
        >>::State,
    ),
}

#[pin_project(project = RuntimeTaskEventsVariant)]
pub enum RuntimeTaskEvents<T, TFactory, TAllocator, TInbox, TAction, TTask>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TInbox: TaskInbox<TAction>,
    TAction: Action + RuntimeTaskAction<T> + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    BytecodeWorker(
        #[pin] <BytecodeWorker<T, TFactory, TAllocator> as Actor<TAction, TTask>>::Events<TInbox>,
    ),
}
impl<T, TFactory, TAllocator, TInbox, TAction, TTask> Stream
    for RuntimeTaskEvents<T, TFactory, TAllocator, TInbox, TAction, TTask>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TInbox: TaskInbox<TAction>,
    TAction: Action + RuntimeTaskAction<T> + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    type Item = TInbox::Message;
    fn poll_next(
        self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<Self::Item>> {
        match self.project() {
            RuntimeTaskEventsVariant::BytecodeWorker(inner) => inner.poll_next(cx),
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            Self::BytecodeWorker(inner) => inner.size_hint(),
        }
    }
}

#[pin_project(project = RuntimeTaskDisposeVariant)]
pub enum RuntimeTaskDispose<T, TFactory, TAllocator, TAction, TTask>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TAction: Action + RuntimeTaskAction<T> + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    BytecodeWorker(
        #[pin] <BytecodeWorker<T, TFactory, TAllocator> as Actor<TAction, TTask>>::Dispose,
    ),
}
impl<T, TFactory, TAllocator, TAction, TTask> Future
    for RuntimeTaskDispose<T, TFactory, TAllocator, TAction, TTask>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TAction: Action + RuntimeTaskAction<T> + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    type Output = ();
    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        match self.project() {
            RuntimeTaskDisposeVariant::BytecodeWorker(inner) => inner.poll(cx),
        }
    }
}

impl<T, TFactory, TAllocator> From<BytecodeWorkerTaskFactory<T, TFactory, TAllocator>>
    for RuntimeTaskFactory<T, TFactory, TAllocator>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
{
    fn from(value: BytecodeWorkerTaskFactory<T, TFactory, TAllocator>) -> Self {
        Self::BytecodeWorker(value)
    }
}
