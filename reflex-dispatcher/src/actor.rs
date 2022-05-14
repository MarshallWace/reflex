// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::marker::PhantomData;

use futures::StreamExt;

use crate::{
    Action, Actor, ActorTransition, HandlerContext, InboundAction, MessageData, OperationStream,
    OutboundAction, StateOperation, StateTransition, Worker, WorkerContext, WorkerFactory,
    WorkerMessageQueue, WorkerTransition,
};

pub struct NoopActor;
impl<T: Action> Actor<T> for NoopActor {
    type State = ();
    fn init(&self) -> Self::State {
        ()
    }
    fn handle(
        &self,
        state: Self::State,
        _action: &T,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> ActorTransition<Self::State, T> {
        ActorTransition::new(state, Default::default())
    }
}

pub enum EitherActor<T1, T2> {
    Left(T1),
    Right(T2),
}
pub enum EitherActorState<T1, T2> {
    Left(T1),
    Right(T2),
}
impl<T1, T2, TAction> Actor<TAction> for EitherActor<T1, T2>
where
    T1: Actor<TAction>,
    T2: Actor<TAction>,
    TAction: Action,
{
    type State = EitherActorState<T1::State, T2::State>;
    fn init(&self) -> Self::State {
        match self {
            Self::Left(actor) => Self::State::Left(actor.init()),
            Self::Right(actor) => Self::State::Right(actor.init()),
        }
    }
    fn handle(
        &self,
        state: Self::State,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> ActorTransition<Self::State, TAction> {
        match (self, state) {
            (Self::Left(actor), Self::State::Left(state)) => {
                let (state, actions) = actor.handle(state, action, metadata, context).into_parts();
                ActorTransition::new(Self::State::Left(state), actions)
            }
            (Self::Right(actor), Self::State::Right(state)) => {
                let (state, actions) = actor.handle(state, action, metadata, context).into_parts();
                ActorTransition::new(Self::State::Right(state), actions)
            }
            (_, state) => ActorTransition::new(state, Default::default()),
        }
    }
}

pub struct FilteredActor<TOuter, TInner, TActor>
where
    TOuter: Action + InboundAction<TInner> + OutboundAction<TInner>,
    TInner: Action,
    TActor: Actor<TInner>,
{
    actor: TActor,
    _outer: PhantomData<TOuter>,
    _inner: PhantomData<TInner>,
}
impl<TOuter, TInner, TActor> FilteredActor<TOuter, TInner, TActor>
where
    TOuter: Action + InboundAction<TInner> + OutboundAction<TInner>,
    TInner: Action,
    TActor: Actor<TInner>,
{
    pub fn new(actor: TActor) -> Self {
        Self {
            actor,
            _outer: Default::default(),
            _inner: Default::default(),
        }
    }
}
impl<TOuter, TInner, TActor> Actor<TOuter> for FilteredActor<TOuter, TInner, TActor>
where
    TOuter: Action + InboundAction<TInner> + OutboundAction<TInner> + Send + 'static,
    TInner: Action + Send + 'static,
    TActor: Actor<TInner>,
{
    type State = TActor::State;
    fn init(&self) -> Self::State {
        self.actor.init()
    }
    fn handle(
        &self,
        state: Self::State,
        action: &TOuter,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> ActorTransition<Self::State, TOuter> {
        if let Some(action) = action.match_type() {
            let (state, actions) = self
                .actor
                .handle(state, action, metadata, context)
                .into_parts();
            ActorTransition::new(
                state,
                StateTransition::new(actions.into_iter().map(transform_state_operation)),
            )
        } else {
            ActorTransition::new(state, Default::default())
        }
    }
}

struct FilteredWorker<TOuter, TInner>
where
    TOuter: Action + InboundAction<TInner> + OutboundAction<TInner>,
    TInner: Action,
{
    worker: Box<dyn Worker<TInner> + Send + 'static>,
    _outer: PhantomData<TOuter>,
}
impl<TOuter, TInner> FilteredWorker<TOuter, TInner>
where
    TOuter: Action + InboundAction<TInner> + OutboundAction<TInner>,
    TInner: Action,
{
    fn new(worker: Box<dyn Worker<TInner> + Send + 'static>) -> Self {
        Self {
            worker,
            _outer: Default::default(),
        }
    }
}
impl<TOuter, TInner> Worker<TOuter> for FilteredWorker<TOuter, TInner>
where
    TOuter: Action + InboundAction<TInner> + OutboundAction<TInner> + Send + 'static,
    TInner: Action + Send + 'static,
{
    fn handle(
        &mut self,
        actions: WorkerMessageQueue<TOuter>,
        context: &mut WorkerContext,
    ) -> WorkerTransition<TOuter> {
        let actions = actions
            .into_iter()
            .filter_map(|(action, metadata)| action.into_type().map(|action| (action, metadata)))
            .collect::<WorkerMessageQueue<TInner>>();
        if actions.is_empty() {
            WorkerTransition::new(None)
        } else {
            transform_worker_transition(self.worker.handle(actions, context))
        }
    }
}

fn transform_worker_transition<TOuter, TInner>(
    transition: WorkerTransition<TInner>,
) -> WorkerTransition<TOuter>
where
    TOuter: Action + InboundAction<TInner> + OutboundAction<TInner> + Send + 'static,
    TInner: Action + Send + 'static,
{
    WorkerTransition::new(
        transition
            .into_iter()
            .map(|(offset, operation)| (offset, transform_state_operation(operation))),
    )
}

fn transform_state_operation<TOuter, TInner>(
    operation: StateOperation<TInner>,
) -> StateOperation<TOuter>
where
    TOuter: Action + InboundAction<TInner> + OutboundAction<TInner> + Send + 'static,
    TInner: Action + Send + 'static,
{
    match operation {
        StateOperation::Send(target, action) => {
            StateOperation::<TOuter>::Send(target, action.into())
        }
        StateOperation::Task(pid, task) => {
            let (stream, dispose) = task.into_parts();
            StateOperation::<TOuter>::Task(
                pid,
                OperationStream::<TOuter>::from_parts(
                    Box::new(
                        stream.map(|operation| {
                            transform_state_operation::<TOuter, TInner>(operation)
                        }),
                    ),
                    dispose,
                ),
            )
        }
        StateOperation::Spawn(pid, factory) => StateOperation::<TOuter>::Spawn(
            pid,
            WorkerFactory::new(|| FilteredWorker::<TOuter, TInner>::new(factory.create())),
        ),
        StateOperation::Kill(pid) => StateOperation::<TOuter>::Kill(pid),
    }
}

pub struct ChainedActor<TAction: Action, T1: Actor<TAction>, T2: Actor<TAction>> {
    left: T1,
    right: T2,
    _action: PhantomData<TAction>,
}
impl<T: Action, T1: Actor<T>, T2: Actor<T>> ChainedActor<T, T1, T2> {
    pub fn new(left: T1, right: T2) -> Self {
        Self {
            left,
            right,
            _action: Default::default(),
        }
    }
}
pub struct ChainedActorState<TAction: Action, T1: Actor<TAction>, T2: Actor<TAction>> {
    left: T1::State,
    right: T2::State,
    _action: PhantomData<TAction>,
}
impl<TAction: Action, T1: Actor<TAction>, T2: Actor<TAction>> Actor<TAction>
    for ChainedActor<TAction, T1, T2>
{
    type State = ChainedActorState<TAction, T1, T2>;
    fn init(&self) -> Self::State {
        Self::State {
            left: self.left.init(),
            right: self.right.init(),
            _action: Default::default(),
        }
    }
    fn handle(
        &self,
        state: Self::State,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> ActorTransition<Self::State, TAction> {
        let Self::State {
            left: left_state,
            right: right_state,
            ..
        } = state;
        let (left_state, left_actions) = self
            .left
            .handle(left_state, action, metadata, context)
            .into_parts();
        let (right_state, right_actions) = self
            .right
            .handle(right_state, action, metadata, context)
            .into_parts();
        ActorTransition::new(
            Self::State {
                left: left_state,
                right: right_state,
                _action: Default::default(),
            },
            StateTransition::new(left_actions.into_iter().chain(right_actions)),
        )
    }
}
