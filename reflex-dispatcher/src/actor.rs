// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::marker::PhantomData;

use futures::StreamExt;

use crate::{
    Action, Actor, ActorTransition, HandlerContext, InboundAction, MessageData, MiddlewareContext,
    OperationStream, OutboundAction, PostMiddleware, PostMiddlewareTransition, PreMiddleware,
    PreMiddlewareTransition, StateOperation, StateTransition, Worker, WorkerContext, WorkerFactory,
    WorkerMessageQueue, WorkerTransition,
};

#[derive(Clone, Copy)]
pub struct NoopActor;
impl<TAction: Action> Actor<TAction> for NoopActor {
    type State = ();
    fn init(&self) -> Self::State {
        ()
    }
    fn handle(
        &self,
        state: Self::State,
        _action: &TAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> ActorTransition<Self::State, TAction> {
        ActorTransition::new(state, Default::default())
    }
}
impl<TAction: Action> PreMiddleware<TAction> for NoopActor {
    type State = ();
    fn init(&self) -> Self::State {
        ()
    }
    fn handle(
        &self,
        state: Self::State,
        action: StateOperation<TAction>,
        _metadata: &MessageData,
        _context: &MiddlewareContext,
    ) -> PreMiddlewareTransition<Self::State, TAction> {
        PreMiddlewareTransition::new(state, action)
    }
}
impl<TAction: Action> PostMiddleware<TAction> for NoopActor {
    type State = ();
    fn init(&self) -> Self::State {
        ()
    }
    fn handle(
        &self,
        state: Self::State,
        _action: StateOperation<TAction>,
        _metadata: &MessageData,
        _context: &MiddlewareContext,
    ) -> PostMiddlewareTransition<Self::State> {
        PostMiddlewareTransition::new(state)
    }
}

impl<T, TAction> Actor<TAction> for Option<T>
where
    T: Actor<TAction>,
    TAction: Action,
{
    type State = Option<T::State>;
    fn init(&self) -> Self::State {
        self.as_ref().map(|inner| inner.init())
    }
    fn handle(
        &self,
        state: Self::State,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> ActorTransition<Self::State, TAction> {
        match (self, state) {
            (Some(inner), Some(state)) => {
                let (state, actions) = inner.handle(state, action, metadata, context).into_parts();
                ActorTransition::new(Some(state), actions)
            }
            (_, state) => ActorTransition::new(state, Default::default()),
        }
    }
}
impl<T, TAction> PreMiddleware<TAction> for Option<T>
where
    T: PreMiddleware<TAction>,
    TAction: Action,
{
    type State = Option<T::State>;
    fn init(&self) -> Self::State {
        self.as_ref().map(|inner| inner.init())
    }
    fn handle(
        &self,
        state: Self::State,
        operation: StateOperation<TAction>,
        metadata: &MessageData,
        context: &MiddlewareContext,
    ) -> PreMiddlewareTransition<Self::State, TAction> {
        match (self, state) {
            (Some(inner), Some(state)) => {
                let (state, operation) = inner
                    .handle(state, operation, metadata, context)
                    .into_parts();
                PreMiddlewareTransition::new(Some(state), operation)
            }
            (_, state) => PreMiddlewareTransition::new(state, operation),
        }
    }
}
impl<T, TAction> PostMiddleware<TAction> for Option<T>
where
    T: PostMiddleware<TAction>,
    TAction: Action,
{
    type State = Option<T::State>;
    fn init(&self) -> Self::State {
        self.as_ref().map(|inner| inner.init())
    }
    fn handle(
        &self,
        state: Self::State,
        operation: StateOperation<TAction>,
        metadata: &MessageData,
        context: &MiddlewareContext,
    ) -> PostMiddlewareTransition<Self::State> {
        match (self, state) {
            (Some(inner), Some(state)) => {
                let state = inner
                    .handle(state, operation, metadata, context)
                    .into_inner();
                PostMiddlewareTransition::new(Some(state))
            }
            (_, state) => PostMiddlewareTransition::new(state),
        }
    }
}

pub enum EitherActor<T1, T2> {
    Left(T1),
    Right(T2),
}
impl<T1, T2> Clone for EitherActor<T1, T2>
where
    T1: Clone,
    T2: Clone,
{
    fn clone(&self) -> Self {
        match self {
            Self::Left(inner) => Self::Left(inner.clone()),
            Self::Right(inner) => Self::Right(inner.clone()),
        }
    }
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
impl<T1, T2, TAction> PreMiddleware<TAction> for EitherActor<T1, T2>
where
    T1: PreMiddleware<TAction>,
    T2: PreMiddleware<TAction>,
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
        operation: StateOperation<TAction>,
        metadata: &MessageData,
        context: &MiddlewareContext,
    ) -> PreMiddlewareTransition<Self::State, TAction> {
        match (self, state) {
            (Self::Left(actor), Self::State::Left(state)) => {
                let (state, operation) = actor
                    .handle(state, operation, metadata, context)
                    .into_parts();
                PreMiddlewareTransition::new(Self::State::Left(state), operation)
            }
            (Self::Right(actor), Self::State::Right(state)) => {
                let (state, operation) = actor
                    .handle(state, operation, metadata, context)
                    .into_parts();
                PreMiddlewareTransition::new(Self::State::Right(state), operation)
            }
            (_, state) => PreMiddlewareTransition::new(state, operation),
        }
    }
}
impl<T1, T2, TAction> PostMiddleware<TAction> for EitherActor<T1, T2>
where
    T1: PostMiddleware<TAction>,
    T2: PostMiddleware<TAction>,
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
        operation: StateOperation<TAction>,
        metadata: &MessageData,
        context: &MiddlewareContext,
    ) -> PostMiddlewareTransition<Self::State> {
        match (self, state) {
            (Self::Left(actor), Self::State::Left(state)) => {
                let state = actor
                    .handle(state, operation, metadata, context)
                    .into_inner();
                PostMiddlewareTransition::new(Self::State::Left(state))
            }
            (Self::Right(actor), Self::State::Right(state)) => {
                let state = actor
                    .handle(state, operation, metadata, context)
                    .into_inner();
                PostMiddlewareTransition::new(Self::State::Right(state))
            }
            (_, state) => PostMiddlewareTransition::new(state),
        }
    }
}

pub struct ChainedActor<T1, T2> {
    left: T1,
    right: T2,
}
impl<T1: Clone, T2: Clone> Clone for ChainedActor<T1, T2> {
    fn clone(&self) -> Self {
        Self {
            left: self.left.clone(),
            right: self.right.clone(),
        }
    }
}
impl<T1, T2> ChainedActor<T1, T2> {
    pub fn new(left: T1, right: T2) -> Self {
        Self { left, right }
    }
}
pub struct ChainedActorState<TAction, T1, T2>
where
    T1: Actor<TAction>,
    T2: Actor<TAction>,
    TAction: Action,
{
    left: T1::State,
    right: T2::State,
    _action: PhantomData<TAction>,
}
impl<T1, T2, TAction> Actor<TAction> for ChainedActor<T1, T2>
where
    T1: Actor<TAction>,
    T2: Actor<TAction>,
    TAction: Action,
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
pub struct ChainedMiddlewareState<TAction, T1, T2>
where
    T1: PreMiddleware<TAction>,
    T2: PreMiddleware<TAction>,
    TAction: Action,
{
    left: T1::State,
    right: T2::State,
    _action: PhantomData<TAction>,
}
impl<T1, T2, TAction> PreMiddleware<TAction> for ChainedActor<T1, T2>
where
    T1: PreMiddleware<TAction>,
    T2: PreMiddleware<TAction>,
    TAction: Action,
{
    type State = ChainedMiddlewareState<TAction, T1, T2>;
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
        operation: StateOperation<TAction>,
        metadata: &MessageData,
        context: &MiddlewareContext,
    ) -> PreMiddlewareTransition<Self::State, TAction> {
        let Self::State {
            left: left_state,
            right: right_state,
            ..
        } = state;
        let (left_state, operation) = self
            .left
            .handle(left_state, operation, metadata, context)
            .into_parts();
        let (right_state, operation) = self
            .right
            .handle(right_state, operation, metadata, context)
            .into_parts();
        PreMiddlewareTransition::new(
            Self::State {
                left: left_state,
                right: right_state,
                _action: Default::default(),
            },
            operation,
        )
    }
}

pub struct FilteredActor<TOuter, TInner, TActor>
where
    TOuter: InboundAction<TInner> + OutboundAction<TInner>,
    TActor: Actor<TInner>,
    TOuter: Action,
    TInner: Action,
{
    actor: TActor,
    _outer: PhantomData<TOuter>,
    _inner: PhantomData<TInner>,
}
impl<TOuter, TInner, TActor> Clone for FilteredActor<TOuter, TInner, TActor>
where
    TActor: Actor<TInner> + Clone,
    TOuter: Action + InboundAction<TInner> + OutboundAction<TInner>,
    TInner: Action,
{
    fn clone(&self) -> Self {
        Self {
            actor: self.actor.clone(),
            _outer: Default::default(),
            _inner: Default::default(),
        }
    }
}
impl<TOuter, TInner, TActor> FilteredActor<TOuter, TInner, TActor>
where
    TActor: Actor<TInner>,
    TOuter: Action + InboundAction<TInner> + OutboundAction<TInner>,
    TInner: Action,
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
    TActor: Actor<TInner>,
    TOuter: Action + InboundAction<TInner> + OutboundAction<TInner> + Send + 'static,
    TInner: Action + Send + 'static,
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

struct FilteredWorkerFactory<TOuter, TInner, TFactory, TWorker>
where
    TFactory: WorkerFactory<TInner, Worker = TWorker>,
    TWorker: Worker<TInner>,
    TOuter: Action + InboundAction<TInner> + OutboundAction<TInner>,
    TInner: Action,
{
    factory: TFactory,
    _outer: PhantomData<TOuter>,
    _inner: PhantomData<TInner>,
    _worker: PhantomData<TWorker>,
}
impl<TOuter, TInner, TFactory, TWorker> FilteredWorkerFactory<TOuter, TInner, TFactory, TWorker>
where
    TFactory: WorkerFactory<TInner, Worker = TWorker>,
    TWorker: Worker<TInner>,
    TOuter: Action + InboundAction<TInner> + OutboundAction<TInner>,
    TInner: Action,
{
    fn new(factory: TFactory) -> Self {
        Self {
            factory,
            _outer: Default::default(),
            _inner: Default::default(),
            _worker: Default::default(),
        }
    }
}
impl<TOuter, TInner, TFactory, TWorker> WorkerFactory<TOuter>
    for FilteredWorkerFactory<TOuter, TInner, TFactory, TWorker>
where
    TFactory: WorkerFactory<TInner, Worker = TWorker>,
    TWorker: Worker<TInner>,
    TOuter: Action + InboundAction<TInner> + OutboundAction<TInner> + Send + 'static,
    TInner: Action + Send + 'static,
{
    type Worker = FilteredWorker<TOuter, TInner, TWorker>;
    fn create(&self) -> Self::Worker {
        FilteredWorker::new(self.factory.create())
    }
}

struct FilteredWorker<TOuter, TInner, TWorker>
where
    TWorker: Worker<TInner>,
    TOuter: InboundAction<TInner> + OutboundAction<TInner>,
    TInner: Action,
{
    worker: TWorker,
    _outer: PhantomData<TOuter>,
    _inner: PhantomData<TInner>,
}
impl<TOuter, TInner, TWorker> FilteredWorker<TOuter, TInner, TWorker>
where
    TWorker: Worker<TInner>,
    TOuter: Action + InboundAction<TInner> + OutboundAction<TInner>,
    TInner: Action,
{
    fn new(worker: TWorker) -> Self {
        Self {
            worker,
            _outer: Default::default(),
            _inner: Default::default(),
        }
    }
}
impl<TOuter, TInner, TWorker> Worker<TOuter> for FilteredWorker<TOuter, TInner, TWorker>
where
    TWorker: Worker<TInner>,
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
        StateOperation::Spawn(pid, factory) => StateOperation::<TOuter>::spawn(
            pid,
            FilteredWorkerFactory::<TOuter, TInner, _, _>::new(factory),
        ),
        StateOperation::Kill(pid) => StateOperation::<TOuter>::Kill(pid),
    }
}
