// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::marker::PhantomData;

use futures::StreamExt;

use crate::{
    Action, Actor, ActorTransition, HandlerContext, InboundAction, MessageData, OperationStream,
    OutboundAction, StateOperation, StateTransition, Worker, WorkerContext, WorkerFactory,
    WorkerTransition,
};

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
        action: TOuter,
        metadata: &MessageData,
        context: &mut WorkerContext,
    ) -> WorkerTransition<TOuter> {
        let actions = if let Some(action) = action.into_type() {
            let operations = self.worker.handle(action, metadata, context).into_iter();
            Some(operations.map(transform_state_operation))
        } else {
            None
        };
        WorkerTransition::new(actions.into_iter().flatten())
    }
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
