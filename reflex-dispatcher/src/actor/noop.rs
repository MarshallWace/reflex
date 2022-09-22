// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    Action, Actor, ActorTransition, HandlerContext, MessageData, MiddlewareContext, PostMiddleware,
    PostMiddlewareTransition, PreMiddleware, PreMiddlewareTransition, StateOperation,
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
        operations: Vec<StateOperation<TAction>>,
        _metadata: &MessageData,
        _context: &MiddlewareContext,
    ) -> PostMiddlewareTransition<Self::State, TAction> {
        PostMiddlewareTransition::new(state, operations)
    }
}
