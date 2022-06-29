// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::marker::PhantomData;

use crate::{
    Action, Actor, ActorTransition, HandlerContext, MessageData, MiddlewareContext, PreMiddleware,
    PreMiddlewareTransition, StateOperation, StateTransition,
};

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
