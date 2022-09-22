// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    Action, Actor, ActorTransition, HandlerContext, MessageData, MiddlewareContext, PostMiddleware,
    PostMiddlewareTransition, PreMiddleware, PreMiddlewareTransition, StateOperation,
};

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
        operations: Vec<StateOperation<TAction>>,
        metadata: &MessageData,
        context: &MiddlewareContext,
    ) -> PostMiddlewareTransition<Self::State, TAction> {
        match (self, state) {
            (Self::Left(actor), Self::State::Left(state)) => {
                let (state, operations) = actor
                    .handle(state, operations, metadata, context)
                    .into_parts();
                PostMiddlewareTransition::new(Self::State::Left(state), operations)
            }
            (Self::Right(actor), Self::State::Right(state)) => {
                let (state, operations) = actor
                    .handle(state, operations, metadata, context)
                    .into_parts();
                PostMiddlewareTransition::new(Self::State::Right(state), operations)
            }
            (_, state) => PostMiddlewareTransition::new(state, operations),
        }
    }
}
