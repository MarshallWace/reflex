// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    Action, Actor, ActorTransition, HandlerContext, MessageData, MiddlewareContext, PostMiddleware,
    PostMiddlewareTransition, PreMiddleware, PreMiddlewareTransition, StateOperation,
};

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
        operations: Vec<StateOperation<TAction>>,
        metadata: &MessageData,
        context: &MiddlewareContext,
    ) -> PostMiddlewareTransition<Self::State, TAction> {
        match (self, state) {
            (Some(inner), Some(state)) => {
                let (state, operations) = inner
                    .handle(state, operations, metadata, context)
                    .into_parts();
                PostMiddlewareTransition::new(Some(state), operations)
            }
            (_, state) => PostMiddlewareTransition::new(state, operations),
        }
    }
}
