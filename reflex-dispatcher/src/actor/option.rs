// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    Action, Actor, ActorEvents, Handler, HandlerContext, MessageData, Named, SchedulerMode,
    TaskFactory, TaskInbox, Worker,
};

impl<T> Named for Option<T>
where
    T: Named,
{
    fn name(&self) -> &'static str {
        match self {
            Some(inner) => inner.name(),
            None => "None",
        }
    }
}
impl<T, TAction, TTask> Actor<TAction, TTask> for Option<T>
where
    T: Actor<TAction, TTask>,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    type Events<TInbox: TaskInbox<TAction>> = <T as Actor<TAction, TTask>>::Events<TInbox>;
    type Dispose = <T as Actor<TAction, TTask>>::Dispose;
    fn init(&self) -> Self::State {
        self.as_ref().map(|inner| inner.init())
    }
    fn events<TInbox: TaskInbox<TAction>>(
        &self,
        inbox: TInbox,
    ) -> ActorEvents<TInbox, Self::Events<TInbox>, Self::Dispose> {
        match self {
            Some(inner) => inner.events(inbox),
            None => ActorEvents::Sync(inbox),
        }
    }
}

impl<T, I, O> Worker<I, O> for Option<T>
where
    T: Worker<I, O>,
{
    fn accept(&self, message: &I) -> bool {
        match self {
            Some(inner) => inner.accept(message),
            None => false,
        }
    }
    fn schedule(&self, message: &I, state: &Self::State) -> Option<SchedulerMode> {
        match (self, state) {
            (Some(inner), Some(state)) => inner.schedule(message, state),
            (_, _) => None,
        }
    }
}

impl<T, I, O> Handler<I, O> for Option<T>
where
    T: Handler<I, O>,
{
    type State = Option<T::State>;
    fn handle(
        &self,
        state: &mut Self::State,
        action: &I,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<O> {
        match (self, state) {
            (Some(inner), Some(state)) => inner.handle(state, action, metadata, context),
            (_, _) => None,
        }
    }
}
