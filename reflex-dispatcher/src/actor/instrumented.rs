// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    Action, Actor, ActorInitContext, Handler, HandlerContext, MessageData, Named, SchedulerMode,
    TaskFactory, TaskInbox, Worker,
};
use metrics::{describe_histogram, histogram, Unit};

#[derive(Clone, Copy, Debug)]
pub struct InstrumentedActorMetricNames {
    pub action_processing_time_micros: &'static str,
}
impl Default for InstrumentedActorMetricNames {
    fn default() -> Self {
        Self {
            action_processing_time_micros: "action_processing_time_micros",
        }
    }
}
impl InstrumentedActorMetricNames {
    fn init(&self) {
        describe_histogram!(
            self.action_processing_time_micros,
            Unit::Microseconds,
            "Time spent processing action (microseconds)"
        );
    }
}

#[derive(Clone)]
pub struct InstrumentedActor<T> {
    inner: T,
    actor_name: &'static str,
    metric_names: InstrumentedActorMetricNames,
}
impl<T> Named for InstrumentedActor<T>
where
    T: Named,
{
    fn name(&self) -> &'static str {
        self.inner.name()
    }
}
impl<T> InstrumentedActor<T> {
    pub fn new(
        inner: T,
        actor_name: &'static str,
        metric_names: InstrumentedActorMetricNames,
    ) -> Self {
        Self {
            inner,
            actor_name,
            metric_names,
        }
    }
}

impl<T, TAction, TTask> Actor<TAction, TTask> for InstrumentedActor<T>
where
    T: Actor<TAction, TTask>,
    TAction: Action + Named,
    TTask: TaskFactory<TAction, TTask>,
{
    type Events<TInbox: TaskInbox<TAction>> = T::Events<TInbox>;
    type Dispose = T::Dispose;
    fn init<TInbox: TaskInbox<TAction>>(
        &self,
        inbox: TInbox,
        context: &impl ActorInitContext,
    ) -> (Self::State, Self::Events<TInbox>, Self::Dispose) {
        self.metric_names.init();
        self.inner.init(inbox, context)
    }
}

impl<T, I, O> Worker<I, O> for InstrumentedActor<T>
where
    T: Worker<I, O>,
    I: Named,
{
    fn accept(&self, message: &I) -> bool {
        self.inner.accept(message)
    }
    fn schedule(&self, message: &I, state: &Self::State) -> Option<SchedulerMode> {
        self.inner.schedule(message, state)
    }
}

impl<T, I, O> Handler<I, O> for InstrumentedActor<T>
where
    T: Handler<I, O>,
    I: Named,
{
    type State = T::State;
    fn handle(
        &self,
        state: &mut Self::State,
        message: &I,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<O> {
        let starting_time = std::time::Instant::now();
        let result = self.inner.handle(state, message, metadata, context);
        let labels = [("actor", self.actor_name), ("action_type", message.name())];
        histogram!(
            self.metric_names.action_processing_time_micros,
            starting_time.elapsed().as_micros() as f64,
            &labels
        );
        result
    }
}
