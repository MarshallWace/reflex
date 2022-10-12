// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use crate::{Actor, ActorTransition, HandlerContext, MessageData, NamedAction};
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
impl<T, TAction> Actor<TAction> for InstrumentedActor<T>
where
    T: Actor<TAction>,
    TAction: NamedAction,
{
    type State = T::State;

    fn init(&self) -> Self::State {
        self.metric_names.init();
        self.inner.init()
    }

    fn handle(
        &self,
        state: Self::State,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> ActorTransition<Self::State, TAction> {
        let starting_time = std::time::Instant::now();
        let inner_return = self.inner.handle(state, action, metadata, context);
        let labels = [("actor", self.actor_name), ("action_type", action.name())];
        histogram!(
            self.metric_names.action_processing_time_micros,
            starting_time.elapsed().as_micros() as f64,
            &labels
        );
        inner_return
    }
}
