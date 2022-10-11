// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use metrics::{describe_histogram, histogram, Unit};
use crate::{Action, Actor, ActorTransition, HandlerContext, MessageData};

#[derive(Clone, Copy)]
pub struct InstrumentedActorNames {
    actor_handle_duration_micros: &'static str,
}
impl Default for InstrumentedActorNames {
    fn default() -> Self {
        Self {
            actor_handle_duration_micros: "actor_handle_duration_micros",
        }
    }
}
impl InstrumentedActorNames {
    fn init(&self) {
        describe_histogram!(
            self.actor_handle_duration_micros,
            Unit::Microseconds,
            "Time spent in a single actors handle function"
        );
    }
}

#[derive(Clone)]
pub struct InstrumentedActor<T1>{
    inner: T1,
    labels: Vec<(String, String)>,
    metric_names: InstrumentedActorNames,
}
impl<T1> InstrumentedActor<T1> {
    pub fn new(inner: T1, label: &str, metric_names: InstrumentedActorNames) -> Self {
        let labels = vec![("actor".to_string(), label.to_string())];
        Self {
            inner,
            labels,
            metric_names
        }
    }
}
impl<T1, TAction> Actor<TAction> for InstrumentedActor<T1>
where
    T1: Actor<TAction>,
    TAction: Action,
{
    type State = T1::State;

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

        histogram!(
            self.metric_names.actor_handle_duration_micros,
            starting_time.elapsed().as_micros() as f64,
            &self.labels
        );
        inner_return
    }
}
