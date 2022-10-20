// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::marker::PhantomData;

use metrics::{describe_counter, increment_counter, Unit};
use reflex_dispatcher::{
    Action, MessageData, MiddlewareContext, NamedAction, SchedulerCommand, TaskFactory,
};

use crate::logger::ActionLogger;

#[derive(Clone, Copy, Debug)]
pub struct PrometheusLoggerMetricNames {
    pub total_action_count: &'static str,
}
impl PrometheusLoggerMetricNames {
    fn init(self) -> Self {
        describe_counter!(self.total_action_count, Unit::Count, "Total action count");
        self
    }
}
impl Default for PrometheusLoggerMetricNames {
    fn default() -> Self {
        Self {
            total_action_count: "total_action_count",
        }
    }
}

pub trait PrometheusLoggerAction: NamedAction {}
impl<_Self> PrometheusLoggerAction for _Self where Self: NamedAction {}

#[derive(Clone)]
pub struct PrometheusLogger<TAction: Action, TTask: TaskFactory<TAction, TTask>> {
    metric_names: PrometheusLoggerMetricNames,
    _action: PhantomData<TAction>,
    _task: PhantomData<TTask>,
}
impl<TAction: Action, TTask: TaskFactory<TAction, TTask>> PrometheusLogger<TAction, TTask> {
    pub fn new(metric_names: PrometheusLoggerMetricNames) -> Self {
        Self {
            metric_names: metric_names.init(),
            _action: PhantomData,
            _task: PhantomData,
        }
    }
}
impl<TAction, TTask> ActionLogger for PrometheusLogger<TAction, TTask>
where
    TAction: Action + PrometheusLoggerAction,
    TTask: TaskFactory<TAction, TTask>,
{
    type Action = TAction;
    type Task = TTask;
    fn log(
        &mut self,
        operation: &SchedulerCommand<Self::Action, Self::Task>,
        _metadata: Option<&MessageData>,
        _context: Option<&MiddlewareContext>,
    ) {
        if let SchedulerCommand::Send(_pid, action) = operation {
            let metric_labels = [("type", action.name())];
            increment_counter!(self.metric_names.total_action_count, &metric_labels);
        }
    }
}
