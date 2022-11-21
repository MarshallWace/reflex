// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use metrics::{describe_gauge, Unit};

#[derive(Clone, Copy, Debug)]
pub struct TokioTaskMetricNames {
    pub tokio_task_dropped_count: &'static str,
    pub tokio_task_first_poll_count: &'static str,
    pub tokio_task_instrumented_count: &'static str,
    pub tokio_task_total_fast_poll_count: &'static str,
    pub tokio_task_total_fast_poll_duration: &'static str,
    pub tokio_task_total_first_poll_delay: &'static str,
    pub tokio_task_total_idle_duration: &'static str,
    pub tokio_task_total_idled_count: &'static str,
    pub tokio_task_total_poll_count: &'static str,
    pub tokio_task_total_poll_duration: &'static str,
    pub tokio_task_total_scheduled_count: &'static str,
    pub tokio_task_total_scheduled_duration: &'static str,
    pub tokio_task_total_slow_poll_count: &'static str,
    pub tokio_task_total_slow_poll_duration: &'static str,
    pub tokio_task_mean_fast_poll_duration: &'static str,
    pub tokio_task_mean_first_poll_delay: &'static str,
    pub tokio_task_mean_idle_duration: &'static str,
    pub tokio_task_mean_poll_duration: &'static str,
    pub tokio_task_mean_scheduled_duration: &'static str,
    pub tokio_task_mean_slow_poll_duration: &'static str,
    pub tokio_task_slow_poll_ratio: &'static str,
}
impl TokioTaskMetricNames {
    pub fn init(self) -> Self {
        describe_gauge!(
            self.tokio_task_dropped_count,
            Unit::Count,
            "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.dropped_count"
        );
        describe_gauge!(
            self.tokio_task_first_poll_count,
            Unit::Count,
            "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.first_poll_count"
        );
        describe_gauge!(
            self.tokio_task_instrumented_count,
            Unit::Count,
            "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.instrumented_count"
        );
        describe_gauge!(
            self.tokio_task_total_fast_poll_count,
            Unit::Count,
            "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.total_fast_poll_count"
        );
        describe_gauge!(
            self.tokio_task_total_fast_poll_duration,
            Unit::Microseconds,
            "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.total_fast_poll_duration"
        );
        describe_gauge!(
            self.tokio_task_total_first_poll_delay,
            Unit::Microseconds,
            "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.total_first_poll_delay"
        );
        describe_gauge!(
            self.tokio_task_total_idle_duration,
            Unit::Microseconds,
            "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.total_idle_duration"
        );
        describe_gauge!(
            self.tokio_task_total_idled_count,
            Unit::Count,
            "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.total_idled_count"
        );
        describe_gauge!(
            self.tokio_task_total_poll_count,
            Unit::Count,
            "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.total_poll_count"
        );
        describe_gauge!(
            self.tokio_task_total_poll_duration,
            Unit::Microseconds,
            "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.total_poll_duration"
        );
        describe_gauge!(
            self.tokio_task_total_scheduled_count,
            Unit::Count,
            "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.total_scheduled_count"
        );
        describe_gauge!(
            self.tokio_task_total_scheduled_duration,
            Unit::Microseconds,
            "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.total_scheduled_duration"
        );
        describe_gauge!(
            self.tokio_task_total_slow_poll_count,
            Unit::Count,
            "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.total_slow_poll_count"
        );
        describe_gauge!(
            self.tokio_task_total_slow_poll_duration,
            Unit::Microseconds,
            "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.total_slow_poll_duration"
        );
        describe_gauge!(
            self.tokio_task_mean_fast_poll_duration,
            Unit::Microseconds,
            "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#method.mean_fast_poll_duration"
        );
        describe_gauge!(
            self.tokio_task_mean_first_poll_delay,
            Unit::Microseconds,
            "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#method.mean_first_poll_delay"
        );
        describe_gauge!(
            self.tokio_task_mean_idle_duration,
            Unit::Microseconds,
            "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#method.mean_idle_duration"
        );
        describe_gauge!(
            self.tokio_task_mean_poll_duration,
            Unit::Microseconds,
            "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#method.mean_poll_duration"
        );
        describe_gauge!(
            self.tokio_task_mean_scheduled_duration,
            Unit::Microseconds,
            "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#method.mean_scheduled_duration"
        );
        describe_gauge!(
            self.tokio_task_mean_slow_poll_duration,
            Unit::Microseconds,
            "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#method.mean_slowehttps://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.htmlpoll_duration"
        );
        describe_gauge!(
            self.tokio_task_slow_poll_ratio,
            Unit::Count,
            "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#method.slow_poll_ratio"
        );
        self
    }
}
impl Default for TokioTaskMetricNames {
    fn default() -> Self {
        Self {
            tokio_task_dropped_count: "tokio_task_dropped_count",
            tokio_task_first_poll_count: "tokio_task_first_poll_count",
            tokio_task_instrumented_count: "tokio_task_instrumented_count",
            tokio_task_total_fast_poll_count: "tokio_task_total_fast_poll_count",
            tokio_task_total_fast_poll_duration: "tokio_task_total_fast_poll_duration",
            tokio_task_total_first_poll_delay: "tokio_task_total_first_poll_delay",
            tokio_task_total_idle_duration: "tokio_task_total_idle_duration",
            tokio_task_total_idled_count: "tokio_task_total_idled_count",
            tokio_task_total_poll_count: "tokio_task_total_poll_count",
            tokio_task_total_poll_duration: "tokio_task_total_poll_duration",
            tokio_task_total_scheduled_count: "tokio_task_total_scheduled_count",
            tokio_task_total_scheduled_duration: "tokio_task_total_scheduled_duration",
            tokio_task_total_slow_poll_count: "tokio_task_total_slow_poll_count",
            tokio_task_total_slow_poll_duration: "tokio_task_total_slow_poll_duration",
            tokio_task_mean_fast_poll_duration: "tokio_task_mean_fast_poll_duration",
            tokio_task_mean_first_poll_delay: "tokio_task_mean_first_poll_delay",
            tokio_task_mean_idle_duration: "tokio_task_mean_idle_duration",
            tokio_task_mean_poll_duration: "tokio_task_mean_poll_duration",
            tokio_task_mean_scheduled_duration: "tokio_task_mean_scheduled_duration",
            tokio_task_mean_slow_poll_duration: "tokio_task_mean_slow_poll_duration",
            tokio_task_slow_poll_ratio: "tokio_task_slow_poll_ratio",
        }
    }
}
