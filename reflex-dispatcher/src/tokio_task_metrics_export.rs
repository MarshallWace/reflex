// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use metrics::{describe_gauge, gauge, Unit};
use std::time::Duration;

#[derive(Clone, Copy, Debug)]
pub struct TokioTaskMetricNames {
    tokio_task_dropped_count: &'static str,
    tokio_task_first_poll_count: &'static str,
    tokio_task_instrumented_count: &'static str,
    tokio_task_total_fast_poll_count: &'static str,
    tokio_task_total_fast_poll_duration: &'static str,
    tokio_task_total_first_poll_delay: &'static str,
    tokio_task_total_idle_duration: &'static str,
    tokio_task_total_idled_count: &'static str,
    tokio_task_total_poll_count: &'static str,
    tokio_task_total_poll_duration: &'static str,
    tokio_task_total_scheduled_count: &'static str,
    tokio_task_total_scheduled_duration: &'static str,
    tokio_task_total_slow_poll_count: &'static str,
    tokio_task_total_slow_poll_duration: &'static str,
    tokio_task_mean_fast_poll_duration: &'static str,
    tokio_task_mean_first_poll_delay: &'static str,
    tokio_task_mean_idle_duration: &'static str,
    tokio_task_mean_poll_duration: &'static str,
    tokio_task_mean_scheduled_duration: &'static str,
    tokio_task_mean_slow_poll_duration: &'static str,
    tokio_task_slow_poll_ratio: &'static str,
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
pub fn get_task_monitor(
    tokio_task_metric_names: &TokioTaskMetricNames,
    task_type: &str,
) -> tokio_metrics::TaskMonitor {
    let task_monitor = tokio_metrics::TaskMonitor::new();
    describe_gauge!(
        tokio_task_metric_names.tokio_task_dropped_count,
        Unit::Count,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.dropped_count"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_first_poll_count,
        Unit::Count,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.first_poll_count"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_instrumented_count,
        Unit::Count,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.instrumented_count"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_total_fast_poll_count,
        Unit::Count,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.total_fast_poll_count"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_total_fast_poll_duration,
        Unit::Microseconds,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.total_fast_poll_duration"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_total_first_poll_delay,
        Unit::Microseconds,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.total_first_poll_delay"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_total_idle_duration,
        Unit::Microseconds,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.total_idle_duration"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_total_idled_count,
        Unit::Count,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.total_idled_count"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_total_poll_count,
        Unit::Count,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.total_poll_count"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_total_poll_duration,
        Unit::Microseconds,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.total_poll_duration"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_total_scheduled_count,
        Unit::Count,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.total_scheduled_count"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_total_scheduled_duration,
        Unit::Microseconds,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.total_scheduled_duration"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_total_slow_poll_count,
        Unit::Count,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.total_slow_poll_count"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_total_slow_poll_duration,
        Unit::Microseconds,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.total_slow_poll_duration"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_mean_fast_poll_duration,
        Unit::Microseconds,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#method.mean_fast_poll_duration"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_mean_first_poll_delay,
        Unit::Microseconds,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#method.mean_first_poll_delay"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_mean_idle_duration,
        Unit::Microseconds,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#method.mean_idle_duration"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_mean_poll_duration,
        Unit::Microseconds,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#method.mean_poll_duration"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_mean_scheduled_duration,
        Unit::Microseconds,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#method.mean_scheduled_duration"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_mean_slow_poll_duration,
        Unit::Microseconds,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#method.mean_slowehttps://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.htmlpoll_duration"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_slow_poll_ratio,
        Unit::Count,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#method.slow_poll_ratio"
    );

    {
        let tokio_task_metric_names = tokio_task_metric_names.clone();
        let task_monitor = task_monitor.clone();
        let task_type = task_type.to_string();
        tokio::spawn(async move {
            let labels = [("task_type".to_string(), task_type)];
            for interval in task_monitor.intervals() {
                gauge!(
                    tokio_task_metric_names.tokio_task_dropped_count,
                    interval.dropped_count as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_first_poll_count,
                    interval.first_poll_count as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_instrumented_count,
                    interval.instrumented_count as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_total_fast_poll_count,
                    interval.total_fast_poll_count as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_total_fast_poll_duration,
                    interval.total_fast_poll_duration.as_micros() as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_total_first_poll_delay,
                    interval.total_first_poll_delay.as_micros() as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_total_idle_duration,
                    interval.total_idle_duration.as_micros() as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_total_idled_count,
                    interval.total_idled_count as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_total_poll_count,
                    interval.total_poll_count as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_total_poll_duration,
                    interval.total_poll_duration.as_micros() as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_total_scheduled_count,
                    interval.total_scheduled_count as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_total_scheduled_duration,
                    interval.total_scheduled_duration.as_micros() as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_total_slow_poll_count,
                    interval.total_slow_poll_count as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_total_slow_poll_duration,
                    interval.total_slow_poll_duration.as_micros() as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_mean_fast_poll_duration,
                    interval.mean_fast_poll_duration().as_micros() as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_mean_first_poll_delay,
                    interval.mean_first_poll_delay().as_micros() as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_mean_idle_duration,
                    interval.mean_idle_duration().as_micros() as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_mean_poll_duration,
                    interval.mean_poll_duration().as_micros() as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_mean_scheduled_duration,
                    interval.mean_scheduled_duration().as_micros() as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_mean_slow_poll_duration,
                    interval.mean_slow_poll_duration().as_micros() as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_slow_poll_ratio,
                    interval.slow_poll_ratio() as f64,
                    &labels
                );

                tokio::time::sleep(Duration::from_secs(1)).await;
            }
        });
    }

    task_monitor
}
