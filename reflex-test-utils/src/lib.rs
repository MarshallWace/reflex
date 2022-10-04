// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use metrics_exporter_prometheus::{PrometheusBuilder, PrometheusHandle};
use std::sync::Mutex;

/// Metrics recorders are not supposed to be switched out during runtime (doing so is unsafe)
/// so to enable isolated testing for metrics we use a global mutex to ensure no two tests that
/// are witnessing metrics behaviour run simultaneously
static METRICS_RECORDER_LOCK: Mutex<()> = Mutex::new(());

pub fn run_metrics_test(test: impl FnOnce(&PrometheusHandle) -> ()) {
    let lock = METRICS_RECORDER_LOCK.lock(). expect("Unable to acquire prometheus lock for metrics test, likely another test failed whilst holding it");
    let handle = PrometheusBuilder::new()
        .install_recorder()
        .expect("Unable to install recorder");
    test(&handle);
    metrics::clear_recorder();
    drop(lock);
}
