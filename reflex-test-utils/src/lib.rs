// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    panic::UnwindSafe,
    sync::{Mutex, MutexGuard},
};

use metrics_exporter_prometheus::{PrometheusBuilder, PrometheusHandle};

/// Metrics recorders are not supposed to be switched out during runtime (doing so is unsafe)
/// so to enable isolated testing for metrics we use a global mutex to ensure no two tests that
/// are witnessing metrics behaviour run simultaneously
static METRICS_RECORDER_LOCK: Mutex<()> = Mutex::new(());

pub fn run_metrics_test(test: (impl FnOnce(&PrometheusHandle) -> () + UnwindSafe)) {
    let lock = METRICS_RECORDER_LOCK
        .lock()
        .unwrap_or_else(|err| err.into_inner());
    match PrometheusBuilder::new().install_recorder() {
        Err(err) => {
            drop(lock);
            panic!("Unable to install Prometheus metrics recorder: {}", err)
        }
        Ok(handle) => {
            /// This allows us to safely use the lock, whilst also allowing other threads to panic
            /// whilst holding it. When another thread panics (i.e. a test fails) the drop
            /// will run, clearing the recorder and cleanly dropping the lock. Other threads can
            /// continue with their tests regardless.
            struct PanicGuard<'a>(Option<MutexGuard<'a, ()>>);
            impl<'a> Drop for PanicGuard<'a> {
                fn drop(&mut self) {
                    let Self(lock) = self;
                    if let Some(lock) = lock.take() {
                        metrics::clear_recorder();
                        drop(lock);
                    }
                }
            }
            let guard = PanicGuard(Some(lock));
            test(&handle);
            drop(guard)
        }
    };
}
