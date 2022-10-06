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
    let lock = METRICS_RECORDER_LOCK.lock().unwrap_or_else(|err| {
        // It's safe to use the poisoned mutex guard because we will have already dealt with the
        // panic scenario (see below)
        err.into_inner()
    });
    match PrometheusBuilder::new().install_recorder() {
        Err(err) => {
            drop(lock);
            panic!("Unable to install Prometheus metrics recorder: {}", err)
        }
        Ok(handle) => {
            /// This guard allows us to safely use the lock, whilst also allowing the thread to
            /// panic whilst holding it. When the thread panics (i.e. the test fails) the PanicGuard
            /// drop() method will run, clearing the recorder and cleanly releasing the lock,
            /// allowing another thread to pick up the lock and run its own tests.
            /// This is roughly equivalent to a 'finally' block in languages that use try/catch.
            struct PanicGuard<'a>(Option<MutexGuard<'a, ()>>);
            impl<'a> Drop for PanicGuard<'a> {
                fn drop(&mut self) {
                    let Self(lock) = self;
                    if let Some(lock) = lock.take() {
                        // Perform cleanup actions
                        metrics::clear_recorder();
                        // Release the lock for use in other tests
                        drop(lock);
                    }
                }
            }
            let guard = PanicGuard(Some(lock));
            // Run the test (bearing in mind that the PanicGuard drop() cleanup method will always
            // run even if the test panics e.g. due to an assertion failure)
            test(&handle);
            // Test completed successfully, explicitly perform cleanup
            drop(guard)
        }
    };
}
