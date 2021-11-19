// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::HashMap,
    io::Write,
    path::Path,
    sync::{Arc, Mutex},
};

use futures::future;
use futures_util::{FutureExt, StreamExt};
use reflex::{
    compiler::Compile,
    core::{Signal, SignalId, StateToken, StringValue},
};
use reflex_runtime::{
    AsyncExpression, RuntimeEffect, SignalHandler, SignalHelpers, SignalResult, StateUpdate,
    StateUpdateType,
};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Serialize, Deserialize)]
struct SignalLogEntry<T: AsyncExpression> {
    signal: Signal<T>,
    value: SignalLogType<T>,
}
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(tag = "type", content = "payload")]
enum SignalLogType<T: AsyncExpression> {
    InitialValue(T),
    SyncEffect(Vec<SanitizedStateUpdate<T>>),
    AsyncEffect(Vec<SanitizedStateUpdate<T>>),
    StreamEffect(Vec<SanitizedStateUpdate<T>>),
}
#[derive(Clone, Debug, Serialize, Deserialize)]
struct SanitizedStateUpdate<T> {
    target: StateToken,
    value: T,
}

#[derive(Clone)]
struct SignalCapture<T: AsyncExpression> {
    recorded_signals: Arc<Mutex<HashMap<SignalId, Vec<SignalLogEntry<T>>>>>,
}
impl<'a, T: AsyncExpression + serde::Deserialize<'a>> SignalCapture<T> {
    fn from_file(capture_file: &Path) -> Result<Self, String> {
        let signal_results = load_capture_file(capture_file)?;
        Ok(Self {
            recorded_signals: Arc::new(Mutex::new(signal_results)),
        })
    }
    fn retrieve_captured_signal(&self, signal: &Signal<T>) -> Option<SignalResult<T>>
    where
        T: AsyncExpression + Compile<T>,
        T::String: StringValue + Send + Sync,
    {
        let captured_records = self.recorded_signals.lock().unwrap();
        captured_records
            .get(&signal.id())
            .map(|updates| generate_mock_signal_handler_result(updates).unwrap())
    }
}
fn load_capture_file<'a, T: AsyncExpression + serde::Deserialize<'a>>(
    capture_file: &Path,
) -> Result<HashMap<SignalId, Vec<SignalLogEntry<T>>>, String> {
    let mut deserialized = HashMap::new();
    let capture_file = std::fs::File::open(capture_file)
        .map_err(|err| format!("Failed to open {}: {}", capture_file.display(), err))?;
    let records: Result<Vec<SignalLogEntry<T>>, serde_json::Error> =
        serde_json::Deserializer::from_reader(capture_file)
            .into_iter::<SignalLogEntry<T>>()
            .collect();
    let records = records.map_err(|err| format!("Unable to deserialize record: {}", err))?;
    for record in records {
        deserialized
            .entry(record.signal.id())
            .or_insert(vec![])
            .push(record);
    }
    Ok(deserialized)
}
fn generate_mock_signal_handler_result<T: AsyncExpression>(
    updates: &[SignalLogEntry<T>],
) -> Result<SignalResult<T>, String> {
    let initial_value = match updates.first().map(|entry| &entry.value) {
        Some(SignalLogType::InitialValue(update)) => Ok(update.clone()),
        _ => Err(String::from("Missing initial signal value")),
    }?;
    let effect_updates = &updates[1..];
    let effect = match effect_updates.first().map(|entry| &entry.value) {
        None => None,
        Some(SignalLogType::InitialValue(_)) => {
            return Err(String::from(
                "Signal capture contains multiple handler results",
            ));
        }
        Some(SignalLogType::SyncEffect(updates)) => {
            if effect_updates.len() > 1 {
                return Err(String::from("Expected single effect from a sync effect"));
            }
            let updates: Vec<StateUpdate<T>> = desanitize_effect_updates(updates);
            Some(RuntimeEffect::Sync(updates))
        }
        Some(SignalLogType::AsyncEffect(updates)) => {
            if effect_updates.len() > 1 {
                return Err(String::from("Expected single effect from an async effect"));
            }
            let updates: Vec<StateUpdate<T>> = desanitize_effect_updates(updates);
            Some(RuntimeEffect::Async((
                Box::pin(async move { updates }),
                Box::pin(future::ready(())),
            )))
        }
        Some(SignalLogType::StreamEffect(_)) => {
            let stream_emissions = effect_updates
                .into_iter()
                .map(|update| match &update.value {
                    SignalLogType::StreamEffect(updates) => Ok(desanitize_effect_updates(updates)),
                    _ => Err(String::from(
                        "The same signal returned different types of effects",
                    )),
                })
                .collect::<Result<Vec<_>, String>>()?;
            Some(RuntimeEffect::Stream((
                Box::pin(futures::stream::iter(stream_emissions)),
                Box::pin(future::ready(())),
            )))
        }
    };
    Ok((initial_value, effect))
}

pub struct SignalRecorder<T>
where
    T: AsyncExpression,
{
    recorded_signal_types: std::collections::HashSet<String>,
    sender: tokio::sync::mpsc::UnboundedSender<SignalLogEntry<T>>,
}
impl<T> SignalRecorder<T>
where
    T: AsyncExpression + Compile<T> + Serialize,
    T::String: Send + Sync,
{
    pub fn new(
        recorded_signal_types: impl IntoIterator<Item = String>,
        capture_file: &Path,
    ) -> Result<Self, String> {
        let mut output_file = std::fs::File::create(&capture_file).map_err(|err| {
            format!(
                "Failed to open signal capture output file {}: {}",
                capture_file.display(),
                err
            )
        })?;
        let (sender, mut receiver) = tokio::sync::mpsc::unbounded_channel();
        tokio::spawn(async move {
            while let Some(message) = receiver.recv().await {
                let _ = serde_json::to_writer(&mut output_file, &message);
                let _ = output_file.write(b"\n");
                let _ = output_file.flush();
            }
        });
        Ok(Self {
            recorded_signal_types: recorded_signal_types.into_iter().collect(),
            sender,
        })
    }
    pub fn wrap_signal_handler(&self, handler: impl SignalHandler<T>) -> impl SignalHandler<T> {
        let sender = self.sender.clone();
        let recorded_signal_types = self.recorded_signal_types.clone();
        move |signal_type: &str, signals: &[&Signal<T>], helpers: &SignalHelpers<T>| {
            let result = handler.handle(signal_type, signals, helpers);
            if recorded_signal_types.contains(signal_type) {
                eprintln!("[capture] Capturing signal type {}", &signal_type);
                result.map(|results| {
                    results
                        .into_iter()
                        .filter_map(|result| result.ok())
                        .zip(signals)
                        .map(|((initial_value, effect), signal)| {
                            let transformed_effect = Self::log_signal_results(
                                signal,
                                &initial_value,
                                effect,
                                sender.clone(),
                            );
                            Ok((initial_value, transformed_effect))
                        })
                        .collect()
                })
            } else {
                result
            }
        }
    }
    fn log_signal_results(
        signal: &Signal<T>,
        initial_value: &T,
        effect: Option<RuntimeEffect<T>>,
        sender: tokio::sync::mpsc::UnboundedSender<SignalLogEntry<T>>,
    ) -> Option<RuntimeEffect<T>> {
        let _ = sender.send(SignalLogEntry {
            signal: signal.clone(),
            value: SignalLogType::InitialValue(initial_value.clone()),
        });
        effect.map(move |runtime_effect| {
            let signal = signal.clone();
            match runtime_effect {
                RuntimeEffect::Sync(updates) => {
                    let _ = sender.send(SignalLogEntry {
                        signal,
                        value: SignalLogType::SyncEffect(sanitize_effect_updates(&updates)),
                    });
                    RuntimeEffect::Sync(updates)
                }
                RuntimeEffect::Async((effect, dispose)) => RuntimeEffect::Async((
                    Box::pin(effect.inspect(move |updates| {
                        let _ = sender.send(SignalLogEntry {
                            signal,
                            value: SignalLogType::AsyncEffect(sanitize_effect_updates(updates)),
                        });
                    })),
                    dispose,
                )),
                RuntimeEffect::Stream((effect, dispose)) => {
                    let transformed = effect.inspect(move |updates| {
                        let _ = sender.send(SignalLogEntry {
                            signal: signal.clone(),
                            value: SignalLogType::StreamEffect(sanitize_effect_updates(&updates)),
                        });
                    });
                    RuntimeEffect::Stream((Box::pin(transformed), dispose))
                }
            }
        })
    }
}

pub struct SignalPlayback<T: AsyncExpression> {
    recorded_signal_types: std::collections::HashSet<String>,
    capture: SignalCapture<T>,
}
impl<'a, T: AsyncExpression + serde::Deserialize<'a>> SignalPlayback<T> {
    pub fn new(
        recorded_signal_types: impl IntoIterator<Item = String>,
        capture_file: &Path,
    ) -> Result<Self, String> {
        let capture = SignalCapture::from_file(capture_file)?;
        Ok(Self {
            recorded_signal_types: recorded_signal_types.into_iter().collect(),
            capture,
        })
    }
    pub fn wrap_signal_handler(&self, handler: impl SignalHandler<T>) -> impl SignalHandler<T>
    where
        T: AsyncExpression + Compile<T>,
        T::String: Send + Sync,
    {
        let recorded_signal_types = self.recorded_signal_types.clone();
        let capture = self.capture.clone();
        move |signal_type: &str, signals: &[&Signal<T>], helpers: &SignalHelpers<T>| {
            if recorded_signal_types.contains(signal_type) {
                Some(
                    signals
                        .iter()
                        .map(|signal| match capture.retrieve_captured_signal(signal) {
                            Some(result) => {
                                eprintln!(
                                    "[capture] Replaying signal: <signal:{}:{}>",
                                    signal.signal_type(),
                                    signal.id()
                                );
                                Ok(result)
                            }
                            None => {
                                eprintln!(
                                    "[capture] Signal capture not found: <signal:{}:{}>",
                                    signal.signal_type(),
                                    signal.id()
                                );
                                Err(format!(
                                    "Failed to find signal {} in signal capture",
                                    signal.id()
                                ))
                            }
                        })
                        .collect::<Vec<Result<SignalResult<T>, String>>>(),
                )
            } else {
                handler.handle(signal_type, signals, helpers)
            }
        }
    }
}

fn sanitize_effect_updates<T: AsyncExpression>(
    updates: &Vec<StateUpdate<T>>,
) -> Vec<SanitizedStateUpdate<T>> {
    updates
        .iter()
        .filter_map(|update| match update.update() {
            StateUpdateType::Value(value) => Some(SanitizedStateUpdate {
                target: update.state_token(),
                value: value.clone(),
            }),
            StateUpdateType::Patch(_) => {
                eprintln!(
                    "[capture] Unable to record patch update for state token {}",
                    update.state_token()
                );
                None
            }
        })
        .collect()
}

fn desanitize_effect_updates<T: AsyncExpression>(
    updates: &[SanitizedStateUpdate<T>],
) -> Vec<StateUpdate<T>>
where
    T: AsyncExpression,
{
    updates
        .iter()
        .cloned()
        .map(
            |SanitizedStateUpdate {
                 target: state_token,
                 value,
             }| StateUpdate::value(state_token, value),
        )
        .collect()
}
