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

use futures_util::{FutureExt, StreamExt};
use reflex::{
    compiler::Compile,
    core::{
        Expression, ExpressionFactory, HeapAllocator, Signal, SignalId, SignalType, StateToken,
        StringValue,
    },
};
use reflex_runtime::{
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, RuntimeEffect, SignalHandler,
    SignalHelpers, SignalResult, StateUpdate, StateUpdateType,
};
use serde::{Deserialize, Serialize};

use self::serialization::{to_signal_result, SignalCaptureRecord};

/// This will record signals for playback in a later run through of a program.
struct SignalRecorderActor<T>
where
    T: AsyncExpression,
{
    receiver: tokio::sync::mpsc::UnboundedReceiver<SignalRecorderMessage<T>>,
}
impl<T> SignalRecorderActor<T>
where
    T: AsyncExpression + Compile<T>,
    T::String: StringValue + Send + Sync,
{
    async fn run(&mut self, mut output_file: std::fs::File) {
        // Why not tokio file? Because tokio files do not implement Write, only
        // AsyncWrite which is not supported by serde. This means holding a file handle
        // across an await. TODO: is this a problem? signal recording should not
        // be used in production so probably not.
        while let Some(message) = self.receiver.recv().await {
            let record: serialization::SignalCaptureRecord = message.into();
            let _ = serde_json::to_writer(&mut output_file, &record);
            let _ = output_file.write(b"\n");
            let _ = output_file.flush();
        }
    }
}
struct UpdateRecord<T>(StateToken, T);
struct SignalExpressionRecord<T> {
    signal_id: SignalId,
    expression: T,
}
struct SyncEffectRecord<T>
where
    T: AsyncExpression,
{
    signal_id: SignalId,
    updates: Vec<UpdateRecord<T>>,
}
struct AsyncEffectRecord<T>
where
    T: AsyncExpression,
{
    signal_id: SignalId,
    updates: Vec<UpdateRecord<T>>,
}
struct StreamEffectRecord<T>
where
    T: AsyncExpression,
{
    signal_id: SignalId,
    updates: Vec<UpdateRecord<T>>,
}
enum SignalRecorderMessage<T>
where
    T: AsyncExpression,
{
    PendingSignal(SignalId),
    Expression(SignalExpressionRecord<T>),
    SyncEffect(SyncEffectRecord<T>),
    AsyncEffect(AsyncEffectRecord<T>),
    StreamEffect(StreamEffectRecord<T>),
}
pub struct SignalRecorder<T>
where
    T: AsyncExpression,
{
    recorded_signal_types: std::collections::HashSet<String>,
    sender: tokio::sync::mpsc::UnboundedSender<SignalRecorderMessage<T>>,
}
impl<T> SignalRecorder<T>
where
    T: AsyncExpression + Compile<T>,
    T::String: Send + Sync,
{
    pub fn new(
        recorded_signal_types: impl IntoIterator<Item = String>,
        capture_file: &Path,
    ) -> Result<Self, String> {
        let output_file = std::fs::File::create(&capture_file).map_err(|err| {
            format!(
                "ERROR: failed to open file for signal capture {}: {}",
                capture_file.display(),
                err
            )
        })?;
        let (sender, receiver) = tokio::sync::mpsc::unbounded_channel();
        let mut recorder = SignalRecorderActor { receiver };
        tokio::spawn(async move { recorder.run(output_file).await });
        Ok(Self {
            recorded_signal_types: recorded_signal_types.into_iter().collect(),
            sender,
        })
    }

    pub fn record_signal_handler(
        &self,
        handler: impl SignalHandler<T>,
        factory: &impl AsyncExpressionFactory<T>,
    ) -> impl SignalHandler<T> {
        let sender = self.sender.clone();
        let recorded_signal_types = self.recorded_signal_types.clone();
        let factory = factory.clone();
        move |signal_type: &str, signals: &[&Signal<T>], helpers: &SignalHelpers<T>| {
            let result = handler.handle(signal_type, signals, helpers);
            if recorded_signal_types.contains(signal_type) {
                eprintln!("Capturing signal for type {}", &signal_type);
                result.map(|results| {
                    results
                        .into_iter()
                        .filter_map(|result| result.ok())
                        .zip(signals)
                        .map(|((expr, runtime_effect), signal)| {
                            Self::record_expression(signal.id(), &expr, sender.clone(), &factory);
                            let transformed_effect = Self::decorate_effect_with_recording(
                                signal.id(),
                                runtime_effect,
                                sender.clone(),
                                &factory,
                            );
                            Ok((expr, transformed_effect))
                        })
                        .collect()
                })
            } else {
                result
            }
        }
    }

    fn decorate_effect_with_recording(
        signal_id: SignalId,
        result: Option<RuntimeEffect<T>>,
        sender: tokio::sync::mpsc::UnboundedSender<SignalRecorderMessage<T>>,
        factory: &impl AsyncExpressionFactory<T>,
    ) -> Option<RuntimeEffect<T>> {
        result.map(move |runtime_effect| {
            let factory = factory.clone();
            match runtime_effect {
                RuntimeEffect::Sync(updates) => {
                    let update_records = Self::to_update_records(&updates, &factory);
                    let effect_record = SyncEffectRecord {
                        signal_id,
                        updates: update_records,
                    };
                    let _ = sender.send(SignalRecorderMessage::SyncEffect(effect_record));
                    RuntimeEffect::Sync(updates)
                }
                RuntimeEffect::Async((effect, callback)) => RuntimeEffect::Async((
                    Box::pin(effect.inspect(move |updates| {
                        let updates = Self::to_update_records(updates, &factory);
                        let effect_record = AsyncEffectRecord { signal_id, updates };
                        let _ = sender.send(SignalRecorderMessage::AsyncEffect(effect_record));
                    })),
                    callback,
                )),
                RuntimeEffect::Stream((effect, _callback)) => {
                    let transformed = effect.map(move |updates| {
                        let update_records = Self::to_update_records(&updates, &factory);
                        if !update_records.is_empty() {
                            let effect_record = StreamEffectRecord {
                                signal_id,
                                updates: update_records,
                            };
                            let _ = sender.send(SignalRecorderMessage::StreamEffect(effect_record));
                        }
                        updates
                    });
                    RuntimeEffect::Stream((Box::pin(transformed), _callback))
                }
            }
        })
    }

    fn to_update_records(
        updates: &Vec<StateUpdate<T>>,
        factory: &impl ExpressionFactory<T>,
    ) -> Vec<UpdateRecord<T>> {
        updates
            .iter()
            .filter(|update| {
                if let StateUpdateType::Value(expr) = update.update() {
                    !Self::is_pending_or_error_signal(expr, factory)
                } else {
                    true
                }
            })
            .map(|update| match update.update() {
                StateUpdateType::Value(expr) => UpdateRecord(update.state_token(), expr.clone()),
                StateUpdateType::Patch(_) => todo!(),
            })
            .collect()
    }

    fn record_expression(
        signal_id: SignalId,
        expression: &T,
        sender: tokio::sync::mpsc::UnboundedSender<SignalRecorderMessage<T>>,
        factory: &impl AsyncExpressionFactory<T>,
    ) {
        if !Self::is_pending_or_error_signal(expression, factory) {
            let expression_record = SignalExpressionRecord {
                signal_id,
                expression: expression.clone(),
            };
            let _ = sender.send(SignalRecorderMessage::Expression(expression_record));
        } else {
            let _ = sender.send(SignalRecorderMessage::PendingSignal(signal_id));
        }
    }
    fn is_pending_or_error_signal(expression: &T, factory: &impl ExpressionFactory<T>) -> bool {
        let is_pending = factory
            .match_signal_term(expression)
            .map(|signal| {
                signal
                    .signals()
                    .iter()
                    .any(|signal| matches!(signal.signal_type(), SignalType::Pending))
            })
            .unwrap_or(false);

        let is_error = factory
            .match_signal_term(expression)
            .map(|signal| {
                signal
                    .signals()
                    .iter()
                    .any(|signal| matches!(signal.signal_type(), SignalType::Error))
            })
            .unwrap_or(false);
        if is_error {
            eprintln!("Received error signal during capture: {}", expression);
        }
        is_pending || is_error
    }
}

#[derive(Clone)]
struct SignalCapture {
    recorded_signals: Arc<Mutex<HashMap<SignalId, Vec<SignalCaptureRecord>>>>,
}
impl SignalCapture {
    fn from_file(capture_file: &Path) -> Result<Self, String> {
        let signal_results = serialization::from_capture_file(capture_file)?;
        Ok(Self {
            recorded_signals: Arc::new(Mutex::new(signal_results)),
        })
    }

    fn return_capture<T>(
        &self,
        signal_id: SignalId,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Option<SignalResult<T>>
    where
        T: AsyncExpression + Compile<T>,
        T::String: StringValue + Send + Sync,
    {
        match self.recorded_signals.lock() {
            Ok(captured_records) => captured_records
                .get(&signal_id)
                .map(|capture_record| capture_record.to_vec())
                .map(|record| to_signal_result(record, factory, allocator).unwrap()),

            Err(err) => {
                panic!("Unable to read poisoned mutex: {}", err);
            }
        }
    }
}
pub struct SignalPlayback {
    recorded_signal_types: std::collections::HashSet<String>,
    capture: SignalCapture,
}

impl SignalPlayback {
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

    pub fn playback_signal_handler<T>(
        &self,
        handler: impl SignalHandler<T>,
        factory: &impl AsyncExpressionFactory<T>,
        allocator: &impl AsyncHeapAllocator<T>,
    ) -> impl SignalHandler<T>
    where
        T: AsyncExpression + Compile<T>,
        T::String: Send + Sync,
    {
        let recorded_signal_types = self.recorded_signal_types.clone();
        let capture = self.capture.clone();
        let factory = factory.clone();
        let allocator = allocator.clone();
        move |signal_type: &str, signals: &[&Signal<T>], helpers: &SignalHelpers<T>| {
            if recorded_signal_types.contains(signal_type) {
                eprintln!("{} is a captured signal_type", signal_type);
                Some(
                    signals
                        .iter()
                        .map(|signal| match capture.return_capture(signal.id(), &factory, &allocator) {
                            Some(result) => {
                                eprintln!("Reloaded signal result for id {} with expression {} and with an effect: {}", signal.id(), result.0, result.1.is_some());
                                Ok(result)
                            }
                            None => {
                                eprintln!("{} was not found in the captured data", signal.id());
                                Err(format!("Failed to find signal id {} in captured data", signal.id()))
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

mod serialization {
    use std::{collections::HashMap, path::Path};

    use reflex::core::{ExpressionFactory, HeapAllocator};
    use reflex_runtime::{create_pending_expression, SignalResult};

    use super::*;

    #[derive(Serialize, Deserialize, Clone)]
    pub(super) enum SignalCaptureRecord {
        PendingSignal(SignalId),
        Expression(SerializableSignalExpressionRecord),
        SyncEffect(SerializableSyncRecord),
        AsyncEffect(SerializableAsyncRecord),
        StreamEffect(SerializableStreamRecord),
    }
    impl SignalCaptureRecord {
        fn id(&self) -> SignalId {
            match self {
                SignalCaptureRecord::PendingSignal(id) => *id,
                SignalCaptureRecord::Expression(record) => record.signal_id,
                SignalCaptureRecord::SyncEffect(record) => record.signal_id,
                SignalCaptureRecord::AsyncEffect(record) => record.signal_id,
                SignalCaptureRecord::StreamEffect(record) => record.signal_id,
            }
        }
    }
    #[derive(Serialize, Deserialize, Clone)]
    pub(super) struct SerializableSignalExpressionRecord {
        signal_id: SignalId,
        serialized_expression: String,
    }
    impl<T> From<SignalRecorderMessage<T>> for SignalCaptureRecord
    where
        T: AsyncExpression,
    {
        fn from(record: SignalRecorderMessage<T>) -> Self {
            match record {
                SignalRecorderMessage::PendingSignal(id) => Self::PendingSignal(id),
                SignalRecorderMessage::Expression(record) => record.into(),
                SignalRecorderMessage::SyncEffect(record) => record.into(),
                SignalRecorderMessage::AsyncEffect(record) => record.into(),
                SignalRecorderMessage::StreamEffect(record) => record.into(),
            }
        }
    }
    fn serialize_expression<T: Expression>(expr: &T) -> String {
        reflex_json::stringify(expr).unwrap_or_else(|err| {
            eprintln!("Can't serialize {}: {}", expr, err);
            "".to_string()
        })
    }
    impl<T> From<SignalExpressionRecord<T>> for SignalCaptureRecord
    where
        T: Expression,
    {
        fn from(record: SignalExpressionRecord<T>) -> Self {
            let serialized_expression = serialize_expression(&record.expression);
            SignalCaptureRecord::Expression(SerializableSignalExpressionRecord {
                signal_id: record.signal_id,
                serialized_expression,
            })
        }
    }
    #[derive(Serialize, Deserialize, Clone)]
    pub(super) struct SerializableSyncRecord {
        signal_id: SignalId,
        serialized_updates: Vec<(StateToken, String)>,
    }
    impl<T> From<SyncEffectRecord<T>> for SignalCaptureRecord
    where
        T: AsyncExpression,
    {
        fn from(record: SyncEffectRecord<T>) -> Self {
            let serialized_updates = record
                .updates
                .into_iter()
                .map(|update| (update.0, serialize_expression(&update.1)))
                .collect();
            SignalCaptureRecord::SyncEffect(SerializableSyncRecord {
                signal_id: record.signal_id,
                serialized_updates,
            })
        }
    }
    #[derive(Serialize, Deserialize, Clone)]
    pub(super) struct SerializableAsyncRecord {
        signal_id: SignalId,
        serialized_updates: Vec<(StateToken, String)>,
    }
    impl<T> From<AsyncEffectRecord<T>> for SignalCaptureRecord
    where
        T: AsyncExpression,
    {
        fn from(record: AsyncEffectRecord<T>) -> Self {
            let serialized_updates = record
                .updates
                .into_iter()
                .map(|update| (update.0, serialize_expression(&update.1)))
                .collect();
            SignalCaptureRecord::AsyncEffect(SerializableAsyncRecord {
                signal_id: record.signal_id,
                serialized_updates,
            })
        }
    }
    #[derive(Serialize, Deserialize, Clone)]
    pub(super) struct SerializableStreamRecord {
        signal_id: SignalId,
        serialized_updates: Vec<(StateToken, String)>,
    }
    impl<T> From<StreamEffectRecord<T>> for SignalCaptureRecord
    where
        T: AsyncExpression,
    {
        fn from(record: StreamEffectRecord<T>) -> Self {
            let serialized_updates = record
                .updates
                .into_iter()
                .map(|update| (update.0, serialize_expression(&update.1)))
                .collect();
            SignalCaptureRecord::StreamEffect(SerializableStreamRecord {
                signal_id: record.signal_id,
                serialized_updates,
            })
        }
    }
    async fn noop_future() {}

    fn to_expression<T>(
        expr: String,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> T
    where
        T: AsyncExpression,
    {
        if expr.is_empty() {
            create_pending_expression(factory, allocator)
        } else {
            reflex_json::hydrate(reflex_json::deserialize(&expr).unwrap(), factory, allocator)
                .unwrap()
        }
    }

    fn to_stream_effect<T>(
        serialized_updates: Vec<Vec<(StateToken, String)>>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> RuntimeEffect<T>
    where
        T: AsyncExpression,
    {
        let updates: Vec<Vec<StateUpdate<T>>> = serialized_updates
            .into_iter()
            .map(|serialized_update| to_state_updates(serialized_update, factory, allocator))
            .collect();

        RuntimeEffect::Stream((
            Box::pin(futures::stream::iter(updates)),
            Box::pin(noop_future()),
        ))
    }

    fn to_state_updates<T>(
        serialized: Vec<(StateToken, String)>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Vec<StateUpdate<T>>
    where
        T: AsyncExpression,
    {
        serialized
            .into_iter()
            .map(|(token, update)| {
                let expr = to_expression(update, factory, allocator);
                (token, expr)
            })
            .map(|(token, expr)| StateUpdate::value(token, expr))
            .collect()
    }

    fn to_async_effect<T>(
        serialized_updates: Vec<(StateToken, String)>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> RuntimeEffect<T>
    where
        T: AsyncExpression,
    {
        let updates: Vec<StateUpdate<T>> = to_state_updates(serialized_updates, factory, allocator);

        let effect = async move { updates };

        RuntimeEffect::Async((Box::pin(effect), Box::pin(noop_future())))
    }
    fn to_sync_effect<T>(
        serialized_updates: Vec<(StateToken, String)>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> RuntimeEffect<T>
    where
        T: AsyncExpression,
    {
        let updates: Vec<StateUpdate<T>> = to_state_updates(serialized_updates, factory, allocator);

        RuntimeEffect::Sync(updates)
    }

    pub(super) fn to_signal_result<T>(
        serialized: Vec<SignalCaptureRecord>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Result<SignalResult<T>, String>
    where
        T: AsyncExpression,
    {
        let (mut expression_records, mut effect_records): (Vec<_>, Vec<_>) =
            serialized.into_iter().partition(|record| {
                matches!(
                    record,
                    SignalCaptureRecord::Expression(_) | SignalCaptureRecord::PendingSignal(_)
                )
            });
        assert_eq!(expression_records.len(), 1);
        let expression_record = expression_records.pop().unwrap();
        let expression = match expression_record {
            SignalCaptureRecord::Expression(expr) => {
                to_expression(expr.serialized_expression, factory, allocator)
            }
            SignalCaptureRecord::PendingSignal(_) => create_pending_expression(factory, allocator),
            _ => unreachable!(),
        };
        let effect = if effect_records.is_empty() {
            None
        } else {
            let first = effect_records.pop().unwrap();
            match first {
                SignalCaptureRecord::SyncEffect(record) => {
                    if !effect_records.is_empty() {
                        return Err(String::from("Expected single effect from a sync effect"));
                    }
                    Some(to_sync_effect(
                        record.serialized_updates,
                        factory,
                        allocator,
                    ))
                }
                SignalCaptureRecord::AsyncEffect(record) => {
                    if !effect_records.is_empty() {
                        return Err(String::from("Expected single effect from an async effect"));
                    }
                    Some(to_async_effect(
                        record.serialized_updates,
                        factory,
                        allocator,
                    ))
                }
                SignalCaptureRecord::StreamEffect(record) => {
                    effect_records.push(SignalCaptureRecord::StreamEffect(record));
                    let stream_updates = effect_records
                        .into_iter()
                        .map(|record| match record {
                            SignalCaptureRecord::StreamEffect(record) => {
                                Ok(record.serialized_updates)
                            }
                            _ => Err(String::from(
                                "The same signal returned different types of effects",
                            )),
                        })
                        .collect::<Result<Vec<Vec<_>>, String>>()?;
                    Some(to_stream_effect(stream_updates, factory, allocator))
                }
                _ => unreachable!(),
            }
        };

        Ok((expression, effect))
    }

    pub(super) fn from_capture_file(
        capture_file: &Path,
    ) -> Result<HashMap<SignalId, Vec<SignalCaptureRecord>>, String> {
        let mut deserialized = HashMap::new();
        let capture_file = std::fs::File::open(capture_file)
            .map_err(|err| format!("Failed to open {}: {}", capture_file.display(), err))?;
        let records: Result<Vec<SignalCaptureRecord>, serde_json::Error> =
            serde_json::Deserializer::from_reader(capture_file)
                .into_iter::<SignalCaptureRecord>()
                .collect();
        let records = records.map_err(|err| format!("Unable to deserialize record: {}", err))?;
        for record in records {
            deserialized
                .entry(record.id())
                .or_insert(vec![])
                .push(record);
        }
        Ok(deserialized)
    }
}
