// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    cmp::Reverse,
    collections::HashMap,
    convert::TryFrom,
    marker::PhantomData,
    path::Path,
    str::FromStr,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc, Mutex, MutexGuard,
    },
};

use anyhow::Result;
use reflex::{
    compiler::Compile,
    core::{Applicable, Expression, Reducible, Rewritable, Uuid},
    stdlib::Stdlib,
};
use reflex_graphql::stdlib::Stdlib as GraphQlStdlib;
use reflex_js::stdlib::Stdlib as JsStdlib;
use reflex_runtime::{AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, SignalHandler};
use tracing::{field::Field, Event, Subscriber};
use tracing_subscriber::{field::Visit, layer::Context, util::SubscriberInitExt, Layer};

use super::execute_query::{self, ExecuteQueryCliOptions};

#[derive(Clone)]
pub struct RuntimeTraceStatistics<T: Expression> {
    _type: PhantomData<T>,
    execution_count: Arc<AtomicUsize>,
    global_instruction_count: Arc<Mutex<HashMap<String, u64>>>,
    apply_target_count: Arc<Mutex<HashMap<String, u64>>>,
    evaluate_target_count: Arc<Mutex<HashMap<String, u64>>>,
    builtin_target_count: Arc<Mutex<HashMap<String, u64>>>,
    factory_create_count: Arc<Mutex<HashMap<String, u64>>>,
    allocator_create_count: Arc<Mutex<HashMap<String, u64>>>,
    allocator_list_id_count: Arc<Mutex<HashMap<String, u64>>>,
}
impl<T: Expression> RuntimeTraceStatistics<T> {
    fn new() -> Self {
        Self {
            _type: PhantomData,
            execution_count: Arc::new(AtomicUsize::new(0)),
            global_instruction_count: Arc::new(Mutex::new(HashMap::new())),
            apply_target_count: Arc::new(Mutex::new(HashMap::new())),
            evaluate_target_count: Arc::new(Mutex::new(HashMap::new())),
            builtin_target_count: Arc::new(Mutex::new(HashMap::new())),
            factory_create_count: Arc::new(Mutex::new(HashMap::new())),
            allocator_create_count: Arc::new(Mutex::new(HashMap::new())),
            allocator_list_id_count: Arc::new(Mutex::new(HashMap::new())),
        }
    }
}
impl<T: Expression> std::fmt::Display for RuntimeTraceStatistics<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "Total execution passes: {}\n",
            self.execution_count.load(Ordering::Acquire)
        )?;
        write_map_summary(
            f,
            "Global Instruction counts",
            self.global_instruction_count
                .lock()
                .unwrap()
                .iter()
                .map(|(key, value)| (key.clone(), *value)),
        )?;
        write_map_summary(
            f,
            "Evaluate target counts",
            self.evaluate_target_count
                .lock()
                .unwrap()
                .iter()
                .map(|(key, value)| (key.clone(), *value)),
        )?;
        write_map_summary(
            f,
            "Apply target counts",
            self.apply_target_count
                .lock()
                .unwrap()
                .iter()
                .map(|(key, value)| (key.clone(), *value)),
        )?;
        write_map_summary(
            f,
            "Builtin function target counts",
            self.builtin_target_count
                .lock()
                .unwrap()
                .iter()
                .map(|(key, value)| {
                    (
                        Uuid::from_str(key)
                            .ok()
                            .and_then(|uuid| {
                                T::Builtin::try_from(uuid)
                                    .ok()
                                    .map(|builtin| format!("{}", builtin))
                            })
                            .unwrap_or_else(|| key.clone()),
                        *value,
                    )
                }),
        )?;
        write_map_summary(
            f,
            "Factory create counts",
            self.factory_create_count
                .lock()
                .unwrap()
                .iter()
                .map(|(key, value)| (key.clone(), *value)),
        )?;
        write_map_summary(
            f,
            "Allocator creation counts",
            self.allocator_create_count
                .lock()
                .unwrap()
                .iter()
                .map(|(key, value)| (key.clone(), *value)),
        )?;
        write_list_summary(
            f,
            "Allocator list id counts",
            self.allocator_list_id_count
                .lock()
                .unwrap()
                .iter()
                .map(|(key, value)| (key.clone(), *value)),
        )?;
        Ok(())
    }
}

fn write_map_summary(
    f: &mut std::fmt::Formatter<'_>,
    title: &str,
    entries: impl IntoIterator<Item = (String, u64)>,
) -> std::fmt::Result {
    let mut entries = entries.into_iter().collect::<Vec<_>>();
    entries.sort_by_key(|(_, value)| Reverse(*value));
    writeln!(
        f,
        "{}:\n\n{}\n",
        title,
        entries
            .into_iter()
            .map(|(key, value)| format!("{}: {}", key, value))
            .collect::<Vec<_>>()
            .join("\n")
    )
}

fn write_list_summary(
    f: &mut std::fmt::Formatter<'_>,
    title: &str,
    entries: impl IntoIterator<Item = (String, u64)>,
) -> std::fmt::Result {
    let mut entries = entries.into_iter().collect::<Vec<_>>();
    entries.sort_by_key(|(_, value)| Reverse(*value));
    writeln!(
        f,
        "{}:\n\nTotal entries: {}\nUnique entries: {}\n\nTop entries:\n\n{}\n",
        title,
        entries.iter().fold(0, |result, (_, value)| result + *value),
        entries.len(),
        entries
            .into_iter()
            .take(10)
            .map(|(key, value)| format!("{}: {}", key, value))
            .collect::<Vec<_>>()
            .join("\n")
    )
}

struct RuntimeStatisticsVisitor<'a> {
    execution_count: Arc<AtomicUsize>,
    global_instruction_count: MutexGuard<'a, HashMap<String, u64>>,
    apply_target_count: MutexGuard<'a, HashMap<String, u64>>,
    evaluate_target_count: MutexGuard<'a, HashMap<String, u64>>,
    builtin_target_count: MutexGuard<'a, HashMap<String, u64>>,
    factory_create_count: MutexGuard<'a, HashMap<String, u64>>,
    allocator_create_count: MutexGuard<'a, HashMap<String, u64>>,
    allocator_list_id_count: MutexGuard<'a, HashMap<String, u64>>,
}

struct RuntimeStatisticsLayer<T: Expression> {
    entries: RuntimeTraceStatistics<T>,
}
impl<T: Expression> RuntimeStatisticsLayer<T> {
    fn new(entries: RuntimeTraceStatistics<T>) -> Self {
        Self { entries }
    }
    fn visitor(&self) -> RuntimeStatisticsVisitor<'_> {
        RuntimeStatisticsVisitor {
            execution_count: self.entries.execution_count.clone(),
            global_instruction_count: self.entries.global_instruction_count.lock().unwrap(),
            apply_target_count: self.entries.apply_target_count.lock().unwrap(),
            evaluate_target_count: self.entries.evaluate_target_count.lock().unwrap(),
            builtin_target_count: self.entries.builtin_target_count.lock().unwrap(),
            factory_create_count: self.entries.factory_create_count.lock().unwrap(),
            allocator_create_count: self.entries.allocator_create_count.lock().unwrap(),
            allocator_list_id_count: self.entries.allocator_list_id_count.lock().unwrap(),
        }
    }
}
impl<'a> Visit for RuntimeStatisticsVisitor<'a> {
    fn record_debug(&mut self, field: &Field, value: &dyn std::fmt::Debug) {
        if field.name() == "message" && format!("{:?}", value) == "Starting execution" {
            self.execution_count.fetch_add(1, Ordering::Release);
        }
        let entry = if field.name() == "instruction" {
            Some((&mut self.global_instruction_count, value))
        } else if field.name() == "apply_metadata" {
            Some((&mut self.apply_target_count, value))
        } else if field.name() == "evaluate_metadata" {
            Some((&mut self.evaluate_target_count, value))
        } else if field.name() == "builtin_target" {
            Some((&mut self.builtin_target_count, value))
        } else if field.name() == "factory_create" {
            Some((&mut self.factory_create_count, value))
        } else if field.name() == "alloc" {
            Some((&mut self.allocator_create_count, value))
        } else if field.name() == "list_id" {
            Some((&mut self.allocator_list_id_count, value))
        } else {
            None
        };
        if let Some((counter, key)) = entry {
            *counter.entry(format!("{:?}", key)).or_insert(0) += 1;
        }
    }
}
impl<T: Expression + 'static, S: Subscriber> Layer<S> for RuntimeStatisticsLayer<T> {
    fn on_event(&self, event: &Event<'_>, _ctx: Context<'_, S>) {
        if event.metadata().level() == &tracing::Level::TRACE {
            event.record(&mut self.visitor());
        }
    }
}

pub async fn cli<T, TLoader>(
    options: ExecuteQueryCliOptions,
    env: Option<impl IntoIterator<Item = (String, String)>>,
    signal_handler: impl SignalHandler<T>,
    module_loader: Option<TLoader>,
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
) -> Result<RuntimeTraceStatistics<T>>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send + Sync,
    T::Builtin: From<Stdlib> + From<JsStdlib> + From<GraphQlStdlib>,
    TLoader: Fn(&str, &Path) -> Option<Result<T, String>> + 'static,
{
    let subscriber = tracing_subscriber::fmt()
        .with_max_level(tracing::Level::TRACE)
        .with_writer(std::io::sink)
        .finish();
    let metrics = RuntimeTraceStatistics::new();
    let layer = RuntimeStatisticsLayer::new(metrics.clone());
    layer.with_subscriber(subscriber).try_init().unwrap();
    let _ = execute_query::cli(
        options,
        env,
        signal_handler,
        module_loader,
        factory,
        allocator,
    )
    .await?;
    Ok(metrics)
}
