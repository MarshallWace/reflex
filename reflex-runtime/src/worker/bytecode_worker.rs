// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::{hash::Hash, iter::once, sync::Arc, time::Instant};

use metrics::{describe_histogram, histogram, Unit};
use reflex::{
    core::{
        Applicable, DependencyList, EvaluationResult, Expression, ExpressionFactory, HeapAllocator,
        InstructionPointer, Reducible, Rewritable, SignalType, StateCache,
    },
    hash::{hash_object, HashId},
};
use reflex_dispatcher::{
    Action, HandlerContext, InboundAction, MessageData, MessageOffset, OutboundAction,
    StateOperation, Worker, WorkerContext, WorkerFactory, WorkerTransition,
};
use reflex_interpreter::{
    compiler::{
        create_main_function, Compile, CompiledProgram, Compiler, CompilerMode, CompilerOptions,
        Instruction, Program,
    },
    execute, DefaultInterpreterCache, InterpreterOptions, MutableInterpreterCache,
};

use crate::{
    action::bytecode_interpreter::{
        BytecodeInterpreterEvaluateAction, BytecodeInterpreterGcAction,
        BytecodeInterpreterResultAction,
    },
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, QueryEvaluationMode,
};

#[derive(Clone, Copy, Debug)]
pub struct BytecodeWorkerMetricNames {
    pub query_worker_compile_duration: &'static str,
    pub query_worker_evaluate_duration: &'static str,
    pub query_worker_gc_duration: &'static str,
}
impl BytecodeWorkerMetricNames {
    pub(crate) fn init(self) -> Self {
        describe_histogram!(
            self.query_worker_compile_duration,
            Unit::Seconds,
            "Worker query compilation duration (seconds)"
        );
        describe_histogram!(
            self.query_worker_evaluate_duration,
            Unit::Seconds,
            "Worker query evaluation duration (seconds)"
        );
        describe_histogram!(
            self.query_worker_gc_duration,
            Unit::Seconds,
            "Worker garbage collection duration (seconds)"
        );
        self
    }
}
impl Default for BytecodeWorkerMetricNames {
    fn default() -> Self {
        Self {
            query_worker_compile_duration: "query_worker_compile_duration",
            query_worker_evaluate_duration: "query_worker_evaluate_duration",
            query_worker_gc_duration: "query_worker_gc_duration",
        }
    }
}

pub trait BytecodeWorkerAction<T: Expression>:
    Action
    + InboundAction<BytecodeInterpreterEvaluateAction<T>>
    + InboundAction<BytecodeInterpreterGcAction>
    + OutboundAction<BytecodeInterpreterResultAction<T>>
{
}
impl<T: Expression, TAction> BytecodeWorkerAction<T> for TAction where
    Self: Action
        + InboundAction<BytecodeInterpreterEvaluateAction<T>>
        + InboundAction<BytecodeInterpreterGcAction>
        + OutboundAction<BytecodeInterpreterResultAction<T>>
{
}

pub(crate) struct BytecodeWorkerFactory<T, TFactory, TAllocator>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
{
    pub cache_id: HashId,
    pub query: T,
    pub evaluation_mode: QueryEvaluationMode,
    pub compiler_options: CompilerOptions,
    pub interpreter_options: InterpreterOptions,
    pub graph_root: Arc<(CompiledProgram, InstructionPointer)>,
    pub factory: TFactory,
    pub allocator: TAllocator,
    pub metric_names: BytecodeWorkerMetricNames,
}
impl<T, TFactory, TAllocator, TAction> WorkerFactory<TAction>
    for BytecodeWorkerFactory<T, TFactory, TAllocator>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TAction: BytecodeWorkerAction<T> + Send + 'static,
{
    type Worker = BytecodeWorker<T, TFactory, TAllocator>;
    fn create(&self) -> Self::Worker {
        let start_time = Instant::now();
        let graph_root = compile_graphql_query(
            &self.query,
            &self.query.id(),
            &self.graph_root,
            self.evaluation_mode,
            &self.compiler_options,
            &self.factory,
            &self.allocator,
        )
        .unwrap_or_else(create_compiler_error_program);
        let elapsed_time = start_time.elapsed();
        histogram!(
            self.metric_names.query_worker_compile_duration,
            elapsed_time.as_secs_f64()
        );
        BytecodeWorker {
            cache_id: self.cache_id,
            graph_root: graph_root,
            interpreter_options: self.interpreter_options,
            factory: self.factory.clone(),
            allocator: self.allocator.clone(),
            metric_names: self.metric_names,
            state: Default::default(),
        }
    }
}

pub(crate) struct BytecodeWorker<T, TFactory, TAllocator>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
{
    pub cache_id: HashId,
    pub graph_root: (CompiledProgram, InstructionPointer),
    pub interpreter_options: InterpreterOptions,
    pub factory: TFactory,
    pub allocator: TAllocator,
    pub metric_names: BytecodeWorkerMetricNames,
    pub state: BytecodeWorkerState<T>,
}

pub(crate) struct BytecodeWorkerState<T>
where
    T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
{
    state_index: Option<MessageOffset>,
    state_values: StateCache<T>,
    latest_result: Option<EvaluationResult<T>>,
    cache: DefaultInterpreterCache<T>,
}
impl<T> Default for BytecodeWorkerState<T>
where
    T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
{
    fn default() -> Self {
        Self {
            state_index: Default::default(),
            state_values: Default::default(),
            latest_result: Default::default(),
            cache: Default::default(),
        }
    }
}

impl<T, TFactory, TAllocator, TAction> Worker<TAction> for BytecodeWorker<T, TFactory, TAllocator>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TAction: BytecodeWorkerAction<T>,
{
    fn handle(
        &mut self,
        action: TAction,
        metadata: &MessageData,
        context: &mut WorkerContext,
    ) -> WorkerTransition<TAction> {
        let actions = match action.try_into_type() {
            Ok(action) => self.handle_bytecode_interpreter_evaluate(action, metadata, context),
            Err(action) => match action.try_into_type() {
                Ok(action) => self.handle_bytecode_interpreter_gc(action, metadata, context),
                Err(_action) => None,
            },
        }
        .unwrap_or_default();
        WorkerTransition::new(actions)
    }
}
impl<T, TFactory, TAllocator> BytecodeWorker<T, TFactory, TAllocator>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
{
    fn handle_bytecode_interpreter_evaluate<TAction>(
        &mut self,
        action: BytecodeInterpreterEvaluateAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<WorkerTransition<TAction>>
    where
        TAction: Action + OutboundAction<BytecodeInterpreterResultAction<T>>,
    {
        let BytecodeInterpreterEvaluateAction {
            cache_id: _,
            state_index,
            state_updates,
        } = action;
        let parent_pid = context.caller_pid()?;
        self.state.state_index = state_index;
        self.state.state_values.extend(
            state_updates
                .iter()
                .map(|(state_token, value)| (*state_token, value.clone())),
        );
        let (program, entry_point) = &self.graph_root;
        let state_id = state_index.map(|value| value.into()).unwrap_or(0);
        let start_time = Instant::now();
        let result = execute(
            self.cache_id,
            program,
            *entry_point,
            state_id,
            &self.state.state_values,
            &self.factory,
            &self.allocator,
            &self.interpreter_options,
            &self.state.cache,
        );
        let elapsed_time = start_time.elapsed();
        histogram!(
            self.metric_names.query_worker_evaluate_duration,
            elapsed_time.as_secs_f64()
        );
        let result = match result {
            Ok((result, cache_entries)) => {
                self.state.latest_result = Some(result.clone());
                self.state.cache.extend(cache_entries);
                result
            }
            Err(message) => EvaluationResult::new(
                create_error_expression(message, &self.factory, &self.allocator),
                DependencyList::empty(),
            ),
        };
        Some(WorkerTransition::new(once(StateOperation::Send(
            parent_pid,
            BytecodeInterpreterResultAction {
                cache_id: self.cache_id,
                state_index,
                result: result.clone(),
            }
            .into(),
        ))))
    }
    fn handle_bytecode_interpreter_gc<TAction>(
        &mut self,
        action: BytecodeInterpreterGcAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<WorkerTransition<TAction>>
    where
        TAction: Action,
    {
        let BytecodeInterpreterGcAction {
            cache_id: _,
            state_index,
        } = action;
        let current_state_index = self.state.state_index;
        if state_index < current_state_index {
            return None;
        }
        let start_time = Instant::now();
        self.state.cache.gc(once(self.cache_id));
        let empty_dependencies = DependencyList::default();
        let retained_keys = self
            .state
            .latest_result
            .as_ref()
            .map(|result| result.dependencies())
            .unwrap_or(&empty_dependencies);
        if retained_keys.len() < self.state.state_values.len() {
            self.state.state_values.gc(retained_keys);
        }
        let elapsed_time = start_time.elapsed();
        histogram!(
            self.metric_names.query_worker_gc_duration,
            elapsed_time.as_secs_f64()
        );
        None
    }
}

fn compile_graphql_query<
    T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
>(
    query: &T,
    operation_id: &impl Hash,
    graph_root: &(CompiledProgram, InstructionPointer),
    evaluation_mode: QueryEvaluationMode,
    compiler_options: &CompilerOptions,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<(CompiledProgram, InstructionPointer), String> {
    // Here 'graph root' refers to the previously-compiled graph root factory bytecode
    // The address will be the instruction offset of a a zero-argument 'factory' function that returns the graph root
    // The incoming graphql query will be compiled into a one-argument function that takes the graph root as its argument
    // and performs a nested traversal of that object to build up the result of the query.
    let (graph_factory, graph_factory_address) = graph_root;
    let CompiledProgram {
        mut instructions,
        data_section,
    } = Compiler::new(*compiler_options, Some(graph_factory.clone())).compile(
        query,
        CompilerMode::Function,
        factory,
        allocator,
    )?;
    match evaluation_mode {
        QueryEvaluationMode::Query => {
            let entry_point = InstructionPointer::new(instructions.len());
            let query_factory_address = InstructionPointer::new(graph_factory.instructions.len());
            let graph_factory_address = *graph_factory_address;
            let graph_factory_hash =
                get_compiled_function_hash(graph_factory_address, &instructions)?;
            let query_factory_hash =
                get_compiled_function_hash(query_factory_address, &instructions)?;
            instructions.extend(create_query_entry_point_function(
                (graph_factory_address, graph_factory_hash),
                (query_factory_address, query_factory_hash),
                operation_id,
            ));
            Ok((
                CompiledProgram {
                    instructions,
                    data_section,
                },
                entry_point,
            ))
        }
        QueryEvaluationMode::Standalone => Ok((
            CompiledProgram {
                instructions,
                data_section,
            },
            InstructionPointer::new(graph_factory.instructions.len()),
        )),
    }
}

fn create_query_entry_point_function(
    graph_factory: (InstructionPointer, HashId),
    query_factory: (InstructionPointer, HashId),
    operation_id: &impl Hash,
) -> Program {
    let (graph_factory_address, graph_factory_hash) = graph_factory;
    let (query_factory_address, query_factory_hash) = query_factory;
    create_main_function([
        Instruction::PushSymbol {
            id: hash_object(operation_id),
        },
        Instruction::Call {
            target_address: graph_factory_address,
            target_hash: graph_factory_hash,
            num_args: 0,
        },
        Instruction::Apply { num_args: 1 },
        Instruction::Evaluate,
        Instruction::Call {
            target_address: query_factory_address,
            target_hash: query_factory_hash,
            num_args: 0,
        },
        Instruction::Apply { num_args: 1 },
        Instruction::Evaluate,
        Instruction::Return,
    ])
}

fn create_compiler_error_program(message: String) -> (CompiledProgram, InstructionPointer) {
    let instructions = create_main_function([
        Instruction::PushString { value: message },
        Instruction::ConstructCondition {
            signal_type: SignalType::Error,
            num_args: 1,
        },
        Instruction::Return,
    ]);
    let entry_point = InstructionPointer::default();
    (
        CompiledProgram {
            instructions,
            data_section: Default::default(),
        },
        entry_point,
    )
}

fn get_compiled_function_hash(
    address: InstructionPointer,
    program: &Program,
) -> Result<HashId, String> {
    program
        .get(address)
        .and_then(|instruction| match instruction {
            &Instruction::Function { hash, .. } => Some(hash),
            _ => None,
        })
        .ok_or_else(|| format!("Target address is not a function: {:x}", address))
}

fn create_error_expression<T: Expression>(
    message: String,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_signal_term(allocator.create_signal_list(once(allocator.create_signal(
        SignalType::Error,
        allocator.create_unit_list(factory.create_string_term(allocator.create_string(message))),
    ))))
}
