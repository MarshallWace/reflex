// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{hash_map::Entry, HashMap},
    hash::Hash,
    iter::once,
    marker::PhantomData,
    sync::{Arc, Once},
    time::Instant,
};

use metrics::{describe_histogram, histogram, Unit};
use reflex::{
    compiler::{
        create_main_function, Compile, Compiler, CompilerMode, CompilerOptions, Instruction,
        InstructionPointer, Program,
    },
    core::{
        Applicable, Expression, ExpressionFactory, HeapAllocator, Reducible, Rewritable, SignalType,
    },
    hash::{hash_object, HashId},
    interpreter::InterpreterOptions,
};
use reflex_dispatcher::{
    Action, Actor, HandlerContext, InboundAction, MessageData, OutboundAction, ProcessId,
    StateOperation, StateTransition, WorkerFactory,
};

use crate::{
    action::evaluate::{EvaluateResultAction, EvaluateStartAction, EvaluateStopAction},
    worker::bytecode_worker::{BytecodeWorker, BytecodeWorkerAction},
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, QueryEvaluationMode,
};

pub const METRIC_QUERY_WORKER_COMPILE_DURATION: &'static str = "query_worker_compile_duration";
pub const METRIC_QUERY_WORKER_EVALUATE_DURATION: &'static str = "query_worker_evaluate_duration";
pub const METRIC_QUERY_WORKER_GC_DURATION: &'static str = "query_worker_gc_duration";

static INIT_METRICS: Once = Once::new();

fn init_metrics() {
    INIT_METRICS.call_once(|| {
        describe_histogram!(
            METRIC_QUERY_WORKER_COMPILE_DURATION,
            Unit::Seconds,
            "Worker query compilation duration (seconds)"
        );
        describe_histogram!(
            METRIC_QUERY_WORKER_EVALUATE_DURATION,
            Unit::Seconds,
            "Worker query evaluation duration (seconds)"
        );
        describe_histogram!(
            METRIC_QUERY_WORKER_GC_DURATION,
            Unit::Seconds,
            "Worker garbage collection duration (seconds)"
        );
    });
}

pub trait BytecodeInterpreterAction<T: Expression>:
    Action
    + BytecodeWorkerAction<T>
    + InboundAction<EvaluateStartAction<T>>
    + InboundAction<EvaluateResultAction<T>>
    + InboundAction<EvaluateStopAction<T>>
    + OutboundAction<EvaluateStartAction<T>>
    + OutboundAction<EvaluateResultAction<T>>
{
}
impl<T: Expression, TAction> BytecodeInterpreterAction<T> for TAction where
    Self: Action
        + BytecodeWorkerAction<T>
        + InboundAction<EvaluateStartAction<T>>
        + InboundAction<EvaluateResultAction<T>>
        + InboundAction<EvaluateStopAction<T>>
        + OutboundAction<EvaluateStartAction<T>>
        + OutboundAction<EvaluateResultAction<T>>
{
}

pub struct BytecodeInterpreter<T, TFactory, TAllocator>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
{
    graph_root: Arc<(Program, InstructionPointer)>,
    compiler_options: CompilerOptions,
    interpreter_options: InterpreterOptions,
    factory: TFactory,
    allocator: TAllocator,
    state: BytecodeInterpreterState,
    _expression: PhantomData<T>,
}
impl<T, TFactory, TAllocator> BytecodeInterpreter<T, TFactory, TAllocator>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
{
    pub fn new(
        graph_root: (Program, InstructionPointer),
        compiler_options: CompilerOptions,
        interpreter_options: InterpreterOptions,
        factory: TFactory,
        allocator: TAllocator,
    ) -> Self {
        init_metrics();
        Self {
            graph_root: Arc::new(graph_root),
            compiler_options,
            interpreter_options,
            factory,
            allocator,
            state: Default::default(),
            _expression: Default::default(),
        }
    }
}

#[derive(Default)]
struct BytecodeInterpreterState {
    workers: HashMap<HashId, ProcessId>,
}

impl<T, TFactory, TAllocator, TAction> Actor<TAction>
    for BytecodeInterpreter<T, TFactory, TAllocator>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TAction: BytecodeInterpreterAction<T> + Send + 'static,
{
    fn handle(
        &mut self,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> StateTransition<TAction> {
        if let Some(action) = action.match_type() {
            self.handle_evaluate_start(action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_evaluate_stop(action, metadata, context)
        } else {
            None
        }
        .unwrap_or_default()
    }
}
impl<T, TFactory, TAllocator> BytecodeInterpreter<T, TFactory, TAllocator>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
{
    fn handle_evaluate_start<TAction>(
        &mut self,
        action: &EvaluateStartAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action
            + Send
            + 'static
            + InboundAction<EvaluateStartAction<T>>
            + InboundAction<EvaluateResultAction<T>>
            + OutboundAction<EvaluateStartAction<T>>
            + OutboundAction<EvaluateResultAction<T>>,
    {
        let EvaluateStartAction {
            cache_key,
            query,
            evaluation_mode,
            invalidation_strategy: _,
            state_index: _,
            state_updates: _,
        } = action;
        match self.state.workers.entry(*cache_key) {
            Entry::Occupied(entry) => {
                let worker_pid = *entry.get();
                Some(StateTransition::new(once(StateOperation::Send(
                    worker_pid,
                    action.clone().into(),
                ))))
            }
            Entry::Vacant(entry) => {
                let worker_pid = context.generate_pid();
                entry.insert(worker_pid);
                Some(StateTransition::new([
                    StateOperation::Spawn(
                        worker_pid,
                        WorkerFactory::new({
                            let cache_key = *cache_key;
                            let query = query.clone();
                            let evaluation_mode = *evaluation_mode;
                            let compiler_options = self.compiler_options;
                            let interpreter_options = self.interpreter_options;
                            let graph_root = self.graph_root.clone();
                            let factory = self.factory.clone();
                            let allocator = self.allocator.clone();
                            move || {
                                let start_time = Instant::now();
                                let graph_root = compile_graphql_query(
                                    &query,
                                    &query,
                                    &graph_root,
                                    evaluation_mode,
                                    &compiler_options,
                                    &factory,
                                    &allocator,
                                )
                                .unwrap_or_else(create_compiler_error_program);
                                let elapsed_time = start_time.elapsed();
                                histogram!(
                                    METRIC_QUERY_WORKER_COMPILE_DURATION,
                                    elapsed_time.as_secs_f64()
                                );
                                BytecodeWorker {
                                    cache_key,
                                    graph_root,
                                    interpreter_options,
                                    factory,
                                    allocator,
                                    state: Default::default(),
                                }
                            }
                        }),
                    ),
                    StateOperation::Send(worker_pid, action.clone().into()),
                ]))
            }
        }
    }
    fn handle_evaluate_stop<TAction>(
        &mut self,
        action: &EvaluateStopAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action,
    {
        let EvaluateStopAction {
            cache_key,
            query: _,
        } = action;
        let worker_pid = self.state.workers.remove(cache_key)?;
        Some(StateTransition::new(once(StateOperation::Kill(worker_pid))))
    }
}

pub fn compile_graphql_query<
    T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
>(
    query: &T,
    operation_id: &impl Hash,
    graph_root: &(Program, InstructionPointer),
    evaluation_mode: QueryEvaluationMode,
    compiler_options: &CompilerOptions,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<(Program, InstructionPointer), String> {
    let (graph_factory, graph_factory_address) = graph_root;
    let mut program = Compiler::new(*compiler_options, Some(graph_factory.clone())).compile(
        query,
        CompilerMode::Function,
        factory,
        allocator,
    )?;
    match evaluation_mode {
        QueryEvaluationMode::Query => {
            let entry_point = InstructionPointer::new(program.len());
            let query_factory_address = InstructionPointer::new(graph_factory.len());
            let graph_factory_address = *graph_factory_address;
            let graph_factory_hash = get_compiled_function_hash(graph_factory_address, &program)?;
            let query_factory_hash = get_compiled_function_hash(query_factory_address, &program)?;
            program.extend(create_query_entry_point_function(
                (graph_factory_address, graph_factory_hash),
                (query_factory_address, query_factory_hash),
                operation_id,
            ));
            Ok((program, entry_point))
        }
        QueryEvaluationMode::Standalone => {
            Ok((program, InstructionPointer::new(graph_factory.len())))
        }
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
        Instruction::PushHash {
            value: hash_object(operation_id),
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

fn create_compiler_error_program(message: String) -> (Program, InstructionPointer) {
    let program = create_main_function([
        Instruction::PushString { value: message },
        Instruction::PushSignal {
            signal_type: SignalType::Error,
            num_args: 1,
        },
        Instruction::Return,
    ]);
    let entry_point = InstructionPointer::default();
    (program, entry_point)
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
