// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{hash::Hash, iter::once, sync::Arc, time::Instant};

use metrics::{describe_histogram, histogram, Unit};
use reflex::{
    compiler::{
        create_main_function, Compile, Compiler, CompilerMode, CompilerOptions, Instruction,
        InstructionPointer, Program,
    },
    core::{
        Applicable, DependencyList, EvaluationResult, Expression, ExpressionFactory, HeapAllocator,
        Reducible, Rewritable, SignalType, StateCache,
    },
    hash::{hash_object, HashId},
    interpreter::{execute, DefaultInterpreterCache, InterpreterOptions, MutableInterpreterCache},
    lang::ValueTerm,
};
use reflex_dispatcher::{
    find_latest_typed_message, Action, HandlerContext, InboundAction, MessageData, MessageOffset,
    OutboundAction, StateOperation, Worker, WorkerContext, WorkerFactory, WorkerMessageQueue,
    WorkerTransition,
};

use crate::{
    action::evaluate::{EvaluateResultAction, EvaluateStartAction},
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, QueryEvaluationMode,
    QueryInvalidationStrategy,
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
    + InboundAction<EvaluateStartAction<T>>
    + InboundAction<EvaluateResultAction<T>>
    + OutboundAction<EvaluateStartAction<T>>
    + OutboundAction<EvaluateResultAction<T>>
{
}
impl<T: Expression, TAction> BytecodeWorkerAction<T> for TAction where
    Self: Action
        + InboundAction<EvaluateStartAction<T>>
        + InboundAction<EvaluateResultAction<T>>
        + OutboundAction<EvaluateStartAction<T>>
        + OutboundAction<EvaluateResultAction<T>>
{
}

pub(crate) struct BytecodeWorkerFactory<T, TFactory, TAllocator>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
{
    pub cache_key: HashId,
    pub query: T,
    pub evaluation_mode: QueryEvaluationMode,
    pub compiler_options: CompilerOptions,
    pub interpreter_options: InterpreterOptions,
    pub graph_root: Arc<(Program, InstructionPointer)>,
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
            &self.query,
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
            cache_key: self.cache_key,
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
    pub cache_key: HashId,
    pub graph_root: (Program, InstructionPointer),
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
        queue: WorkerMessageQueue<TAction>,
        context: &mut WorkerContext,
    ) -> WorkerTransition<TAction> {
        sort_worker_message_queue(queue).into_iter().fold(
            WorkerTransition::new(None),
            |transition, (action, metadata)| {
                transition.append(match action.try_into_type() {
                    Ok(action) => self.handle_evaluate_start(action, metadata, context),
                    Err(action) => match action.try_into_type() {
                        Ok(action) => self.handle_evaluate_result(action, metadata, context),
                        Err(_action) => WorkerTransition::new(None),
                    },
                })
            },
        )
    }
}
impl<T, TFactory, TAllocator> BytecodeWorker<T, TFactory, TAllocator>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
{
    fn handle_evaluate_start<TAction>(
        &mut self,
        action: EvaluateStartAction<T>,
        metadata: MessageData,
        context: &mut impl HandlerContext,
    ) -> WorkerTransition<TAction>
    where
        TAction: Action + OutboundAction<EvaluateResultAction<T>>,
    {
        let EvaluateStartAction {
            cache_key,
            query,
            evaluation_mode: _,
            invalidation_strategy: _,
            state_index,
            state_updates,
        } = action;
        self.state.state_index = state_index;
        self.state.state_values.extend(
            state_updates
                .iter()
                .map(|(state_token, value)| (*state_token, value.clone())),
        );
        let (program, entry_point) = &self.graph_root;
        let start_time = Instant::now();
        let result = execute(
            self.cache_key,
            program,
            *entry_point,
            state_index.map(|value| value.into()).unwrap_or(0),
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
                self.state.cache.extend(cache_entries);
                result
            }
            Err(message) => EvaluationResult::new(
                create_error_expression(message, &self.factory, &self.allocator),
                DependencyList::empty(),
            ),
        };
        let emit_result_operation = context.caller_pid().map(|parent_pid| {
            StateOperation::Send(
                parent_pid,
                EvaluateResultAction {
                    cache_key,
                    query: query.clone(),
                    state_index,
                    result: result.clone(),
                }
                .into(),
            )
        });
        let internal_gc_operation = StateOperation::Send(
            context.pid(),
            EvaluateResultAction {
                cache_key,
                query,
                state_index,
                result,
            }
            .into(),
        );
        WorkerTransition::new(
            emit_result_operation
                .into_iter()
                .chain(once(internal_gc_operation))
                .map(|operation| (metadata.offset, operation)),
        )
    }
    fn handle_evaluate_result<TAction>(
        &mut self,
        action: EvaluateResultAction<T>,
        _metadata: MessageData,
        _context: &mut impl HandlerContext,
    ) -> WorkerTransition<TAction>
    where
        TAction: Action,
    {
        let EvaluateResultAction {
            cache_key: _,
            query: _,
            state_index,
            result,
        } = action;
        let current_state_index = self.state.state_index;
        if state_index < current_state_index {
            return WorkerTransition::new(None);
        }
        let start_time = Instant::now();
        self.state.cache.gc(once(self.cache_key));
        if result.dependencies().len() < self.state.state_values.len() {
            self.state.state_values.gc(result.dependencies());
        }
        let elapsed_time = start_time.elapsed();
        histogram!(
            self.metric_names.query_worker_gc_duration,
            elapsed_time.as_secs_f64()
        );
        // TODO: Garbage-collect state values
        WorkerTransition::new(None)
    }
}

fn sort_worker_message_queue<T, TAction>(
    inbox: WorkerMessageQueue<TAction>,
) -> WorkerMessageQueue<TAction>
where
    T: Expression,
    TAction: Action
        + InboundAction<EvaluateStartAction<T>>
        + InboundAction<EvaluateResultAction<T>>
        + OutboundAction<EvaluateStartAction<T>>
        + OutboundAction<EvaluateResultAction<T>>,
{
    inbox.into_iter().fold(
        WorkerMessageQueue::default(),
        |queue, (action, metadata)| match action.try_into_type() {
            Ok(action) => enqueue_evaluate_start(queue, action, metadata),
            Err(action) => match action.try_into_type() {
                Ok(action) => enqueue_evaluate_result(queue, action, metadata),
                Err(_) => queue,
            },
        },
    )
}

fn enqueue_evaluate_start<T, TAction>(
    queue: WorkerMessageQueue<TAction>,
    action: EvaluateStartAction<T>,
    metadata: MessageData,
) -> WorkerMessageQueue<TAction>
where
    T: Expression,
    TAction:
        Action + InboundAction<EvaluateStartAction<T>> + OutboundAction<EvaluateStartAction<T>>,
{
    match &action.invalidation_strategy {
        QueryInvalidationStrategy::Exact => {
            enqueue_evaluate_start_unbatched(queue, action, metadata)
        }
        QueryInvalidationStrategy::CombineUpdateBatches => {
            enqueue_evaluate_start_batched(queue, action, metadata)
        }
    }
}

fn enqueue_evaluate_start_unbatched<T, TAction>(
    queue: WorkerMessageQueue<TAction>,
    action: EvaluateStartAction<T>,
    metadata: MessageData,
) -> WorkerMessageQueue<TAction>
where
    T: Expression,
    TAction:
        Action + InboundAction<EvaluateStartAction<T>> + OutboundAction<EvaluateStartAction<T>>,
{
    let mut queue = queue;
    let (latest_start_message, trailing_actions) =
        find_latest_typed_message::<EvaluateStartAction<T>, TAction>(&mut queue);
    if let Some((latest_start_action, _previous_metadata)) = latest_start_message {
        queue.push_back((latest_start_action.into(), metadata));
        queue.push_back((action.into(), metadata));
        queue.extend(trailing_actions);
    } else {
        queue.push_back((action.into(), metadata));
        queue.extend(trailing_actions);
    }
    queue
}

fn enqueue_evaluate_start_batched<T, TAction>(
    queue: WorkerMessageQueue<TAction>,
    action: EvaluateStartAction<T>,
    metadata: MessageData,
) -> WorkerMessageQueue<TAction>
where
    T: Expression,
    TAction:
        Action + InboundAction<EvaluateStartAction<T>> + OutboundAction<EvaluateStartAction<T>>,
{
    let mut queue = queue;
    let (latest_start_message, trailing_actions) =
        find_latest_typed_message::<EvaluateStartAction<T>, TAction>(&mut queue);
    if let Some((latest_start_action, _previous_metadata)) = latest_start_message {
        let mut combined_state_updates = latest_start_action.state_updates;
        combined_state_updates.extend(
            action
                .state_updates
                .iter()
                .map(|(state_token, value)| (*state_token, value.clone())),
        );
        queue.push_back((
            EvaluateStartAction {
                cache_key: action.cache_key,
                query: action.query,
                evaluation_mode: action.evaluation_mode,
                invalidation_strategy: action.invalidation_strategy,
                state_index: action.state_index,
                state_updates: combined_state_updates,
            }
            .into(),
            metadata.clone(),
        ));
        queue.extend(trailing_actions);
    } else {
        queue.push_back((action.into(), metadata));
        queue.extend(trailing_actions);
    }
    queue
}

fn enqueue_evaluate_result<T, TAction>(
    queue: WorkerMessageQueue<TAction>,
    action: EvaluateResultAction<T>,
    metadata: MessageData,
) -> WorkerMessageQueue<TAction>
where
    T: Expression,
    TAction:
        Action + InboundAction<EvaluateResultAction<T>> + OutboundAction<EvaluateResultAction<T>>,
{
    let mut queue = queue;
    let (_, trailing_actions) =
        find_latest_typed_message::<EvaluateResultAction<T>, TAction>(&mut queue);
    queue.extend(trailing_actions);
    queue.push_back((action.into(), metadata));
    queue
}

fn compile_graphql_query<
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

fn create_error_expression<T: Expression>(
    message: String,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_signal_term(allocator.create_signal_list(once(allocator.create_signal(
        SignalType::Error,
        allocator.create_unit_list(factory.create_value_term(ValueTerm::String(message.into()))),
    ))))
}
