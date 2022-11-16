// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use metrics::histogram;
use reflex::{
    core::{
        Applicable, DependencyList, EvaluationResult, Expression, ExpressionFactory, HeapAllocator,
        InstructionPointer, Reducible, Rewritable, SignalType, StateCache,
    },
    hash::{hash_object, HashId},
};
use reflex_dispatcher::{
    Action, ActorInitContext, HandlerContext, MessageData, MessageOffset, NoopDisposeCallback,
    ProcessId, SchedulerCommand, SchedulerMode, SchedulerTransition, TaskFactory, TaskInbox,
};
use reflex_interpreter::{
    compiler::{
        create_main_function, Compile, CompiledProgram, Compiler, CompilerMode, CompilerOptions,
        Instruction, Program,
    },
    execute, DefaultInterpreterCache, InterpreterOptions, MutableInterpreterCache,
};
use reflex_macros::{dispatcher, Named};
use serde::{Deserialize, Serialize};
use std::{borrow::Cow, hash::Hash, iter::once, marker::PhantomData, sync::Arc, time::Instant};

use crate::{
    action::bytecode_interpreter::{
        BytecodeInterpreterEvaluateAction, BytecodeInterpreterGcAction,
        BytecodeInterpreterInitAction, BytecodeInterpreterResultAction,
    },
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator,
};
use crate::{
    action::bytecode_interpreter::{BytecodeInterpreterGcCompleteAction, BytecodeWorkerStatistics},
    QueryEvaluationMode,
};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct BytecodeWorkerMetricNames {
    pub query_worker_compile_duration: Cow<'static, str>,
    pub query_worker_evaluate_duration: Cow<'static, str>,
    pub query_worker_gc_duration: Cow<'static, str>,
}

pub trait BytecodeWorkerTask<T, TFactory, TAllocator>:
    From<BytecodeWorkerTaskFactory<T, TFactory, TAllocator>>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
{
}
impl<TSelf, T, TFactory, TAllocator> BytecodeWorkerTask<T, TFactory, TAllocator> for TSelf
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
    Self: From<BytecodeWorkerTaskFactory<T, TFactory, TAllocator>>,
{
}

#[derive(Named, Clone, Serialize, Deserialize)]
pub struct BytecodeWorkerTaskFactory<
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
> {
    pub cache_id: HashId,
    pub query: T,
    pub evaluation_mode: QueryEvaluationMode,
    pub compiler_options: CompilerOptions,
    pub interpreter_options: InterpreterOptions,
    pub graph_root: Arc<(CompiledProgram, InstructionPointer)>,
    pub metric_names: BytecodeWorkerMetricNames,
    pub caller_pid: ProcessId,
    pub _expression: PhantomData<T>,
    pub _factory: PhantomData<TFactory>,
    pub _allocator: PhantomData<TAllocator>,
}
impl<T, TFactory, TAllocator, TAction, TTask> TaskFactory<TAction, TTask>
    for BytecodeWorkerTaskFactory<T, TFactory, TAllocator>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TAction: Action + BytecodeWorkerAction<T> + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    type Actor = BytecodeWorker<T, TFactory, TAllocator>;
    fn create(self) -> Self::Actor {
        let Self {
            cache_id,
            query,
            evaluation_mode,
            compiler_options,
            interpreter_options,
            graph_root,
            metric_names,
            caller_pid,
            _expression,
            _factory,
            _allocator,
        } = self;
        let factory = TFactory::default();
        let allocator = TAllocator::default();
        BytecodeWorker {
            cache_id,
            query,
            evaluation_mode,
            compiler_options,
            interpreter_options,
            graph_root,
            factory,
            allocator,
            metric_names,
            caller_pid,
        }
    }
}

#[derive(Named, Clone)]
pub struct BytecodeWorker<T, TFactory, TAllocator>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
{
    cache_id: HashId,
    query: T,
    evaluation_mode: QueryEvaluationMode,
    compiler_options: CompilerOptions,
    interpreter_options: InterpreterOptions,
    graph_root: Arc<(CompiledProgram, InstructionPointer)>,
    factory: TFactory,
    allocator: TAllocator,
    metric_names: BytecodeWorkerMetricNames,
    caller_pid: ProcessId,
}

pub enum BytecodeWorkerState<T: Expression> {
    Uninitialized,
    Initialized(BytecodeWorkerInitializedState<T>),
}
impl<T: Expression> Default for BytecodeWorkerState<T> {
    fn default() -> Self {
        Self::Uninitialized
    }
}
pub struct BytecodeWorkerInitializedState<T: Expression> {
    graph_root: (CompiledProgram, InstructionPointer),
    state_index: Option<MessageOffset>,
    state_values: StateCache<T>,
    latest_result: Option<EvaluationResult<T>>,
    cache: DefaultInterpreterCache<T>,
}
impl<T: Expression> BytecodeWorkerInitializedState<T> {
    fn new(graph_root: (CompiledProgram, InstructionPointer)) -> Self {
        Self {
            graph_root,
            state_index: Default::default(),
            state_values: Default::default(),
            latest_result: Default::default(),
            cache: Default::default(),
        }
    }
}
impl<T: Expression> BytecodeWorkerInitializedState<T> {
    fn get_statistics(&self) -> BytecodeWorkerStatistics {
        BytecodeWorkerStatistics {
            state_dependency_count: self
                .latest_result
                .as_ref()
                .map(|result| result.dependencies().len())
                .unwrap_or(0),
            evaluation_cache_entry_count: self.cache.len(),
            evaluation_cache_deep_size: self.cache.size(),
        }
    }
}

dispatcher!({
    pub enum BytecodeWorkerAction<T: Expression> {
        Inbox(BytecodeInterpreterInitAction),
        Inbox(BytecodeInterpreterEvaluateAction<T>),
        Inbox(BytecodeInterpreterGcAction),

        Outbox(BytecodeInterpreterResultAction<T>),
        Outbox(BytecodeInterpreterGcCompleteAction),
    }

    impl<T, TFactory, TAllocator, TAction, TTask> Dispatcher<TAction, TTask>
        for BytecodeWorker<T, TFactory, TAllocator>
    where
        T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
        TFactory: ExpressionFactory<T>,
        TAllocator: HeapAllocator<T>,
        TAction: Action,
        TTask: TaskFactory<TAction, TTask>,
    {
        type State = BytecodeWorkerState<T>;
        type Events<TInbox: TaskInbox<TAction>> = TInbox;
        type Dispose = NoopDisposeCallback;

        fn init<TInbox: TaskInbox<TAction>>(
            &self,
            inbox: TInbox,
            _context: &impl ActorInitContext,
        ) -> (Self::State, Self::Events<TInbox>, Self::Dispose) {
            (Default::default(), inbox, NoopDisposeCallback)
        }

        fn accept(&self, _action: &BytecodeInterpreterInitAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &BytecodeInterpreterInitAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &BytecodeInterpreterInitAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_bytecode_interpreter_init(state, action, metadata, context)
        }

        fn accept(&self, _action: &BytecodeInterpreterEvaluateAction<T>) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &BytecodeInterpreterEvaluateAction<T>,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &BytecodeInterpreterEvaluateAction<T>,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_bytecode_interpreter_evaluate(state, action, metadata, context)
        }

        fn accept(&self, _action: &BytecodeInterpreterGcAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &BytecodeInterpreterGcAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &BytecodeInterpreterGcAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_bytecode_interpreter_gc(state, action, metadata, context)
        }
    }
});

impl<T, TFactory, TAllocator> BytecodeWorker<T, TFactory, TAllocator>
where
    T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
{
    fn handle_bytecode_interpreter_init<TAction, TTask>(
        &self,
        state: &mut BytecodeWorkerState<T>,
        action: &BytecodeInterpreterInitAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action
            + From<BytecodeInterpreterResultAction<T>>
            + From<BytecodeInterpreterGcCompleteAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let BytecodeInterpreterInitAction { cache_id: _ } = action;
        match state {
            BytecodeWorkerState::Uninitialized => {
                let compiler_start_time = Instant::now();
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
                let elapsed_time = compiler_start_time.elapsed();
                {
                    match &self.metric_names.query_worker_compile_duration {
                        Cow::Borrowed(metric_name) => {
                            histogram!(*metric_name, elapsed_time.as_secs_f64())
                        }
                        Cow::Owned(metric_name) => {
                            histogram!(metric_name.clone(), elapsed_time.as_secs_f64())
                        }
                    }
                }
                *state = BytecodeWorkerState::Initialized(BytecodeWorkerInitializedState::new(
                    graph_root,
                ));
                None
            }
            BytecodeWorkerState::Initialized(_) => None,
        }
    }
    fn handle_bytecode_interpreter_evaluate<TAction, TTask>(
        &self,
        state: &mut BytecodeWorkerState<T>,
        action: &BytecodeInterpreterEvaluateAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action
            + From<BytecodeInterpreterResultAction<T>>
            + From<BytecodeInterpreterGcCompleteAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let BytecodeInterpreterEvaluateAction {
            cache_id: _,
            state_index,
            state_updates,
        } = action;
        match state {
            BytecodeWorkerState::Uninitialized => None,
            BytecodeWorkerState::Initialized(state) => {
                let state_index = *state_index;
                state.state_index = state_index;
                state.state_values.extend(
                    state_updates
                        .iter()
                        .map(|(state_token, value)| (*state_token, value.clone())),
                );
                let (program, entry_point) = &state.graph_root;
                let state_id = state_index.map(|value| value.into()).unwrap_or(0);
                let start_time = Instant::now();
                let result = execute(
                    self.cache_id,
                    program,
                    *entry_point,
                    state_id,
                    &state.state_values,
                    &self.factory,
                    &self.allocator,
                    &self.interpreter_options,
                    &state.cache,
                );
                let elapsed_time = start_time.elapsed();
                match &self.metric_names.query_worker_evaluate_duration {
                    Cow::Borrowed(metric_name) => {
                        histogram!(*metric_name, elapsed_time.as_secs_f64())
                    }
                    Cow::Owned(metric_name) => {
                        histogram!(metric_name.clone(), elapsed_time.as_secs_f64())
                    }
                }
                let result = match result {
                    Ok((result, cache_entries)) => {
                        state.latest_result = Some(result.clone());
                        state.cache.extend(cache_entries);
                        result
                    }
                    Err(message) => EvaluationResult::new(
                        create_error_expression(message, &self.factory, &self.allocator),
                        DependencyList::empty(),
                    ),
                };
                Some(SchedulerTransition::new(once(SchedulerCommand::Send(
                    self.caller_pid,
                    BytecodeInterpreterResultAction {
                        cache_id: self.cache_id,
                        state_index,
                        result: result.clone(),
                        statistics: state.get_statistics(),
                    }
                    .into(),
                ))))
            }
        }
    }
    fn handle_bytecode_interpreter_gc<TAction, TTask>(
        &self,
        state: &mut BytecodeWorkerState<T>,
        action: &BytecodeInterpreterGcAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<BytecodeInterpreterGcCompleteAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let BytecodeInterpreterGcAction {
            cache_id,
            state_index,
        } = action;
        match state {
            BytecodeWorkerState::Uninitialized => None,
            BytecodeWorkerState::Initialized(state) => {
                let latest_state_index = &state.state_index;
                if state_index < latest_state_index {
                    return None;
                }
                let start_time = Instant::now();
                state.cache.gc(once(self.cache_id));
                let empty_dependencies = DependencyList::default();
                let retained_keys = state
                    .latest_result
                    .as_ref()
                    .map(|result| result.dependencies())
                    .unwrap_or(&empty_dependencies);
                if retained_keys.len() < state.state_values.len() {
                    state.state_values.gc(retained_keys);
                }
                let elapsed_time = start_time.elapsed();
                match &self.metric_names.query_worker_gc_duration {
                    Cow::Borrowed(metric_name) => {
                        histogram!(*metric_name, elapsed_time.as_secs_f64())
                    }
                    Cow::Owned(metric_name) => {
                        histogram!(metric_name.clone(), elapsed_time.as_secs_f64())
                    }
                }
                Some(SchedulerTransition::new(once(SchedulerCommand::Send(
                    self.caller_pid,
                    BytecodeInterpreterGcCompleteAction {
                        cache_id: *cache_id,
                        statistics: state.get_statistics(),
                    }
                    .into(),
                ))))
            }
        }
    }
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
