// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::{
    collections::{hash_map::Entry, HashMap},
    iter::once,
    marker::PhantomData,
    sync::Arc,
};

use reflex::{
    core::{Applicable, Expression, InstructionPointer, Reducible, Rewritable},
    hash::HashId,
};
use reflex_dispatcher::{
    Action, Actor, ActorTransition, HandlerContext, InboundAction, MessageData, OutboundAction,
    ProcessId, StateOperation, StateTransition,
};
use reflex_interpreter::{
    compiler::{Compile, CompiledProgram, CompilerOptions},
    InterpreterOptions,
};

use crate::{
    action::evaluate::{
        EvaluateResultAction, EvaluateStartAction, EvaluateStopAction, EvaluateUpdateAction,
    },
    worker::bytecode_worker::{
        BytecodeWorkerAction, BytecodeWorkerFactory, BytecodeWorkerMetricNames,
    },
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator,
};

pub trait BytecodeInterpreterAction<T: Expression>:
    Action
    + BytecodeWorkerAction<T>
    + InboundAction<EvaluateStartAction<T>>
    + InboundAction<EvaluateUpdateAction<T>>
    + InboundAction<EvaluateResultAction<T>>
    + InboundAction<EvaluateStopAction>
    + OutboundAction<EvaluateUpdateAction<T>>
    + OutboundAction<EvaluateResultAction<T>>
{
}
impl<T: Expression, TAction> BytecodeInterpreterAction<T> for TAction where
    Self: Action
        + BytecodeWorkerAction<T>
        + InboundAction<EvaluateStartAction<T>>
        + InboundAction<EvaluateUpdateAction<T>>
        + InboundAction<EvaluateResultAction<T>>
        + InboundAction<EvaluateStopAction>
        + OutboundAction<EvaluateUpdateAction<T>>
        + OutboundAction<EvaluateResultAction<T>>
{
}

pub struct BytecodeInterpreter<T, TFactory, TAllocator>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
{
    graph_root: Arc<(CompiledProgram, InstructionPointer)>,
    compiler_options: CompilerOptions,
    interpreter_options: InterpreterOptions,
    factory: TFactory,
    allocator: TAllocator,
    metric_names: BytecodeWorkerMetricNames,
    _expression: PhantomData<T>,
}
impl<T, TFactory, TAllocator> BytecodeInterpreter<T, TFactory, TAllocator>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
{
    pub fn new(
        graph_root: (CompiledProgram, InstructionPointer),
        compiler_options: CompilerOptions,
        interpreter_options: InterpreterOptions,
        factory: TFactory,
        allocator: TAllocator,
        metric_names: BytecodeWorkerMetricNames,
    ) -> Self {
        Self {
            graph_root: Arc::new(graph_root),
            compiler_options,
            interpreter_options,
            factory,
            allocator,
            metric_names: metric_names.init(),
            _expression: Default::default(),
        }
    }
}

#[derive(Default)]
pub struct BytecodeInterpreterState {
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
    type State = BytecodeInterpreterState;
    fn init(&self) -> Self::State {
        Default::default()
    }
    fn handle(
        &self,
        state: Self::State,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> ActorTransition<Self::State, TAction> {
        let mut state = state;
        let actions = if let Some(action) = action.match_type() {
            self.handle_evaluate_start(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_evaluate_update(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_evaluate_stop(&mut state, action, metadata, context)
        } else {
            None
        }
        .unwrap_or_default();
        ActorTransition::new(state, actions)
    }
}
impl<T, TFactory, TAllocator> BytecodeInterpreter<T, TFactory, TAllocator>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
{
    fn handle_evaluate_start<TAction>(
        &self,
        state: &mut BytecodeInterpreterState,
        action: &EvaluateStartAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action
            + Send
            + 'static
            + InboundAction<EvaluateStartAction<T>>
            + InboundAction<EvaluateUpdateAction<T>>
            + InboundAction<EvaluateResultAction<T>>
            + OutboundAction<EvaluateStartAction<T>>
            + OutboundAction<EvaluateUpdateAction<T>>
            + OutboundAction<EvaluateResultAction<T>>,
    {
        let EvaluateStartAction {
            cache_id,
            label: _,
            query,
            evaluation_mode,
            invalidation_strategy,
        } = action;
        match state.workers.entry(*cache_id) {
            Entry::Occupied(_) => None,
            Entry::Vacant(entry) => {
                let worker_pid = context.generate_pid();
                entry.insert(worker_pid);
                Some(StateTransition::new([
                    StateOperation::spawn(
                        worker_pid,
                        BytecodeWorkerFactory {
                            cache_id: *cache_id,
                            query: query.clone(),
                            evaluation_mode: *evaluation_mode,
                            invalidation_strategy: *invalidation_strategy,
                            compiler_options: self.compiler_options,
                            interpreter_options: self.interpreter_options,
                            graph_root: self.graph_root.clone(),
                            factory: self.factory.clone(),
                            allocator: self.allocator.clone(),
                            metric_names: self.metric_names,
                        },
                    ),
                    StateOperation::Send(worker_pid, action.clone().into()),
                ]))
            }
        }
    }
    fn handle_evaluate_update<TAction>(
        &self,
        state: &mut BytecodeInterpreterState,
        action: &EvaluateUpdateAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action
            + Send
            + 'static
            + InboundAction<EvaluateUpdateAction<T>>
            + OutboundAction<EvaluateUpdateAction<T>>,
    {
        let EvaluateUpdateAction {
            cache_id: cache_key,
            ..
        } = action;
        let worker_pid = state.workers.get(cache_key).copied()?;
        Some(StateTransition::new(once(StateOperation::Send(
            worker_pid,
            action.clone().into(),
        ))))
    }
    fn handle_evaluate_stop<TAction>(
        &self,
        state: &mut BytecodeInterpreterState,
        action: &EvaluateStopAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action,
    {
        let EvaluateStopAction {
            cache_id: cache_key,
        } = action;
        let worker_pid = state.workers.remove(cache_key)?;
        Some(StateTransition::new(once(StateOperation::Kill(worker_pid))))
    }
}
