// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::{
    collections::{hash_map::Entry, HashMap, VecDeque},
    iter::once,
    marker::PhantomData,
    sync::Arc,
};

use reflex::{
    core::{
        Applicable, ConditionListType, ConditionType, EvaluationResult, Expression,
        ExpressionFactory, InstructionPointer, Reducible, RefType, Rewritable, SignalTermType,
        SignalType, StateToken,
    },
    hash::IntMap,
};
use reflex_dispatcher::{
    Action, Actor, ActorTransition, HandlerContext, InboundAction, MessageData, MessageOffset,
    OutboundAction, ProcessId, StateOperation, StateTransition,
};
use reflex_interpreter::{
    compiler::{Compile, CompiledProgram, CompilerOptions},
    InterpreterOptions,
};

use crate::{
    action::{
        bytecode_interpreter::{
            BytecodeInterpreterEvaluateAction, BytecodeInterpreterGcAction,
            BytecodeInterpreterResultAction,
        },
        evaluate::{
            EvaluateResultAction, EvaluateStartAction, EvaluateStopAction, EvaluateUpdateAction,
        },
    },
    worker::bytecode_worker::{
        BytecodeWorkerAction, BytecodeWorkerFactory, BytecodeWorkerMetricNames,
    },
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, QueryInvalidationStrategy,
};

// TODO: Allow tweaking bytecode interpreter GC trigger
const MAX_UPDATES_WITHOUT_GC: usize = 3;

pub trait BytecodeInterpreterAction<T: Expression>:
    Action
    + BytecodeWorkerAction<T>
    + InboundAction<EvaluateStartAction<T>>
    + InboundAction<EvaluateUpdateAction<T>>
    + InboundAction<EvaluateStopAction>
    + InboundAction<BytecodeInterpreterResultAction<T>>
    + OutboundAction<EvaluateResultAction<T>>
    + OutboundAction<BytecodeInterpreterEvaluateAction<T>>
    + OutboundAction<BytecodeInterpreterGcAction>
{
}
impl<T: Expression, TAction> BytecodeInterpreterAction<T> for TAction where
    Self: Action
        + BytecodeWorkerAction<T>
        + InboundAction<EvaluateStartAction<T>>
        + InboundAction<EvaluateUpdateAction<T>>
        + InboundAction<EvaluateStopAction>
        + InboundAction<BytecodeInterpreterResultAction<T>>
        + OutboundAction<EvaluateResultAction<T>>
        + OutboundAction<BytecodeInterpreterEvaluateAction<T>>
        + OutboundAction<BytecodeInterpreterGcAction>
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

pub struct BytecodeInterpreterState<T: Expression> {
    // TODO: Use newtypes for state hashmap keys
    workers: HashMap<StateToken, BytecodeInterpreterWorkerState<T>>,
}
impl<T: Expression> Default for BytecodeInterpreterState<T> {
    fn default() -> Self {
        Self {
            workers: Default::default(),
        }
    }
}

struct BytecodeInterpreterWorkerState<T: Expression> {
    pid: ProcessId,
    state_index: Option<MessageOffset>,
    status: BytecodeInterpreterWorkerStatus<T>,
    invalidation_strategy: QueryInvalidationStrategy,
    updates_since_gc: usize,
}

enum BytecodeInterpreterWorkerStatus<T: Expression> {
    Working(BytecodeWorkerUpdateQueue<T>),
    Idle,
}

enum BytecodeWorkerUpdateQueue<T: Expression> {
    Single(SingleWorkerUpdateQueue<T>),
    Combined(CombinedWorkerUpdateQueue<T>),
    Exact(ExactWorkerUpdateQueue<T>),
}
impl<T: Expression> BytecodeWorkerUpdateQueue<T> {
    fn new(invalidation_strategy: QueryInvalidationStrategy) -> Self {
        match invalidation_strategy {
            QueryInvalidationStrategy::CombineUpdateBatches => Self::Single(Default::default()),
            QueryInvalidationStrategy::Exact => Self::Exact(Default::default()),
        }
    }
    fn push_update_batch(&mut self, updates: &Vec<(StateToken, T)>) {
        match self {
            Self::Single(queue) => {
                let initial_batch = std::mem::take(&mut queue.updates);
                *self = Self::Combined(CombinedWorkerUpdateQueue::from_iter(
                    initial_batch.into_iter().chain(
                        updates
                            .iter()
                            .map(|(state_token, value)| (*state_token, value.clone())),
                    ),
                ))
            }
            Self::Combined(queue) => {
                queue.updates.extend(
                    updates
                        .iter()
                        .map(|(state_token, value)| (*state_token, value.clone())),
                );
            }
            Self::Exact(queue) => queue.batches.push_back(updates.clone()),
        }
    }
    fn pop_update_batch(&mut self) -> Vec<(StateToken, T)> {
        match self {
            Self::Single(queue) => std::mem::take(&mut queue.updates),
            Self::Combined(queue) => std::mem::take(&mut queue.updates).into_iter().collect(),
            Self::Exact(queue) => queue.batches.pop_front().unwrap_or_default(),
        }
    }
}

struct SingleWorkerUpdateQueue<T: Expression> {
    updates: Vec<(StateToken, T)>,
}
impl<T: Expression> Default for SingleWorkerUpdateQueue<T> {
    fn default() -> Self {
        Self {
            updates: Default::default(),
        }
    }
}

struct CombinedWorkerUpdateQueue<T: Expression> {
    updates: IntMap<StateToken, T>,
}
impl<T: Expression> Default for CombinedWorkerUpdateQueue<T> {
    fn default() -> Self {
        Self {
            updates: Default::default(),
        }
    }
}
impl<T: Expression> FromIterator<(StateToken, T)> for CombinedWorkerUpdateQueue<T> {
    fn from_iter<TIter: IntoIterator<Item = (StateToken, T)>>(iter: TIter) -> Self {
        Self {
            updates: iter.into_iter().collect(),
        }
    }
}
impl<T: Expression> IntoIterator for CombinedWorkerUpdateQueue<T> {
    type Item = (StateToken, T);
    type IntoIter = std::collections::hash_map::IntoIter<StateToken, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.updates.into_iter()
    }
}

struct ExactWorkerUpdateQueue<T: Expression> {
    batches: VecDeque<Vec<(StateToken, T)>>,
}
impl<T: Expression> Default for ExactWorkerUpdateQueue<T> {
    fn default() -> Self {
        Self {
            batches: Default::default(),
        }
    }
}

impl<T, TFactory, TAllocator, TAction> Actor<TAction>
    for BytecodeInterpreter<T, TFactory, TAllocator>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TAction: BytecodeInterpreterAction<T> + Send + 'static,
{
    type State = BytecodeInterpreterState<T>;
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
        } else if let Some(action) = action.match_type() {
            self.handle_bytecode_interpreter_result(&mut state, action, metadata, context)
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
        state: &mut BytecodeInterpreterState<T>,
        action: &EvaluateStartAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action
            + Send
            + 'static
            + OutboundAction<BytecodeInterpreterEvaluateAction<T>>
            + BytecodeWorkerAction<T>,
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
                entry.insert(BytecodeInterpreterWorkerState {
                    pid: worker_pid,
                    state_index: None,
                    status: BytecodeInterpreterWorkerStatus::Working(
                        BytecodeWorkerUpdateQueue::new(*invalidation_strategy),
                    ),
                    invalidation_strategy: *invalidation_strategy,
                    updates_since_gc: 0,
                });
                Some(StateTransition::new([
                    StateOperation::spawn(
                        worker_pid,
                        BytecodeWorkerFactory {
                            cache_id: *cache_id,
                            query: query.clone(),
                            evaluation_mode: *evaluation_mode,
                            compiler_options: self.compiler_options,
                            interpreter_options: self.interpreter_options,
                            graph_root: self.graph_root.clone(),
                            factory: self.factory.clone(),
                            allocator: self.allocator.clone(),
                            metric_names: self.metric_names,
                        },
                    ),
                    StateOperation::Send(
                        worker_pid,
                        BytecodeInterpreterEvaluateAction {
                            cache_id: *cache_id,
                            state_index: None,
                            state_updates: Default::default(),
                        }
                        .into(),
                    ),
                ]))
            }
        }
    }
    fn handle_evaluate_update<TAction>(
        &self,
        state: &mut BytecodeInterpreterState<T>,
        action: &EvaluateUpdateAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + Send + 'static + OutboundAction<BytecodeInterpreterEvaluateAction<T>>,
    {
        let EvaluateUpdateAction {
            cache_id,
            state_index,
            state_updates,
        } = action;
        let worker_state = state.workers.get_mut(cache_id)?;
        worker_state.state_index = *state_index;
        let evaluate_action = match &mut worker_state.status {
            BytecodeInterpreterWorkerStatus::Working(update_queue) => {
                update_queue.push_update_batch(&state_updates);
                None
            }
            BytecodeInterpreterWorkerStatus::Idle => Some(BytecodeInterpreterEvaluateAction {
                cache_id: *cache_id,
                state_index: *state_index,
                state_updates: state_updates.clone(),
            }),
        }?;
        let worker_pid = worker_state.pid;
        worker_state.status = BytecodeInterpreterWorkerStatus::Working(
            BytecodeWorkerUpdateQueue::new(worker_state.invalidation_strategy),
        );
        Some(StateTransition::new(once(StateOperation::Send(
            worker_pid,
            evaluate_action.into(),
        ))))
    }
    fn handle_evaluate_stop<TAction>(
        &self,
        state: &mut BytecodeInterpreterState<T>,
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
        let worker_state = state.workers.remove(cache_key)?;
        let worker_pid = worker_state.pid;
        Some(StateTransition::new(once(StateOperation::Kill(worker_pid))))
    }
    fn handle_bytecode_interpreter_result<TAction>(
        &self,
        state: &mut BytecodeInterpreterState<T>,
        action: &BytecodeInterpreterResultAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action
            + Send
            + 'static
            + OutboundAction<EvaluateResultAction<T>>
            + OutboundAction<BytecodeInterpreterEvaluateAction<T>>
            + OutboundAction<BytecodeInterpreterGcAction>,
    {
        let BytecodeInterpreterResultAction {
            cache_id,
            state_index,
            result,
        } = action;
        let worker_state = state.workers.get_mut(cache_id)?;
        let queued_evaluation = match std::mem::replace(
            &mut worker_state.status,
            BytecodeInterpreterWorkerStatus::Idle,
        ) {
            BytecodeInterpreterWorkerStatus::Working(mut existing_queue) => {
                let pending_updates = existing_queue.pop_update_batch();
                if pending_updates.is_empty() {
                    None
                } else {
                    Some((
                        BytecodeInterpreterEvaluateAction {
                            cache_id: *cache_id,
                            state_index: worker_state.state_index,
                            state_updates: pending_updates,
                        },
                        existing_queue,
                    ))
                }
            }
            BytecodeInterpreterWorkerStatus::Idle => None,
        };
        let gc_action = if is_unresolved_result(&result, &self.factory) {
            None
        } else {
            let should_gc = queued_evaluation.is_none()
                || worker_state.updates_since_gc > MAX_UPDATES_WITHOUT_GC;
            if should_gc {
                worker_state.updates_since_gc = 0;
                Some(BytecodeInterpreterGcAction {
                    cache_id: *cache_id,
                    state_index: *state_index,
                })
            } else {
                worker_state.updates_since_gc += 1;
                None
            }
        };
        Some(StateTransition::new(
            once(StateOperation::Send(
                context.pid(),
                EvaluateResultAction {
                    cache_id: *cache_id,
                    state_index: *state_index,
                    result: result.clone(),
                }
                .into(),
            ))
            .chain(gc_action.map(|action| {
                let worker_pid = worker_state.pid;
                StateOperation::Send(worker_pid, action.into())
            }))
            .chain(queued_evaluation.map(|(action, remaining_queue)| {
                let worker_pid = worker_state.pid;
                worker_state.status = BytecodeInterpreterWorkerStatus::Working(remaining_queue);
                StateOperation::Send(worker_pid, action.into())
            })),
        ))
    }
}

fn is_unresolved_result<T: Expression>(
    result: &EvaluationResult<T>,
    factory: &impl ExpressionFactory<T>,
) -> bool {
    factory
        .match_signal_term(result.result())
        .map(|term| {
            term.signals()
                .as_deref()
                .iter()
                .map(|item| item.as_deref())
                .any(is_unresolved_effect)
        })
        .unwrap_or(false)
}

fn is_unresolved_effect<T: Expression<Signal<T> = V>, V: ConditionType<T>>(effect: &V) -> bool {
    match effect.signal_type() {
        SignalType::Error => false,
        SignalType::Pending | SignalType::Custom(_) => true,
    }
}
