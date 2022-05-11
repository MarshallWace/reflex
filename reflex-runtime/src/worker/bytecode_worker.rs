// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{iter::once, time::Instant};

use metrics::histogram;
use reflex::{
    compiler::{Compile, InstructionPointer, Program},
    core::{
        Applicable, DependencyList, EvaluationResult, Expression, ExpressionFactory, HeapAllocator,
        Reducible, Rewritable, SignalType, StateCache,
    },
    hash::HashId,
    interpreter::{execute, DefaultInterpreterCache, InterpreterOptions, MutableInterpreterCache},
    lang::ValueTerm,
};
use reflex_dispatcher::{
    find_latest_typed_message, Action, HandlerContext, InboundAction, MessageData, MessageOffset,
    OutboundAction, StateOperation, Worker, WorkerContext, WorkerMessageQueue, WorkerTransition,
};

use crate::{
    action::evaluate::{EvaluateResultAction, EvaluateStartAction},
    actor::bytecode_interpreter::BytecodeInterpreterMetricNames,
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, QueryInvalidationStrategy,
};

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
    pub metric_names: BytecodeInterpreterMetricNames,
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
