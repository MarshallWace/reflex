// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::marker::PhantomData;

use reflex_dispatcher::{
    Action, MessageData, MiddlewareContext, PostMiddleware, PreMiddleware, SchedulerCommand,
    SchedulerTransition, TaskFactory,
};

use crate::logger::ActionLogger;

pub trait LoggerMiddlewareFactory<T: ActionLogger> {
    fn create(&self) -> T;
}
impl<T> LoggerMiddlewareFactory<T> for T
where
    T: ActionLogger + Clone,
{
    fn create(&self) -> Self {
        self.clone()
    }
}

pub struct LoggerMiddleware<TFactory, TLogger>
where
    TFactory: LoggerMiddlewareFactory<TLogger>,
    TLogger: ActionLogger,
{
    factory: TFactory,
    _logger: PhantomData<TLogger>,
}
impl<TFactory, TLogger> Clone for LoggerMiddleware<TFactory, TLogger>
where
    TFactory: LoggerMiddlewareFactory<TLogger> + Clone,
    TLogger: ActionLogger,
{
    fn clone(&self) -> Self {
        Self {
            factory: self.factory.clone(),
            _logger: Default::default(),
        }
    }
}
impl<TFactory, TLogger> LoggerMiddleware<TFactory, TLogger>
where
    TFactory: LoggerMiddlewareFactory<TLogger>,
    TLogger: ActionLogger,
{
    pub fn new(factory: TFactory) -> Self {
        Self {
            factory,
            _logger: Default::default(),
        }
    }
}

#[derive(Default)]
pub struct LoggerMiddlewareState<TLogger: ActionLogger> {
    logger: TLogger,
}

impl<TFactory, TLogger, TAction, TTask> PreMiddleware<TAction, TTask>
    for LoggerMiddleware<TFactory, TLogger>
where
    TFactory: LoggerMiddlewareFactory<TLogger>,
    TLogger: ActionLogger<Action = TAction, Task = TTask>,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    type State = LoggerMiddlewareState<TLogger>;
    fn init(&self) -> Self::State {
        LoggerMiddlewareState {
            logger: self.factory.create(),
        }
    }
    fn handle(
        &self,
        state: &mut Self::State,
        operation: SchedulerCommand<TAction, TTask>,
        metadata: &MessageData,
        context: &MiddlewareContext,
    ) -> SchedulerCommand<TAction, TTask> {
        state.logger.log(&operation, Some(metadata), Some(context));
        operation
    }
}
impl<TFactory, TLogger, TAction, TTask> PostMiddleware<TAction, TTask>
    for LoggerMiddleware<TFactory, TLogger>
where
    TFactory: LoggerMiddlewareFactory<TLogger>,
    TLogger: ActionLogger<Action = TAction, Task = TTask>,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    type State = LoggerMiddlewareState<TLogger>;
    fn init(&self) -> Self::State {
        LoggerMiddlewareState {
            logger: self.factory.create(),
        }
    }
    fn handle(
        &self,
        state: &mut Self::State,
        operations: Vec<SchedulerCommand<TAction, TTask>>,
        metadata: &MessageData,
        context: &MiddlewareContext,
    ) -> Option<SchedulerTransition<TAction, TTask>> {
        for operation in operations.iter() {
            state.logger.log(operation, Some(metadata), Some(context));
        }
        Some(SchedulerTransition::new(operations))
    }
}
