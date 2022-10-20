// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::marker::PhantomData;

use reflex_dispatcher::{Action, MessageData, MiddlewareContext, SchedulerCommand, TaskFactory};

pub mod formatted;
pub mod json;
pub mod prometheus;

pub trait ActionLogger {
    type Action: Action;
    type Task: TaskFactory<Self::Action, Self::Task>;
    fn log(
        &mut self,
        operation: &SchedulerCommand<Self::Action, Self::Task>,
        metadata: Option<&MessageData>,
        context: Option<&MiddlewareContext>,
    );
}

#[derive(Copy, Clone, Default, Debug)]
struct NoopLogger<TAction, TTask>
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    _action: PhantomData<TAction>,
    _task: PhantomData<TTask>,
}
impl<TAction, TTask> ActionLogger for NoopLogger<TAction, TTask>
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    type Action = TAction;
    type Task = TTask;
    fn log(
        &mut self,
        _operation: &SchedulerCommand<Self::Action, Self::Task>,
        _metadata: Option<&MessageData>,
        _context: Option<&MiddlewareContext>,
    ) {
    }
}

impl<T, TAction, TTask> ActionLogger for Option<T>
where
    T: ActionLogger<Action = TAction, Task = TTask>,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    type Action = TAction;
    type Task = TTask;
    fn log(
        &mut self,
        operation: &SchedulerCommand<Self::Action, Self::Task>,
        metadata: Option<&MessageData>,
        context: Option<&MiddlewareContext>,
    ) {
        if let Some(inner) = self {
            inner.log(operation, metadata, context);
        }
    }
}

pub enum EitherLogger<T1, T2> {
    Left(T1),
    Right(T2),
}
impl<T1, T2> Clone for EitherLogger<T1, T2>
where
    T1: Clone,
    T2: Clone,
{
    fn clone(&self) -> Self {
        match self {
            Self::Left(inner) => Self::Left(inner.clone()),
            Self::Right(inner) => Self::Right(inner.clone()),
        }
    }
}
impl<T1, T2, TAction, TTask> ActionLogger for EitherLogger<T1, T2>
where
    T1: ActionLogger<Action = TAction, Task = TTask>,
    T2: ActionLogger<Action = TAction, Task = TTask>,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    type Action = TAction;
    type Task = TTask;
    fn log(
        &mut self,
        operation: &SchedulerCommand<Self::Action, Self::Task>,
        metadata: Option<&MessageData>,
        context: Option<&MiddlewareContext>,
    ) {
        match self {
            Self::Left(logger) => logger.log(operation, metadata, context),
            Self::Right(logger) => logger.log(operation, metadata, context),
        }
    }
}

pub struct ChainLogger<T1, T2> {
    left: T1,
    right: T2,
}
impl<T1, T2> ChainLogger<T1, T2> {
    pub fn new(left: T1, right: T2) -> Self {
        Self { left, right }
    }
}
impl<T1, T2> Clone for ChainLogger<T1, T2>
where
    T1: Clone,
    T2: Clone,
{
    fn clone(&self) -> Self {
        Self {
            left: self.left.clone(),
            right: self.right.clone(),
        }
    }
}
impl<T1, T2, TAction, TTask> ActionLogger for ChainLogger<T1, T2>
where
    T1: ActionLogger<Action = TAction, Task = TTask>,
    T2: ActionLogger<Action = TAction, Task = TTask>,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    type Action = TAction;
    type Task = TTask;
    fn log(
        &mut self,
        operation: &SchedulerCommand<Self::Action, Self::Task>,
        metadata: Option<&MessageData>,
        context: Option<&MiddlewareContext>,
    ) {
        self.left.log(operation, metadata, context);
        self.right.log(operation, metadata, context);
    }
}
