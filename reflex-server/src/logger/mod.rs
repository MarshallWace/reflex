// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::marker::PhantomData;

use reflex_dispatcher::{Action, TaskFactory};
use reflex_scheduler::tokio::{TokioCommand, TokioSchedulerLogger};

pub mod formatted;
pub mod formatter;
pub mod json;
pub mod messages;
pub mod prometheus;

pub trait ActionLogger {
    type Action: Action;
    fn log(&mut self, action: &Self::Action);
}

#[derive(Copy, Debug)]
pub struct NoopLogger<TAction, TTask>
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    _action: PhantomData<TAction>,
    _task: PhantomData<TTask>,
}
impl<TAction, TTask> Clone for NoopLogger<TAction, TTask>
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    fn clone(&self) -> Self {
        Self {
            _action: PhantomData,
            _task: PhantomData,
        }
    }
}
impl<TAction, TTask> Default for NoopLogger<TAction, TTask>
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    fn default() -> Self {
        Self {
            _action: PhantomData,
            _task: PhantomData,
        }
    }
}
impl<TAction, TTask> ActionLogger for NoopLogger<TAction, TTask>
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    type Action = TAction;
    fn log(&mut self, _action: &Self::Action) {}
}
impl<TAction, TTask> TokioSchedulerLogger for NoopLogger<TAction, TTask>
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    type Action = TAction;
    type Task = TTask;
    fn log(&mut self, _command: &TokioCommand<TAction, TTask>) {}
}

impl<T, TAction> ActionLogger for Option<T>
where
    T: ActionLogger<Action = TAction>,
    TAction: Action,
{
    type Action = TAction;
    fn log(&mut self, action: &Self::Action) {
        if let Some(inner) = self {
            inner.log(action);
        }
    }
}

#[derive(Debug)]
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
impl<T1, T2, TAction> ActionLogger for EitherLogger<T1, T2>
where
    T1: ActionLogger<Action = TAction>,
    T2: ActionLogger<Action = TAction>,
    TAction: Action,
{
    type Action = TAction;
    fn log(&mut self, action: &Self::Action) {
        match self {
            Self::Left(logger) => logger.log(action),
            Self::Right(logger) => logger.log(action),
        }
    }
}
impl<T1, T2, TAction, TTask> TokioSchedulerLogger for EitherLogger<T1, T2>
where
    T1: TokioSchedulerLogger<Action = TAction, Task = TTask>,
    T2: TokioSchedulerLogger<Action = TAction, Task = TTask>,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    type Action = TAction;
    type Task = TTask;
    fn log(&mut self, command: &TokioCommand<TAction, TTask>) {
        match self {
            Self::Left(inner) => inner.log(command),
            Self::Right(inner) => inner.log(command),
        }
    }
}

#[derive(Debug)]
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
impl<T1, T2, TAction> ActionLogger for ChainLogger<T1, T2>
where
    T1: ActionLogger<Action = TAction>,
    T2: ActionLogger<Action = TAction>,
    TAction: Action,
{
    type Action = TAction;
    fn log(&mut self, action: &Self::Action) {
        self.left.log(action);
        self.right.log(action);
    }
}
impl<T1, T2, TAction, TTask> TokioSchedulerLogger for ChainLogger<T1, T2>
where
    T1: TokioSchedulerLogger<Action = TAction, Task = TTask>,
    T2: TokioSchedulerLogger<Action = TAction, Task = TTask>,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    type Action = TAction;
    type Task = TTask;
    fn log(&mut self, command: &TokioCommand<TAction, TTask>) {
        self.left.log(command);
        self.right.log(command);
    }
}

pub trait FilteredLoggerPredicate<TAction: Action, TTask: TaskFactory<TAction, TTask>> {
    fn accept(&self, command: &TokioCommand<TAction, TTask>) -> bool;
}
impl<T, TAction, TTask> FilteredLoggerPredicate<TAction, TTask> for T
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
    Self: Fn(&TokioCommand<TAction, TTask>) -> bool,
{
    fn accept(&self, command: &TokioCommand<TAction, TTask>) -> bool {
        (self)(command)
    }
}

pub struct FilteredLogger<T, TPredicate, TAction, TTask>
where
    T: TokioSchedulerLogger<Action = TAction, Task = TTask>,
    TPredicate: FilteredLoggerPredicate<TAction, TTask>,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    inner: T,
    predicate: TPredicate,
}
impl<T, TPredicate, TAction, TTask> FilteredLogger<T, TPredicate, TAction, TTask>
where
    T: TokioSchedulerLogger<Action = TAction, Task = TTask>,
    TPredicate: FilteredLoggerPredicate<TAction, TTask>,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    pub fn new(inner: T, predicate: TPredicate) -> Self {
        Self { inner, predicate }
    }
}
impl<T, TPredicate, TAction, TTask> TokioSchedulerLogger
    for FilteredLogger<T, TPredicate, TAction, TTask>
where
    T: TokioSchedulerLogger<Action = TAction, Task = TTask>,
    TPredicate: FilteredLoggerPredicate<TAction, TTask>,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    type Action = TAction;
    type Task = TTask;
    fn log(&mut self, command: &TokioCommand<Self::Action, Self::Task>) {
        if self.predicate.accept(command) {
            self.inner.log(command)
        }
    }
}
