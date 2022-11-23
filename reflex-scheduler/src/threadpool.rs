// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{marker::PhantomData, pin::Pin};

use futures::{Future, Stream, StreamExt};
use reflex_dispatcher::{Action, Handler, HandlerContext, TaskFactory};

use crate::tokio::{TokioSchedulerHandlerTimer, TokioThreadPoolFactory, TokioThreadPoolRequest};

pub struct TokioRuntimeThreadPoolFactory<T, TAction, TTask>
where
    T: TokioThreadPoolFactory<TAction, TTask>,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    runtime: tokio::runtime::Handle,
    threadpool: T,
    _action: PhantomData<TAction>,
    _task: PhantomData<TTask>,
}
impl<T, TAction, TTask> TokioRuntimeThreadPoolFactory<T, TAction, TTask>
where
    T: TokioThreadPoolFactory<TAction, TTask>,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    pub fn new(runtime: tokio::runtime::Handle, threadpool: T) -> Self {
        Self {
            runtime,
            threadpool,
            _action: PhantomData,
            _task: PhantomData,
        }
    }
}
impl<T, TAction, TTask> TokioThreadPoolFactory<TAction, TTask>
    for TokioRuntimeThreadPoolFactory<T, TAction, TTask>
where
    T: TokioThreadPoolFactory<TAction, TTask>,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    type Task = Pin<Box<dyn Future<Output = ()> + Send + 'static>>;
    fn create(
        self,
        requests: impl Stream<Item = TokioThreadPoolRequest<TAction, TTask>> + Unpin + Send + 'static,
    ) -> Self::Task {
        let Self {
            runtime,
            threadpool,
            ..
        } = self;
        let task = runtime.spawn(threadpool.create(requests));
        Box::pin(async move {
            let _ = task.await;
        })
    }
}

pub struct AsyncTokioThreadPoolFactory<TTimer, TAction, TTask>
where
    TTimer: TokioSchedulerHandlerTimer<Action = TAction, Task = TTask> + Send + 'static,
    TTimer::Span: Send + 'static,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    timer: TTimer,
    _action: PhantomData<TAction>,
    _task: PhantomData<TTask>,
}
impl<TTimer, TAction, TTask> AsyncTokioThreadPoolFactory<TTimer, TAction, TTask>
where
    TTimer: TokioSchedulerHandlerTimer<Action = TAction, Task = TTask> + Send + 'static,
    TTimer::Span: Send + 'static,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    pub fn new(timer: TTimer) -> Self {
        Self {
            timer,
            _action: PhantomData,
            _task: PhantomData,
        }
    }
}
impl<TTimer, TAction, TTask> TokioThreadPoolFactory<TAction, TTask>
    for AsyncTokioThreadPoolFactory<TTimer, TAction, TTask>
where
    TTimer: TokioSchedulerHandlerTimer<Action = TAction, Task = TTask> + Send + 'static,
    TTimer::Span: Send + 'static,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    type Task = Pin<Box<dyn Future<Output = ()> + Send + 'static>>;
    fn create(
        self,
        mut requests: impl Stream<Item = TokioThreadPoolRequest<TAction, TTask>>
            + Unpin
            + Send
            + 'static,
    ) -> Self::Task {
        let Self { timer, .. } = self;
        Box::pin(async move {
            while let Some(request) = requests.next().await {
                let TokioThreadPoolRequest {
                    actor,
                    mut state,
                    message,
                    metadata,
                    mut context,
                    response,
                } = request;
                let pid = context.pid();
                let span = timer.start_span(&actor, &message, pid, &metadata);
                let result = actor.handle(&mut state, &message, &metadata, &mut context);
                timer.end_span(span, &actor, &message, pid, &metadata);
                let _ = response.send((message, state, result));
            }
        })
    }
}
