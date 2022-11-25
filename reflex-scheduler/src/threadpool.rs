// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{marker::PhantomData, pin::Pin};

use futures::{Future, Stream, StreamExt};
use reflex_dispatcher::{Action, Handler, TaskFactory};

use crate::tokio::{TokioThreadPoolFactory, TokioThreadPoolRequest};

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

pub struct AsyncTokioThreadPoolFactory<TAction, TTask>
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    _action: PhantomData<TAction>,
    _task: PhantomData<TTask>,
}
impl<TAction, TTask> Default for AsyncTokioThreadPoolFactory<TAction, TTask>
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
impl<TAction, TTask> TokioThreadPoolFactory<TAction, TTask>
    for AsyncTokioThreadPoolFactory<TAction, TTask>
where
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
                let result = actor.handle(&mut state, &message, &metadata, &mut context);
                let _ = response.send((message, state, result));
            }
        })
    }
}
