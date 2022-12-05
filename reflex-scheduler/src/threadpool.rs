// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{marker::PhantomData, pin::Pin};

use futures::{Future, Stream, StreamExt};
use reflex_dispatcher::{Action, Handler, SchedulerTransition, TaskFactory};

use crate::tokio::{TokioThreadPoolFactory, TokioThreadPoolRequest};

pub struct TokioRuntimeThreadPoolFactory<TAction, TTask>
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    runtime: tokio::runtime::Handle,
    _action: PhantomData<TAction>,
    _task: PhantomData<TTask>,
}
impl<TAction, TTask> TokioRuntimeThreadPoolFactory<TAction, TTask>
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    pub fn new(runtime: tokio::runtime::Handle) -> Self {
        Self {
            runtime,
            _action: PhantomData,
            _task: PhantomData,
        }
    }
}
impl<TAction, TTask> TokioThreadPoolFactory<TAction, TTask>
    for TokioRuntimeThreadPoolFactory<TAction, TTask>
where
    TAction: Action + Send + Sync + 'static,
    TTask: TaskFactory<TAction, TTask> + Send + 'static,
    TTask::Actor: Send + Sync,
    <TTask::Actor as Handler<TAction, SchedulerTransition<TAction, TTask>>>::State: Send,
{
    type Task = Pin<Box<dyn Future<Output = ()> + Send + 'static>>;
    fn create(
        self,
        mut requests: impl Stream<Item = TokioThreadPoolRequest<TAction, TTask>>
            + Unpin
            + Send
            + 'static,
    ) -> Self::Task {
        let Self { runtime, .. } = self;
        let inner_handle = runtime.clone();
        let inner_task = Box::pin(async move {
            while let Some(request) = requests.next().await {
                inner_handle.spawn(async move {
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
                });
            }
        });
        let task = runtime.spawn(inner_task);
        Box::pin(async move {
            let _ = task.await;
        })
    }
}
