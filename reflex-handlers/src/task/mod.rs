// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use reflex_dispatcher::{Action, TaskFactory};
use reflex_macros::{blanket_trait, task_factory_enum};

pub mod fetch;
pub mod graphql;
pub mod timeout;
pub mod timestamp;

use crate::task::graphql::{
    GraphQlHandlerHttpFetchTaskFactory, GraphQlHandlerWebSocketConnectionTaskFactory,
};
use crate::task::{
    fetch::{FetchHandlerTask, FetchHandlerTaskAction, FetchHandlerTaskFactory},
    graphql::{GraphQlHandlerTask, GraphQlHandlerTaskAction, GraphQlHandlerTaskFactory},
    timeout::{TimeoutHandlerTask, TimeoutHandlerTaskAction, TimeoutHandlerTaskFactory},
    timestamp::{TimestampHandlerTask, TimestampHandlerTaskAction, TimestampHandlerTaskFactory},
};

blanket_trait!(
    pub trait DefaultHandlersTaskAction:
        Action
        + FetchHandlerTaskAction
        + GraphQlHandlerTaskAction
        + TimeoutHandlerTaskAction
        + TimestampHandlerTaskAction
    {
    }
);

blanket_trait!(
    pub trait DefaultHandlersTask<TConnect>:
        FetchHandlerTask<TConnect>
        + GraphQlHandlerTask<TConnect>
        + TimeoutHandlerTask
        + TimestampHandlerTask
    where
        TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    {
    }
);

task_factory_enum!({
    #[derive(Clone)]
    pub enum DefaultHandlersTaskFactory<TConnect>
    where
        TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    {
        Fetch(FetchHandlerTaskFactory<TConnect>),
        GraphQl(GraphQlHandlerTaskFactory<TConnect>),
        Timeout(TimeoutHandlerTaskFactory),
        Timestamp(TimestampHandlerTaskFactory),
    }

    impl<TConnect, TAction, TTask> TaskFactory<TAction, TTask> for DefaultHandlersTaskFactory<TConnect>
    where
        TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
        TAction: Action + DefaultHandlersTaskAction + Send + 'static + Send + 'static,
        TTask: TaskFactory<TAction, TTask>,
    {
    }
});

impl<TConnect> From<GraphQlHandlerHttpFetchTaskFactory<TConnect>>
    for DefaultHandlersTaskFactory<TConnect>
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    fn from(value: GraphQlHandlerHttpFetchTaskFactory<TConnect>) -> Self {
        Self::GraphQl(GraphQlHandlerTaskFactory::HttpFetch(value))
    }
}

impl<TConnect> From<GraphQlHandlerWebSocketConnectionTaskFactory>
    for DefaultHandlersTaskFactory<TConnect>
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    fn from(value: GraphQlHandlerWebSocketConnectionTaskFactory) -> Self {
        Self::GraphQl(GraphQlHandlerTaskFactory::WebSocketConnection(value))
    }
}
