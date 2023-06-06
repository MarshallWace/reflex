// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use actor::HandlerActor;
use hyper::Body;
use reflex::core::{Applicable, Expression};
use reflex_dispatcher::{Action, ProcessId, TaskFactory};
use reflex_runtime::{AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator};
use reflex_utils::reconnect::ReconnectTimeout;

use crate::{
    actor::{
        fetch::{FetchHandler, FetchHandlerAction, FetchHandlerMetricNames},
        graphql::{GraphQlHandler, GraphQlHandlerAction, GraphQlHandlerMetricNames},
        loader::{LoaderHandler, LoaderHandlerAction, LoaderHandlerMetricNames},
        scan::{ScanHandler, ScanHandlerAction, ScanHandlerMetricNames},
        timeout::{TimeoutHandler, TimeoutHandlerAction},
        timestamp::{TimestampHandler, TimestampHandlerAction},
        variable::{VariableHandler, VariableHandlerAction},
    },
    task::{
        fetch::FetchHandlerTask, graphql::GraphQlHandlerTask, timeout::TimeoutHandlerTask,
        timestamp::TimestampHandlerTask,
    },
};

pub use hyper;
use reflex_macros::blanket_trait;

pub mod action;
pub mod actor;
pub mod imports;
pub mod loader;
pub mod stdlib;
pub mod task;
pub mod utils;

blanket_trait!(
    pub trait DefaultHandlerAction<T: Expression>:
        FetchHandlerAction<T>
        + GraphQlHandlerAction<T>
        + LoaderHandlerAction<T>
        + ScanHandlerAction<T>
        + TimeoutHandlerAction<T>
        + TimestampHandlerAction<T>
        + VariableHandlerAction<T>
    {
    }
);

#[derive(Default, Clone, Copy, Debug)]
pub struct DefaultHandlerMetricNames {
    pub fetch_handler: FetchHandlerMetricNames,
    pub graphql_handler: GraphQlHandlerMetricNames,
    pub loader_handler: LoaderHandlerMetricNames,
    pub scan_handler: ScanHandlerMetricNames,
}

pub trait DefaultHandlerTask<TConnect>:
    FetchHandlerTask<TConnect>
    + TimeoutHandlerTask
    + TimestampHandlerTask
    + GraphQlHandlerTask<TConnect>
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
}

impl<TSelf, TConnect> DefaultHandlerTask<TConnect> for TSelf
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    Self: FetchHandlerTask<TConnect>
        + TimeoutHandlerTask
        + TimestampHandlerTask
        + GraphQlHandlerTask<TConnect>,
{
}

pub fn default_handler_actors<TAction, TTask, T, TFactory, TAllocator, TConnect, TReconnect>(
    https_client: hyper::Client<TConnect, Body>,
    factory: &TFactory,
    allocator: &TAllocator,
    reconnect_timeout: TReconnect,
    metric_names: DefaultHandlerMetricNames,
    main_pid: ProcessId,
) -> impl IntoIterator<Item = HandlerActor<T, TFactory, TAllocator, TConnect, TReconnect>>
where
    T: AsyncExpression + Applicable<T>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal: Send,
    T::SignalList: Send,
    T::StructPrototype: Send,
    T::ExpressionList: Send,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TAction: Action + DefaultHandlerAction<T> + Send + 'static,
    TTask: TaskFactory<TAction, TTask> + DefaultHandlerTask<TConnect>,
{
    [
        HandlerActor::FetchHandler(FetchHandler::new(
            https_client.clone(),
            factory.clone(),
            allocator.clone(),
            metric_names.fetch_handler,
            main_pid,
        )),
        HandlerActor::GraphQlHandler(GraphQlHandler::new(
            https_client,
            factory.clone(),
            allocator.clone(),
            reconnect_timeout,
            metric_names.graphql_handler,
            main_pid,
        )),
        HandlerActor::LoaderHandler(LoaderHandler::new(
            factory.clone(),
            allocator.clone(),
            metric_names.loader_handler,
            main_pid,
        )),
        HandlerActor::ScanHandler(ScanHandler::new(
            factory.clone(),
            allocator.clone(),
            metric_names.scan_handler,
            main_pid,
        )),
        HandlerActor::TimeoutHandler(TimeoutHandler::new(
            factory.clone(),
            allocator.clone(),
            main_pid,
        )),
        HandlerActor::TimestampHandler(TimestampHandler::new(
            factory.clone(),
            allocator.clone(),
            main_pid,
        )),
        HandlerActor::VariableHandler(VariableHandler::new(
            factory.clone(),
            allocator.clone(),
            main_pid,
        )),
    ]
}
