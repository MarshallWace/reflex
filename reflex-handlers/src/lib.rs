// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use actor::{
    assign::AssignHandlerAction,
    fetch::{FetchHandlerAction, FetchHandlerMetricNames},
    graphql::{GraphQlHandlerAction, GraphQlHandlerMetricNames},
    increment::IncrementHandlerAction,
    loader::{LoaderHandlerAction, LoaderHandlerMetricNames},
    scan::ScanHandlerAction,
    timeout::TimeoutHandlerAction,
    timestamp::TimestampHandlerAction,
};
use hyper::Body;
use reflex::core::{Applicable, Expression};
use reflex_dispatcher::{Action, Actor, ChainedActor};
use reflex_runtime::{AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator};
use reflex_utils::reconnect::ReconnectTimeout;

use crate::actor::{
    assign::AssignHandler, fetch::FetchHandler, graphql::GraphQlHandler,
    increment::IncrementHandler, loader::LoaderHandler, scan::ScanHandler, timeout::TimeoutHandler,
    timestamp::TimestampHandler,
};

pub mod action;
pub mod actor;
pub mod imports;
pub mod loader;
pub mod stdlib;
pub mod utils;

pub trait DefaultHandlersAction<T: Expression>:
    Action
    + AssignHandlerAction<T>
    + FetchHandlerAction<T>
    + GraphQlHandlerAction<T>
    + IncrementHandlerAction<T>
    + LoaderHandlerAction<T>
    + ScanHandlerAction<T>
    + TimeoutHandlerAction<T>
    + TimestampHandlerAction<T>
{
}
impl<T: Expression, TAction> DefaultHandlersAction<T> for TAction where
    Self: Action
        + AssignHandlerAction<T>
        + FetchHandlerAction<T>
        + GraphQlHandlerAction<T>
        + IncrementHandlerAction<T>
        + LoaderHandlerAction<T>
        + ScanHandlerAction<T>
        + TimeoutHandlerAction<T>
        + TimestampHandlerAction<T>
{
}

#[derive(Default, Clone, Copy, Debug)]
pub struct DefaultHandlersMetricNames {
    pub fetch_handler: FetchHandlerMetricNames,
    pub graphql_handler: GraphQlHandlerMetricNames,
    pub loader_handler: LoaderHandlerMetricNames,
}

pub fn default_handlers<TAction, T, TFactory, TAllocator, TConnect, TReconnect>(
    https_client: hyper::Client<TConnect, Body>,
    factory: &TFactory,
    allocator: &TAllocator,
    reconnect_timeout: TReconnect,
    metric_names: DefaultHandlersMetricNames,
) -> impl Actor<TAction, State = impl Send> + Send + Clone
where
    T: AsyncExpression + Applicable<T>,
    TFactory: AsyncExpressionFactory<T> + Sync,
    TAllocator: AsyncHeapAllocator<T> + Sync,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone,
    TAction: DefaultHandlersAction<T> + Send + 'static,
{
    ChainedActor::new(
        AssignHandler::new(factory.clone(), allocator.clone()),
        ChainedActor::new(
            FetchHandler::new(
                https_client.clone(),
                factory.clone(),
                allocator.clone(),
                metric_names.fetch_handler,
            ),
            ChainedActor::new(
                GraphQlHandler::new(
                    https_client,
                    factory.clone(),
                    allocator.clone(),
                    reconnect_timeout,
                    metric_names.graphql_handler,
                ),
                ChainedActor::new(
                    IncrementHandler::new(factory.clone(), allocator.clone()),
                    ChainedActor::new(
                        LoaderHandler::new(
                            factory.clone(),
                            allocator.clone(),
                            metric_names.loader_handler,
                        ),
                        ChainedActor::new(
                            ScanHandler::new(factory.clone(), allocator.clone()),
                            ChainedActor::new(
                                TimeoutHandler::new(factory.clone(), allocator.clone()),
                                TimestampHandler::new(factory.clone(), allocator.clone()),
                            ),
                        ),
                    ),
                ),
            ),
        ),
    )
}
