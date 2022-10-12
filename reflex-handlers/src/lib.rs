// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use actor::{
    fetch::{FetchHandlerAction, FetchHandlerMetricNames},
    graphql::{GraphQlHandlerAction, GraphQlHandlerMetricNames},
    loader::{LoaderHandlerAction, LoaderHandlerMetricNames},
    scan::{ScanHandlerAction, ScanHandlerMetricNames},
    timeout::TimeoutHandlerAction,
    timestamp::TimestampHandlerAction,
    variable::VariableHandlerAction,
};
use hyper::Body;
use reflex::core::{Applicable, Expression};
use reflex_dispatcher::{
    Action, Actor, ChainedActor, InstrumentedActor, InstrumentedActorMetricNames,
};
use reflex_runtime::{AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator};
use reflex_utils::reconnect::ReconnectTimeout;

use crate::actor::{
    fetch::FetchHandler, graphql::GraphQlHandler, loader::LoaderHandler, scan::ScanHandler,
    timeout::TimeoutHandler, timestamp::TimestampHandler, variable::VariableHandler,
};

pub use hyper;

pub mod action;
pub mod actor;
pub mod imports;
pub mod loader;
pub mod stdlib;
pub mod utils;

pub trait DefaultHandlersAction<T: Expression>:
    Action
    + FetchHandlerAction<T>
    + GraphQlHandlerAction<T>
    + LoaderHandlerAction<T>
    + ScanHandlerAction<T>
    + TimeoutHandlerAction<T>
    + TimestampHandlerAction<T>
    + VariableHandlerAction<T>
{
}
impl<T: Expression, TAction> DefaultHandlersAction<T> for TAction where
    Self: Action
        + FetchHandlerAction<T>
        + GraphQlHandlerAction<T>
        + LoaderHandlerAction<T>
        + ScanHandlerAction<T>
        + TimeoutHandlerAction<T>
        + TimestampHandlerAction<T>
        + VariableHandlerAction<T>
{
}

#[derive(Default, Clone, Copy, Debug)]
pub struct DefaultHandlersMetricNames {
    pub fetch_handler: FetchHandlerMetricNames,
    pub graphql_handler: GraphQlHandlerMetricNames,
    pub loader_handler: LoaderHandlerMetricNames,
    pub scan_handler: ScanHandlerMetricNames,
    pub actor: InstrumentedActorMetricNames,
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
    T::String: Send,
    T::Builtin: Send,
    T::Signal<T>: Send,
    T::SignalList<T>: Send,
    T::StructPrototype<T>: Send,
    T::ExpressionList<T>: Send,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone,
    TAction: DefaultHandlersAction<T> + Send + 'static,
{
    ChainedActor::new(
        InstrumentedActor::new(
            FetchHandler::new(
                https_client.clone(),
                factory.clone(),
                allocator.clone(),
                metric_names.fetch_handler,
            ),
            "FetchHandler",
            metric_names.actor,
        ),
        ChainedActor::new(
            InstrumentedActor::new(
                GraphQlHandler::new(
                    https_client,
                    factory.clone(),
                    allocator.clone(),
                    reconnect_timeout,
                    metric_names.graphql_handler,
                ),
                "GraphQlHandler",
                metric_names.actor,
            ),
            ChainedActor::new(
                InstrumentedActor::new(
                    LoaderHandler::new(
                        factory.clone(),
                        allocator.clone(),
                        metric_names.loader_handler,
                    ),
                    "LoaderHandler",
                    metric_names.actor,
                ),
                ChainedActor::new(
                    InstrumentedActor::new(
                        ScanHandler::new(
                            factory.clone(),
                            allocator.clone(),
                            metric_names.scan_handler,
                        ),
                        "ScanHandler",
                        metric_names.actor,
                    ),
                    ChainedActor::new(
                        InstrumentedActor::new(
                            TimeoutHandler::new(factory.clone(), allocator.clone()),
                            "TimeoutHandler",
                            metric_names.actor,
                        ),
                        ChainedActor::new(
                            InstrumentedActor::new(
                                TimestampHandler::new(factory.clone(), allocator.clone()),
                                "TimestampHandler",
                                metric_names.actor,
                            ),
                            InstrumentedActor::new(
                                VariableHandler::new(factory.clone(), allocator.clone()),
                                "VariableHandler",
                                metric_names.actor,
                            ),
                        ),
                    ),
                ),
            ),
        ),
    )
}
