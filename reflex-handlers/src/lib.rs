// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use actor::{
    assign::AssignHandlerAction, fetch::FetchHandlerAction, graphql::GraphQlHandlerAction,
    increment::IncrementHandlerAction, loader::LoaderHandlerAction, scan::ScanHandlerAction,
    timeout::TimeoutHandlerAction, timestamp::TimestampHandlerAction,
};
use reflex::core::{Applicable, Expression};
use reflex_dispatcher::{compose_actors, Action, Actor};
use reflex_runtime::{AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator};
use reflex_utils::reconnect::ReconnectTimeout;

use crate::actor::{
    assign::AssignHandler, fetch::FetchHandler, graphql::GraphQlHandler,
    increment::IncrementHandler, loader::LoaderHandler, scan::ScanHandler, timeout::TimeoutHandler,
    timestamp::TimestampHandler,
};

pub(crate) mod utils;

pub mod action;
pub mod actor;
pub mod imports;
pub mod loader;
pub mod stdlib;

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

pub fn default_handlers<TAction, T, TFactory, TAllocator, TReconnect>(
    factory: &TFactory,
    allocator: &TAllocator,
    reconnect_timeout: TReconnect,
) -> impl Actor<TAction>
where
    T: AsyncExpression + Applicable<T>,
    TFactory: AsyncExpressionFactory<T> + Sync,
    TAllocator: AsyncHeapAllocator<T> + Sync,
    TReconnect: ReconnectTimeout,
    TAction: DefaultHandlersAction<T> + Send + 'static,
{
    compose_actors(
        AssignHandler::new(factory.clone(), allocator.clone()),
        compose_actors(
            FetchHandler::new(factory.clone(), allocator.clone()),
            compose_actors(
                GraphQlHandler::new(factory.clone(), allocator.clone(), reconnect_timeout),
                compose_actors(
                    IncrementHandler::new(factory.clone(), allocator.clone()),
                    compose_actors(
                        LoaderHandler::new(factory.clone(), allocator.clone()),
                        compose_actors(
                            ScanHandler::new(factory.clone(), allocator.clone()),
                            compose_actors(
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
