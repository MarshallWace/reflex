// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
pub mod date;
pub mod graphql;
pub mod http;
pub mod loader;

use graphql::graphql_execute_handler;
use reflex::{
    compiler::Compile,
    core::{Applicable, Reducible, Rewritable, Signal, StringValue},
};
use reflex_runtime::{
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, SignalHandlerResult,
    SignalHelpers, SignalResult,
};

pub(crate) mod utils {
    mod fetch;
    mod graphql;
    pub(crate) use fetch::fetch;
    pub(crate) use graphql::{
        create_json_error_object, create_websocket_connection, subscribe_websocket_operation,
    };
}

pub fn builtin_signal_handler<
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
>(
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
) -> impl Fn(&str, &[&Signal<T>], &SignalHelpers<T>) -> SignalHandlerResult<T> + Send + Sync + 'static
where
    T::String: StringValue + Send + Sync,
{
    compose_signal_handlers(
        date::date_timestamp_handler(factory, allocator),
        compose_signal_handlers(
            loader::load_signal_handler(factory, allocator),
            compose_signal_handlers(
                http::http_fetch_handler(factory, allocator),
                graphql_execute_handler(factory, allocator),
            ),
        ),
    )
}

pub fn compose_signal_handlers<T: AsyncExpression + Compile<T>>(
    head: impl Fn(&str, &[&Signal<T>], &SignalHelpers<T>) -> SignalHandlerResult<T> + 'static,
    tail: impl Fn(&str, &[&Signal<T>], &SignalHelpers<T>) -> SignalHandlerResult<T> + 'static,
) -> impl Fn(&str, &[&Signal<T>], &SignalHelpers<T>) -> SignalHandlerResult<T>
where
    T::String: StringValue + Send + Sync,
{
    move |signal_type: &str, signals: &[&Signal<T>], helpers: &SignalHelpers<T>| match head(
        signal_type,
        signals,
        helpers,
    ) {
        Some(result) => Some(result),
        None => tail(signal_type, signals, helpers),
    }
}

pub fn debug_signal_handler<T: AsyncExpression + Compile<T>, THandler>(
    handler: THandler,
) -> impl Fn(&str, &[&Signal<T>], &SignalHelpers<T>) -> SignalHandlerResult<T> + Send + Sync + 'static
where
    T::String: StringValue + Send + Sync,
    THandler: Fn(&str, &[&Signal<T>], &SignalHelpers<T>) -> SignalHandlerResult<T>
        + Send
        + Sync
        + 'static,
{
    move |signal_type, signals, helpers| {
        eprintln!(
            "[{}]{}",
            signal_type,
            if signals.len() == 1 {
                String::new()
            } else {
                format!(" x {}", signals.len())
            }
        );
        for signal in signals.iter() {
            let args = signal.args();
            if !args.is_empty() {
                eprintln!(
                    "  {}",
                    args.iter()
                        .map(|arg| format!("{}", arg))
                        .collect::<Vec<_>>()
                        .join(" ")
                )
            };
        }
        handler(signal_type, signals, helpers)
    }
}
