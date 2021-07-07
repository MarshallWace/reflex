// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
pub mod date;
pub mod graphql;
pub mod http;

use graphql::graphql_execute_handler;
use reflex::{core::Signal, serialize};
use reflex_runtime::{SignalHandlerResult, SignalHelpers, SignalResult};

pub(crate) mod utils {
    mod fetch;
    mod graphql;
    pub(crate) use fetch::fetch;
    pub(crate) use graphql::{create_websocket_connection, subscribe_websocket_operation};
}

pub fn builtin_signal_handler(
) -> impl Fn(&str, &[&Signal], &SignalHelpers) -> SignalHandlerResult + Send + Sync + 'static {
    compose_signal_handlers(
        date::date_timestamp_handler,
        compose_signal_handlers(http::http_fetch_handler, graphql_execute_handler),
    )
}

pub fn compose_signal_handlers(
    head: impl Fn(&str, &[&Signal], &SignalHelpers) -> SignalHandlerResult + 'static,
    tail: impl Fn(&str, &[&Signal], &SignalHelpers) -> SignalHandlerResult + 'static,
) -> impl Fn(&str, &[&Signal], &SignalHelpers) -> SignalHandlerResult {
    move |signal_type: &str, signals: &[&Signal], helpers: &SignalHelpers| match head(
        signal_type,
        signals,
        helpers,
    ) {
        Some(result) => Some(result),
        None => tail(signal_type, signals, helpers),
    }
}

pub fn debug_signal_handler<THandler>(
    handler: THandler,
) -> impl Fn(&str, &[&Signal], &SignalHelpers) -> SignalHandlerResult + Send + Sync + 'static
where
    THandler: Fn(&str, &[&Signal], &SignalHelpers) -> SignalHandlerResult + Send + Sync + 'static,
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
                        .map(|arg| match serialize(arg.value()) {
                            Ok(value) => format!("{}", value),
                            Err(_) => format!("{}", arg),
                        })
                        .collect::<Vec<_>>()
                        .join(" ")
                )
            };
        }
        handler(signal_type, signals, helpers)
    }
}
