// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
pub mod date;
pub mod graphql;
pub mod http;
pub mod loader;

pub use date::*;
pub use graphql::*;
pub use http::*;
pub use loader::*;

use reflex::{
    compiler::Compile,
    core::{Applicable, Reducible, Rewritable, Signal},
    stdlib::Stdlib,
};
use reflex_runtime::{
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, SignalHandler,
    SignalHandlerResult, SignalHelpers, SignalResult,
};

pub mod utils {
    mod fetch;
    pub(crate) use fetch::fetch;
    pub(crate) mod graphql;
    pub mod signal_capture;
}

pub fn builtin_signal_handler<
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
>(
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
) -> impl SignalHandler<T>
where
    T::String: Send + Sync,
    T::Builtin: From<Stdlib>,
{
    compose_signal_handlers(
        date::create_date_timestamp_handler(factory, allocator),
        compose_signal_handlers(
            loader::create_loader_load_signal_handler(factory, allocator),
            compose_signal_handlers(
                http::create_http_fetch_signal_handler(factory, allocator),
                graphql::create_graphql_execute_signal_handler(factory, allocator),
            ),
        ),
    )
}

#[derive(Clone, Copy, Debug)]
pub struct NoopSignalHandler;
impl<T> SignalHandler<T> for NoopSignalHandler
where
    T: AsyncExpression + Compile<T>,
    T::String: Send + Sync,
{
    fn handle(
        &self,
        _signal_type: &str,
        _signals: &[&Signal<T>],
        _helpers: &SignalHelpers<T>,
    ) -> SignalHandlerResult<T> {
        None
    }
}

pub fn compose_signal_handlers<T>(
    head: impl SignalHandler<T>,
    tail: impl SignalHandler<T>,
) -> impl SignalHandler<T>
where
    T: AsyncExpression + Compile<T>,
    T::String: Send + Sync,
{
    move |signal_type: &str, signals: &[&Signal<T>], helpers: &SignalHelpers<T>| match head.handle(
        signal_type,
        signals,
        helpers,
    ) {
        Some(result) => Some(result),
        None => tail.handle(signal_type, signals, helpers),
    }
}

pub fn debug_signal_handler<T>(handler: impl SignalHandler<T>) -> impl SignalHandler<T>
where
    T: AsyncExpression + Compile<T>,
    T::String: Send + Sync,
{
    move |signal_type: &str, signals: &[&Signal<T>], helpers: &SignalHelpers<T>| {
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
            if args.is_empty() {
                eprintln!("  {}", signal.id(),);
            } else {
                eprintln!(
                    "  {}: {}",
                    signal.id(),
                    args.iter()
                        .map(|arg| format!("{}", arg))
                        .collect::<Vec<_>>()
                        .join(" ")
                )
            };
        }
        handler.handle(signal_type, signals, helpers)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum EitherHandler<L, R> {
    Left(L),
    Right(R),
}
impl<T, L, R> SignalHandler<T> for EitherHandler<L, R>
where
    T: AsyncExpression + Compile<T>,
    T::String: Send + Sync,
    L: SignalHandler<T>,
    R: SignalHandler<T>,
{
    fn handle(
        &self,
        signal_type: &str,
        signals: &[&Signal<T>],
        helpers: &SignalHelpers<T>,
    ) -> reflex_runtime::SignalHandlerResult<T> {
        match self {
            Self::Left(handler) => handler.handle(signal_type, signals, helpers),
            Self::Right(handler) => handler.handle(signal_type, signals, helpers),
        }
    }
}
