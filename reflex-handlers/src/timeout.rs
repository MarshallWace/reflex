// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::time::Duration;

use futures::FutureExt;
use futures_util::future::{AbortHandle, Abortable};
use reflex::{
    compiler::Compile,
    core::{Expression, ExpressionFactory, ExpressionList, Signal, StateToken},
    lang::ValueTerm,
};
use reflex_runtime::{
    create_pending_expression, AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator,
    RuntimeEffect, SignalHandler, SignalHelpers, SignalResult, StateUpdate,
};
use tokio::time::sleep;

pub const SIGNAL_TYPE_TIMEOUT: &str = "reflex::timeout";

pub fn create_timeout_signal_handler<T: AsyncExpression + Compile<T>>(
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
) -> impl SignalHandler<T>
where
    T::String: Send + Sync,
{
    let factory = factory.clone();
    let allocator = allocator.clone();
    move |signal_type: &str, signals: &[&Signal<T>], _helpers: &SignalHelpers<T>| {
        if signal_type != SIGNAL_TYPE_TIMEOUT {
            return None;
        }
        Some(
            signals
                .iter()
                .map(|signal| {
                    handle_timeout_signal(signal.id(), signal.args(), &factory, &allocator)
                })
                .collect(),
        )
    }
}

fn handle_timeout_signal<T: AsyncExpression>(
    signal_id: StateToken,
    args: &ExpressionList<T>,
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
) -> Result<SignalResult<T>, String> {
    let mut args = args.into_iter();
    if args.len() != 2 {
        return Err(format!(
            "Invalid timeout signal: Expected 2 arguments, received {}",
            args.len()
        ));
    }
    let duration = args.next().unwrap();
    let _token = args.next().unwrap();
    match parse_number_arg(duration, factory) {
        Some(duration) if duration == 0.0 => Ok((factory.create_value_term(ValueTerm::Null), None)),
        Some(duration) => {
            let duration = Duration::from_millis(duration as u64);
            let (task, dispose) = {
                let timeout = sleep(duration);
                let (abort_handle, abort_registration) = AbortHandle::new_pair();
                let task = Abortable::new(timeout, abort_registration);
                let dispose = async move {
                    abort_handle.abort();
                };
                (task, dispose)
            };
            Ok((
                create_pending_expression(factory, allocator),
                Some(RuntimeEffect::deferred(
                    {
                        let factory = factory.clone();
                        task.map(move |_| {
                            StateUpdate::value(
                                signal_id,
                                factory.create_value_term(ValueTerm::Null),
                            )
                        })
                    },
                    dispose,
                )),
            ))
        }
        _ => Err(format!("Invalid timeout signal duration: {}", duration)),
    }
}

fn parse_number_arg<T: Expression>(value: &T, factory: &impl ExpressionFactory<T>) -> Option<f64> {
    match factory.match_value_term(value) {
        Some(ValueTerm::Int(value)) => Some(*value as f64),
        Some(ValueTerm::Float(value)) => Some(*value),
        _ => None,
    }
}
