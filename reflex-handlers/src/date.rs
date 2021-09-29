// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use futures_util::future::{AbortHandle, Abortable};
use reflex::{
    core::{Expression, ExpressionFactory, ExpressionList, Signal, StateToken},
    lang::ValueTerm,
};
use reflex_runtime::{
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, RuntimeEffect,
    SignalHandlerResult, SignalHelpers, StateUpdate,
};
use tokio::time::{interval_at, Instant};
use tokio_stream::{wrappers::IntervalStream, StreamExt};

use crate::SignalResult;

pub fn date_timestamp_handler<T: AsyncExpression>(
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
) -> impl Fn(&str, &[&Signal<T>], &SignalHelpers<T>) -> SignalHandlerResult<T> {
    let factory = factory.clone();
    let allocator = allocator.clone();
    move |signal_type: &str, signals: &[&Signal<T>], _helpers: &SignalHelpers<T>| {
        if signal_type != "reflex::date::timestamp" {
            return None;
        }
        Some(
            signals
                .iter()
                .map(|signal| {
                    handle_date_timestamp_signal(signal.id(), signal.args(), &factory, &allocator)
                })
                .collect(),
        )
    }
}

fn handle_date_timestamp_signal<T: AsyncExpression>(
    signal_id: StateToken,
    args: &ExpressionList<T>,
    factory: &impl AsyncExpressionFactory<T>,
    _allocator: &impl AsyncHeapAllocator<T>,
) -> Result<SignalResult<T>, String> {
    let mut args = args.into_iter();
    if args.len() != 1 {
        return Err(format!(
            "Invalid timestamp signal: Expected 1 argument, received {}",
            args.len()
        ));
    }
    let interval = parse_number_arg(args.next().unwrap(), factory);
    match interval {
        Some(duration) if duration >= 1.0 => {
            let period = Duration::from_millis(duration as u64);
            let first_update = Instant::now()
                .checked_add(period)
                .unwrap_or_else(|| Instant::now());
            let (stream, dispose) = {
                let stream = IntervalStream::new(interval_at(first_update, period));
                let (abort_handle, abort_registration) = AbortHandle::new_pair();
                let stream = Abortable::new(stream, abort_registration);
                let dispose = async move {
                    abort_handle.abort();
                };
                (stream, dispose)
            };
            Ok((
                factory.create_value_term(ValueTerm::Float(get_current_time())),
                Some(RuntimeEffect::stream(
                    {
                        let factory = factory.clone();
                        stream.map(move |_| {
                            StateUpdate::value(
                                signal_id,
                                factory.create_value_term(ValueTerm::Float(get_current_time())),
                            )
                        })
                    },
                    dispose,
                )),
            ))
        }
        _ => Err(String::from("Invalid timestamp signal arguments")),
    }
}

fn get_current_time() -> f64 {
    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or(Duration::from_secs(0))
        .as_secs_f64();
    (timestamp * 1000.0).floor()
}

fn parse_number_arg<T: Expression>(value: &T, factory: &impl ExpressionFactory<T>) -> Option<f64> {
    match factory.match_value_term(value) {
        Some(ValueTerm::Int(value)) => Some(*value as f64),
        Some(ValueTerm::Float(value)) => Some(*value),
        _ => None,
    }
}
