// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use reflex::{
    core::{Expression, Signal, StateToken, Term},
    stdlib::value::ValueTerm,
};
use reflex_runtime::{RuntimeEffect, SignalHandlerResult, SignalHelpers, StateUpdate};
use tokio::time::{interval_at, Instant};
use tokio_stream::{wrappers::IntervalStream, StreamExt};

use crate::SignalResult;

pub fn date_timestamp_handler(
    signal_type: &str,
    signals: &[&Signal],
    _helpers: &SignalHelpers,
) -> SignalHandlerResult {
    if signal_type != "reflex::date::timestamp" {
        return None;
    }
    Some(
        signals
            .iter()
            .map(|signal| handle_date_timestamp_signal(signal.id(), signal.args()))
            .collect(),
    )
}

fn handle_date_timestamp_signal(
    signal_id: StateToken,
    args: &[Expression],
) -> Result<SignalResult, String> {
    if args.len() != 1 {
        return Err(format!(
            "Invalid timestamp signal: Expected 1 argument, received {}",
            args.len()
        ));
    }
    let mut args = args.into_iter();
    let interval = parse_number_arg(args.next().unwrap());
    match interval {
        Some(duration) if duration >= 1.0 => {
            let period = Duration::from_millis(duration as u64);
            let first_update = Instant::now()
                .checked_add(period)
                .unwrap_or_else(|| Instant::now());
            Ok((
                Expression::new(Term::Value(ValueTerm::Float(get_current_time()))),
                Some(RuntimeEffect::stream(Box::pin(
                    IntervalStream::new(interval_at(first_update, period)).map(move |_| {
                        StateUpdate::value(
                            signal_id,
                            Expression::new(Term::Value(ValueTerm::Float(get_current_time()))),
                        )
                    }),
                ))),
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

fn parse_number_arg(value: &Expression) -> Option<f64> {
    match value.value() {
        Term::Value(value) => match value {
            ValueTerm::Int(value) => Some(*value as f64),
            ValueTerm::Float(value) => Some(*value),
            _ => None,
        },
        _ => None,
    }
}
