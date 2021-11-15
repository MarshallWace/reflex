// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{iter::once, sync::Mutex};

use futures_util::{future, StreamExt};
use reflex::{
    cache::SubstitutionCache,
    compiler::Compile,
    core::{
        Applicable, Expression, ExpressionFactory, ExpressionList, HeapAllocator, Reducible,
        Rewritable, Signal, SignalType, StateToken,
    },
};
use reflex_runtime::{
    create_pending_expression, AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator,
    RuntimeEffect, SignalHandler, SignalHelpers, SignalResult, StateUpdate,
};

pub const SIGNAL_TYPE_SCAN: &str = "reflex::scan";

pub fn create_scan_handler<
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
>(
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
) -> impl SignalHandler<T>
where
    T::String: Send + Sync,
{
    let factory = factory.clone();
    let allocator = allocator.clone();
    move |signal_type: &str, signals: &[&Signal<T>], helpers: &SignalHelpers<T>| {
        if signal_type != SIGNAL_TYPE_SCAN {
            return None;
        }
        Some(
            signals
                .iter()
                .map(|signal| {
                    handle_scan_signal(signal.id(), signal.args(), helpers, &factory, &allocator)
                })
                .collect(),
        )
    }
}

fn handle_scan_signal<
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
>(
    signal_id: StateToken,
    args: &ExpressionList<T>,
    helpers: &SignalHelpers<T>,
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
) -> Result<SignalResult<T>, String>
where
    T::String: Send + Sync,
{
    if args.len() != 3 {
        return Err(format!(
            "Invalid scan signal: Expected 3 arguments, received {}",
            args.len()
        ));
    }
    let mut args = args.into_iter();
    let target = args.next().unwrap();
    let seed = args.next().unwrap();
    let iteratee = args.next().unwrap();
    let factory = factory.clone();
    let allocator = allocator.clone();
    let target = target.clone();
    let seed = seed.clone();
    let iteratee = iteratee.clone();
    let helpers = helpers.clone();
    let dispose = future::ready(());
    Ok((
        create_pending_expression(&factory, &allocator),
        Some(RuntimeEffect::stream(
            Box::pin({
                // TODO: Garbage-collect cache used for scan iteratee
                let mut cache = Mutex::new(SubstitutionCache::new());
                helpers
                    .watch_expression(target, once(signal_id), &factory, &allocator)?
                    .filter_map({
                        let factory = factory.clone();
                        let allocator = allocator.clone();
                        move |value| {
                            let result = match filter_pending_signals(value, &factory, &allocator) {
                                Ok(value) => Some(value),
                                Err(_) => None,
                            };
                            async { result }
                        }
                    })
                    .scan(seed, {
                        let factory = factory.clone();
                        let allocator = allocator.clone();
                        move |state, value| {
                            let result = {
                                let result = factory.create_application_term(
                                    iteratee.clone(),
                                    allocator.create_pair(state.clone(), value),
                                );
                                let result = result
                                    .reduce(&factory, &allocator, cache.get_mut().unwrap())
                                    .unwrap_or(result);
                                *state = result.clone();
                                result
                            };
                            let result = StateUpdate::value(signal_id, result);
                            async { Some(result) }
                        }
                    })
            }),
            dispose,
        )),
    ))
}

fn filter_pending_signals<T: Expression>(
    expression: T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, T> {
    match factory.match_signal_term(&expression) {
        Some(signal) if signal.signals().into_iter().any(is_pending_signal) => {
            let signals = signal
                .signals()
                .into_iter()
                .filter(|signal| !signal.is_type(&SignalType::Pending))
                .map(|signal| allocator.clone_signal(signal))
                .collect::<Vec<_>>();
            if signals.is_empty() {
                Err(expression)
            } else {
                Ok(factory.create_signal_term(allocator.create_signal_list(signals)))
            }
        }
        _ => Ok(expression),
    }
}

fn is_pending_signal<T: Expression>(signal: &Signal<T>) -> bool {
    signal.is_type(&SignalType::Pending)
}
