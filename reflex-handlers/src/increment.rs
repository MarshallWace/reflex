// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::once;

use reflex::{
    compiler::Compile,
    core::{
        Expression, ExpressionFactory, ExpressionList, HeapAllocator, Reducible, Rewritable,
        Signal, SignalType, StateToken,
    },
    lang::ValueTerm,
};
use reflex_runtime::{
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, RuntimeEffect, SignalHandler,
    SignalHelpers, SignalResult,
};

pub const SIGNAL_TYPE_INCREMENT: &'static str = "reflex::increment";

pub fn increment_signal_handler<T: AsyncExpression + Rewritable<T> + Reducible<T> + Compile<T>>(
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
) -> impl SignalHandler<T>
where
    T::String: Send + Sync,
{
    let factory = factory.clone();
    let allocator = allocator.clone();
    move |signal_type: &str, signals: &[&Signal<T>], helpers: &SignalHelpers<T>| {
        if signal_type != SIGNAL_TYPE_INCREMENT {
            return None;
        }
        Some(
            signals
                .iter()
                .map(|signal| {
                    handle_increment_signal(
                        signal.id(),
                        signal.args(),
                        helpers,
                        &factory,
                        &allocator,
                    )
                })
                .collect(),
        )
    }
}

fn handle_increment_signal<T: AsyncExpression + Rewritable<T> + Reducible<T> + Compile<T>>(
    _signal_id: StateToken,
    args: &ExpressionList<T>,
    _helpers: &SignalHelpers<T>,
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
) -> Result<SignalResult<T>, String>
where
    T::String: Send + Sync,
{
    let mut args = args.into_iter();
    if args.len() != 2 {
        return Err(format!(
            "Invalid increment signal: Expected 2 arguments, received {}",
            args.len()
        ));
    }
    let state_token = args.next().unwrap();
    if let Some(ValueTerm::Hash(state_token)) = factory.match_value_term(state_token) {
        Ok((
            factory.create_dynamic_variable_term(
                *state_token,
                create_pending_expression(factory, allocator),
            ),
            Some(RuntimeEffect::update(*state_token, {
                let factory = factory.clone();
                let allocator = allocator.clone();
                move |value| increment_value(value, &factory, &allocator)
            })),
        ))
    } else {
        Err(format!(
            "Invalid increment signal identifier: Expected Hash, received {}",
            state_token
        ))
    }
}

fn increment_value<T: Expression>(
    existing: Option<&T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    match existing {
        None => factory.create_value_term(ValueTerm::Int(1)),
        Some(existing) => {
            let result = if let Some(value) = factory.match_value_term(existing) {
                if let Some(value) = value.match_int() {
                    Some(factory.create_value_term(ValueTerm::Int(value + 1)))
                } else if let Some(value) = value.match_float() {
                    Some(factory.create_value_term(ValueTerm::Float(value + 1.0)))
                } else {
                    None
                }
            } else {
                None
            };
            match result {
                Some(result) => result,
                None => create_error_expression(
                    format!("Unable to increment non-numeric value: {}", existing),
                    factory,
                    allocator,
                ),
            }
        }
    }
}

fn create_pending_expression<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_signal_term(allocator.create_signal_list(once(
        allocator.create_signal(SignalType::Pending, allocator.create_empty_list()),
    )))
}

fn create_error_expression<T: Expression>(
    message: String,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_signal_term(allocator.create_signal_list(once(allocator.create_signal(
        SignalType::Error,
        allocator.create_unit_list(
            factory.create_value_term(ValueTerm::String(allocator.create_string(message))),
        ),
    ))))
}
