// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::{
    compiler::Compile,
    core::{
        ExpressionFactory, ExpressionList, HeapAllocator, Reducible, Rewritable, Signal, StateToken,
    },
    lang::ValueTerm,
};
use reflex_runtime::{
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, RuntimeEffect, SignalHandler,
    SignalHelpers, SignalResult,
};

pub const SIGNAL_TYPE_ASSIGN: &'static str = "reflex::assign";

pub fn create_assign_signal_handler<
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Compile<T>,
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
        if signal_type != SIGNAL_TYPE_ASSIGN {
            return None;
        }
        Some(
            signals
                .iter()
                .map(|signal| {
                    handle_assign_signal(signal.id(), signal.args(), helpers, &factory, &allocator)
                })
                .collect(),
        )
    }
}

fn handle_assign_signal<T: AsyncExpression + Rewritable<T> + Reducible<T> + Compile<T>>(
    _signal_id: StateToken,
    args: &ExpressionList<T>,
    _helpers: &SignalHelpers<T>,
    factory: &impl ExpressionFactory<T>,
    _allocator: &impl HeapAllocator<T>,
) -> Result<SignalResult<T>, String>
where
    T::String: Send + Sync,
{
    let mut args = args.into_iter();
    if args.len() != 2 {
        return Err(format!(
            "Invalid assign signal: Expected 2 arguments, received {}",
            args.len()
        ));
    }
    let state_token = args.next().unwrap();
    let value = args.next().unwrap();
    if let Some(ValueTerm::Hash(state_token)) = factory.match_value_term(state_token) {
        Ok((
            value.clone(),
            Some(RuntimeEffect::assign(*state_token, value.clone())),
        ))
    } else {
        Err(format!(
            "Invalid assign signal identifier: Expected Hash, received {}",
            state_token
        ))
    }
}
