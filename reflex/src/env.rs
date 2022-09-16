// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    cache::SubstitutionCache,
    core::{
        create_record, ConditionType, Expression, ExpressionFactory, HeapAllocator, Reducible,
        Rewritable, SignalType, StateCache,
    },
};

const EVENT_TYPE_ENV: &'static str = "reflex::env";

pub fn create_env_args_accessor<T: Expression>(
    _factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T::Signal<T> {
    allocator.create_signal(
        SignalType::Custom(String::from(EVENT_TYPE_ENV)),
        allocator.create_empty_list(),
    )
}

pub fn inject_env_vars<T: Expression + Rewritable<T> + Reducible<T>>(
    expression: T,
    vars: impl IntoIterator<Item = (String, String)>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    let env = create_record(
        vars.into_iter().map(|(key, value)| {
            (
                factory.create_string_term(allocator.create_string(key)),
                factory.create_string_term(allocator.create_string(value)),
            )
        }),
        factory,
        allocator,
    );
    let mut state = StateCache::default();
    state.set(create_env_args_accessor(factory, allocator).id(), env);
    expression
        .substitute_dynamic(
            true,
            &state,
            factory,
            allocator,
            &mut SubstitutionCache::new(),
        )
        .unwrap_or(expression)
}
