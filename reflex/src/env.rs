// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    cache::SubstitutionCache,
    core::{
        Expression, ExpressionFactory, HeapAllocator, Reducible, Rewritable, StateCache, StateToken,
    },
    lang::create_struct,
};

pub const ENV_STATE_TOKEN: StateToken = 0;

pub fn inject_env_vars<T: Expression + Rewritable<T> + Reducible<T>>(
    expression: T,
    vars: impl IntoIterator<Item = (String, String)>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    let env = create_struct(
        vars.into_iter().map(|(key, value)| {
            (
                key,
                factory.create_string_term(allocator.create_string(value)),
            )
        }),
        factory,
        allocator,
    );
    let mut state = StateCache::default();
    state.set(ENV_STATE_TOKEN, env);
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
