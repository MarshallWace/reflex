// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{create_record, Expression, ExpressionFactory, HeapAllocator};
use reflex_stdlib::Stdlib;

pub fn global_math<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Stdlib>,
{
    create_record(
        vec![
            (
                factory.create_string_term(allocator.create_static_string("abs")),
                factory.create_builtin_term(Stdlib::Abs),
            ),
            (
                factory.create_string_term(allocator.create_static_string("ceil")),
                factory.create_builtin_term(Stdlib::Ceil),
            ),
            (
                factory.create_string_term(allocator.create_static_string("floor")),
                factory.create_builtin_term(Stdlib::Floor),
            ),
            (
                factory.create_string_term(allocator.create_static_string("max")),
                factory.create_builtin_term(Stdlib::Max),
            ),
            (
                factory.create_string_term(allocator.create_static_string("min")),
                factory.create_builtin_term(Stdlib::Min),
            ),
            (
                factory.create_string_term(allocator.create_static_string("round")),
                factory.create_builtin_term(Stdlib::Round),
            ),
        ],
        factory,
        allocator,
    )
}
