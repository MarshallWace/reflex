// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{create_record, Expression, ExpressionFactory, HeapAllocator};
use reflex_stdlib::{Abs, Ceil, Floor, Max, Min, Round};

pub fn global_math<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Abs> + From<Ceil> + From<Floor> + From<Max> + From<Min> + From<Round>,
{
    create_record(
        vec![
            (
                factory.create_string_term(allocator.create_static_string("abs")),
                factory.create_builtin_term(Abs),
            ),
            (
                factory.create_string_term(allocator.create_static_string("ceil")),
                factory.create_builtin_term(Ceil),
            ),
            (
                factory.create_string_term(allocator.create_static_string("floor")),
                factory.create_builtin_term(Floor),
            ),
            (
                factory.create_string_term(allocator.create_static_string("max")),
                factory.create_builtin_term(Max),
            ),
            (
                factory.create_string_term(allocator.create_static_string("min")),
                factory.create_builtin_term(Min),
            ),
            (
                factory.create_string_term(allocator.create_static_string("round")),
                factory.create_builtin_term(Round),
            ),
        ],
        factory,
        allocator,
    )
}
