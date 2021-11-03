// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator},
    lang::create_struct,
    stdlib::Stdlib,
};

pub fn global_math<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Stdlib>,
{
    create_struct(
        vec![
            (
                String::from("abs"),
                factory.create_builtin_term(Stdlib::Abs),
            ),
            (
                String::from("ceil"),
                factory.create_builtin_term(Stdlib::Ceil),
            ),
            (
                String::from("floor"),
                factory.create_builtin_term(Stdlib::Floor),
            ),
            (
                String::from("max"),
                factory.create_builtin_term(Stdlib::Max),
            ),
            (
                String::from("min"),
                factory.create_builtin_term(Stdlib::Min),
            ),
            (
                String::from("round"),
                factory.create_builtin_term(Stdlib::Round),
            ),
        ],
        factory,
        allocator,
    )
}
