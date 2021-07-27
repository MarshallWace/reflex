// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator},
    lang::{create_struct, BuiltinTerm},
};

pub fn global_math<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    create_struct(
        vec![
            (
                String::from("abs"),
                factory.create_builtin_term(BuiltinTerm::Abs),
            ),
            (
                String::from("ceil"),
                factory.create_builtin_term(BuiltinTerm::Ceil),
            ),
            (
                String::from("floor"),
                factory.create_builtin_term(BuiltinTerm::Floor),
            ),
            (
                String::from("max"),
                factory.create_builtin_term(BuiltinTerm::Max),
            ),
            (
                String::from("min"),
                factory.create_builtin_term(BuiltinTerm::Min),
            ),
            (
                String::from("round"),
                factory.create_builtin_term(BuiltinTerm::Round),
            ),
        ],
        factory,
        allocator,
    )
}
