// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::stdlib::Stdlib as GraphQlStdlib;
use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator},
    lang::create_struct,
};

pub fn graphql_imports<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Vec<(&'static str, T)>
where
    T::Builtin: From<GraphQlStdlib>,
{
    vec![(
        "reflex::graphql",
        create_struct(
            vec![(
                String::from("Resolver"),
                factory.create_builtin_term(GraphQlStdlib::GraphQlResolver),
            )],
            factory,
            allocator,
        ),
    )]
}
