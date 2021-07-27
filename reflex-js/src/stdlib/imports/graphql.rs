// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator, VarArgs},
    lang::create_struct,
};

pub fn import_graphql<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    create_struct(
        vec![(
            String::from("Resolver"),
            import_graphql_resolver(factory, allocator),
        )],
        factory,
        allocator,
    )
}

fn import_graphql_resolver<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_constructor_term(
        allocator.create_struct_prototype(vec![
            String::from("query"),
            String::from("mutation"),
            String::from("subscription"),
        ]),
        VarArgs::Lazy,
    )
}
