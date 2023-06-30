// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{create_record, Expression, ExpressionFactory, HeapAllocator};

use crate::stdlib::GraphQlResolver;

pub fn import_graphql<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<GraphQlResolver>,
{
    create_record(
        vec![(
            factory.create_string_term(allocator.create_static_string("Resolver")),
            factory.create_builtin_term(GraphQlResolver),
        )],
        factory,
        allocator,
    )
}
