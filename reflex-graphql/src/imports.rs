// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{Builtin, Expression, ExpressionFactory, HeapAllocator};

use crate::stdlib::GraphQlResolver;

pub mod graphql;

pub use self::graphql::import_graphql;

pub trait GraphQlImportsBuiltin: Builtin + From<GraphQlResolver> {}
impl<T> GraphQlImportsBuiltin for T where T: Builtin + From<GraphQlResolver> {}

pub fn graphql_imports<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Vec<(&'static str, T)>
where
    T::Builtin: GraphQlImportsBuiltin,
{
    vec![("reflex::graphql", import_graphql(factory, allocator))]
}
