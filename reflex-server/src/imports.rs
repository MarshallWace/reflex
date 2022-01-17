// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::stdlib::Stdlib as ServerStdlib;
use reflex::core::{Expression, ExpressionFactory, HeapAllocator};

pub(crate) mod graphql;

pub use self::graphql::import_graphql;

pub fn server_imports<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Vec<(&'static str, T)>
where
    T::Builtin: From<ServerStdlib>,
{
    vec![("reflex::graphql", import_graphql(factory, allocator))]
}
