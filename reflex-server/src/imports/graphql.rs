// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::stdlib::Stdlib as ServerStdlib;
use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator},
    lang::create_record,
};

pub fn import_graphql<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<ServerStdlib>,
{
    create_record(
        vec![(
            String::from("Resolver"),
            factory.create_builtin_term(ServerStdlib::GraphQlResolver),
        )],
        factory,
        allocator,
    )
}
