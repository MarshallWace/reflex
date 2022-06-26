// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator},
    stdlib::Stdlib,
};

pub fn global_boolean<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Stdlib>,
{
    factory.create_lambda_term(
        1,
        factory.create_application_term(
            factory.create_builtin_term(Stdlib::If),
            allocator.create_triple(
                factory.create_static_variable_term(0),
                factory.create_boolean_term(true),
                factory.create_boolean_term(false),
            ),
        ),
    )
}
