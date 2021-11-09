// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::once;

use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator},
    lang::{create_struct, ValueTerm},
    stdlib::Stdlib,
};

pub(crate) const SIGNAL_TYPE_LOAD: &'static str = "reflex::loader::load";

pub fn import_loader<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Stdlib>,
{
    create_struct(
        once((
            String::from("default"),
            factory.create_lambda_term(
                1,
                create_struct(
                    vec![
                        (
                            String::from("load"),
                            factory.create_lambda_term(1, {
                                factory.create_application_term(
                                    factory.create_builtin_term(Stdlib::Get),
                                    allocator.create_list(vec![
                                        create_load_expression(
                                            factory.create_static_variable_term(1),
                                            factory.create_application_term(
                                                factory.create_builtin_term(Stdlib::ResolveDeep),
                                                allocator.create_unit_list(
                                                    factory.create_vector_term(
                                                        allocator.create_unit_list(
                                                            factory.create_static_variable_term(0),
                                                        ),
                                                    ),
                                                ),
                                            ),
                                            factory,
                                            allocator,
                                        ),
                                        factory.create_value_term(ValueTerm::Int(0)),
                                    ]),
                                )
                            }),
                        ),
                        (
                            String::from("loadMany"),
                            factory.create_lambda_term(
                                1,
                                create_load_expression(
                                    factory.create_static_variable_term(1),
                                    factory.create_application_term(
                                        factory.create_builtin_term(Stdlib::ResolveDeep),
                                        allocator.create_unit_list(
                                            factory.create_static_variable_term(0),
                                        ),
                                    ),
                                    factory,
                                    allocator,
                                ),
                            ),
                        ),
                    ],
                    factory,
                    allocator,
                ),
            ),
        )),
        factory,
        allocator,
    )
}

fn create_load_expression<T: Expression>(
    loader: T,
    keys: T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Stdlib>,
{
    factory.create_application_term(
        factory.create_builtin_term(Stdlib::Effect),
        allocator.create_list(
            once(factory.create_value_term(ValueTerm::String(
                allocator.create_static_string(SIGNAL_TYPE_LOAD),
            )))
            .chain(once(factory.create_partial_application_term(
                factory.create_lambda_term(
                    2,
                    factory.create_application_term(
                        factory.create_builtin_term(Stdlib::ResolveShallow),
                        allocator.create_unit_list(factory.create_application_term(
                            factory.create_static_variable_term(1),
                            allocator.create_unit_list(factory.create_static_variable_term(0)),
                        )),
                    ),
                ),
                allocator.create_unit_list(loader),
            )))
            .chain(once(keys))
            .collect::<Vec<_>>(),
        ),
    )
}
