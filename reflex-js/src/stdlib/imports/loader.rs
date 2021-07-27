// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::once;

use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator},
    lang::{create_struct, BuiltinTerm, ValueTerm},
};

pub(crate) const SIGNAL_TYPE_LOAD: &'static str = "reflex::loader::load";

pub fn import_loader<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
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
                                    factory.create_builtin_term(BuiltinTerm::Get),
                                    allocator.create_list(vec![
                                        create_load_expression(
                                            factory.create_static_variable_term(1),
                                            factory.create_application_term(
                                                factory.create_builtin_term(BuiltinTerm::ResolveDeep),
                                                allocator.create_unit_list(
                                                    factory.create_vector_term(allocator.create_unit_list(
                                                        factory.create_static_variable_term(0),
                                                    )),
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
                                        factory.create_builtin_term(BuiltinTerm::ResolveDeep),
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
) -> T {
    factory.create_application_term(
        factory.create_builtin_term(BuiltinTerm::Effect),
        allocator.create_list(
            once(factory.create_value_term(ValueTerm::String(
                allocator.create_string(String::from(SIGNAL_TYPE_LOAD)),
            )))
            .chain(once(loader))
            .chain(once(keys))
            .collect::<Vec<_>>(),
        ),
    )
}
