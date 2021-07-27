// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator},
    lang::{create_struct, BuiltinTerm, ValueTerm},
};

pub fn import_date<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    create_struct(
        vec![(
            String::from("timestamp"),
            factory.create_lambda_term(
                1,
                factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::Effect),
                    allocator.create_pair(
                        factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("reflex::date::timestamp")),
                        )),
                        factory.create_application_term(
                            factory.create_builtin_term(BuiltinTerm::Get),
                            allocator.create_pair(
                                factory.create_static_variable_term(0),
                                factory.create_value_term(ValueTerm::String(
                                    allocator.create_string(String::from("interval")),
                                )),
                            ),
                        ),
                    ),
                ),
            ),
        )],
        factory,
        allocator,
    )
}
