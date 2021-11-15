// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator},
    lang::{create_struct, ValueTerm},
    stdlib::Stdlib,
};
use reflex_handlers::SIGNAL_TYPE_DATE_TIMESTAMP;

pub fn import_date<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Stdlib>,
{
    create_struct(
        vec![(
            String::from("timestamp"),
            factory.create_lambda_term(
                1,
                factory.create_application_term(
                    factory.create_builtin_term(Stdlib::Effect),
                    allocator.create_pair(
                        factory.create_value_term(ValueTerm::String(
                            allocator.create_static_string(SIGNAL_TYPE_DATE_TIMESTAMP),
                        )),
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Get),
                            allocator.create_pair(
                                factory.create_static_variable_term(0),
                                factory.create_value_term(ValueTerm::String(
                                    allocator.create_static_string("interval"),
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
