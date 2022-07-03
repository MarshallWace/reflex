// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator},
    lang::create_record,
    stdlib::Stdlib,
};

use crate::actor::{timeout::EFFECT_TYPE_TIMEOUT, timestamp::EFFECT_TYPE_TIMESTAMP};

pub fn import_time<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Stdlib>,
{
    create_record(
        vec![
            (
                String::from("timeout"),
                factory.create_lambda_term(
                    2,
                    factory.create_application_term(
                        factory.create_builtin_term(Stdlib::Effect),
                        allocator.create_triple(
                            factory.create_string_term(
                                allocator.create_string(String::from(EFFECT_TYPE_TIMEOUT)),
                            ),
                            factory.create_variable_term(1),
                            factory.create_variable_term(0),
                        ),
                    ),
                ),
            ),
            (
                String::from("timestamp"),
                factory.create_lambda_term(
                    1,
                    factory.create_application_term(
                        factory.create_builtin_term(Stdlib::Effect),
                        allocator.create_pair(
                            factory.create_string_term(
                                allocator.create_static_string(EFFECT_TYPE_TIMESTAMP),
                            ),
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::Get),
                                allocator.create_pair(
                                    factory.create_variable_term(0),
                                    factory.create_string_term(
                                        allocator.create_static_string("interval"),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        ],
        factory,
        allocator,
    )
}
