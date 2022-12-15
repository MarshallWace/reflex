// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{create_record, Expression, ExpressionFactory, HeapAllocator};
use reflex_stdlib::{Effect, Get};

use crate::actor::{timeout::EFFECT_TYPE_TIMEOUT, timestamp::EFFECT_TYPE_TIMESTAMP};

pub fn import_time<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Effect> + From<Get>,
{
    create_record(
        [
            (
                factory.create_string_term(allocator.create_static_string("timeout")),
                factory.create_lambda_term(
                    2,
                    factory.create_application_term(
                        factory.create_builtin_term(Effect),
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
                factory.create_string_term(allocator.create_static_string("timestamp")),
                factory.create_lambda_term(
                    1,
                    factory.create_application_term(
                        factory.create_builtin_term(Effect),
                        allocator.create_pair(
                            factory.create_string_term(
                                allocator.create_static_string(EFFECT_TYPE_TIMESTAMP),
                            ),
                            factory.create_application_term(
                                factory.create_builtin_term(Get),
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
