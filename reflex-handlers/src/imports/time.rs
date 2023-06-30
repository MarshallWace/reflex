// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{create_record, Builtin, Expression, ExpressionFactory, HeapAllocator};
use reflex_macros::blanket_trait;
use reflex_stdlib::stdlib;

use crate::actor::{timeout::EFFECT_TYPE_TIMEOUT, timestamp::EFFECT_TYPE_TIMESTAMP};

blanket_trait!(
    pub trait TimeImportBuiltin:
        Builtin + From<stdlib::CollectList> + From<stdlib::Effect> + From<stdlib::Get>
    {
    }
);

pub fn import_time<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: TimeImportBuiltin,
{
    create_record(
        [
            (
                factory.create_string_term(allocator.create_static_string("timeout")),
                factory.create_lambda_term(
                    2,
                    factory.create_application_term(
                        factory.create_builtin_term(stdlib::Effect),
                        allocator.create_triple(
                            factory.create_string_term(
                                allocator.create_string(String::from(EFFECT_TYPE_TIMEOUT)),
                            ),
                            factory.create_application_term(
                                factory.create_builtin_term(stdlib::CollectList),
                                allocator.create_unit_list(factory.create_variable_term(1)),
                            ),
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
                        factory.create_builtin_term(stdlib::Effect),
                        allocator.create_triple(
                            factory.create_string_term(
                                allocator.create_static_string(EFFECT_TYPE_TIMESTAMP),
                            ),
                            factory.create_application_term(
                                factory.create_builtin_term(stdlib::CollectList),
                                allocator.create_unit_list(factory.create_application_term(
                                    factory.create_builtin_term(stdlib::Get),
                                    allocator.create_pair(
                                        factory.create_variable_term(0),
                                        factory.create_string_term(
                                            allocator.create_static_string("interval"),
                                        ),
                                    ),
                                )),
                            ),
                            factory.create_nil_term(),
                        ),
                    ),
                ),
            ),
        ],
        factory,
        allocator,
    )
}
