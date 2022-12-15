// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::once;

use reflex::core::{create_record, Expression, ExpressionFactory, HeapAllocator};
use reflex_stdlib::{Effect, Map};

use crate::actor::loader::EFFECT_TYPE_LOADER;

pub fn import_loader<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Effect> + From<Map>,
{
    create_record(
        once((
            factory.create_string_term(allocator.create_static_string("default")),
            factory.create_lambda_term(
                2,
                create_record(
                    [
                        (
                            factory.create_string_term(allocator.create_static_string("load")),
                            factory.create_lambda_term(
                                1,
                                factory.create_application_term(
                                    factory.create_builtin_term(Effect),
                                    allocator.create_list([
                                        factory.create_string_term(
                                            allocator.create_static_string(EFFECT_TYPE_LOADER),
                                        ),
                                        factory.create_variable_term(2),
                                        factory.create_variable_term(1),
                                        factory.create_variable_term(0),
                                    ]),
                                ),
                            ),
                        ),
                        (
                            factory.create_string_term(allocator.create_static_string("loadMany")),
                            factory.create_lambda_term(
                                1,
                                factory.create_application_term(
                                    factory.create_builtin_term(Map),
                                    allocator.create_pair(
                                        factory.create_variable_term(0),
                                        factory.create_lambda_term(
                                            1,
                                            factory.create_application_term(
                                                factory.create_builtin_term(Effect),
                                                allocator.create_list([
                                                    factory.create_string_term(
                                                        allocator.create_static_string(
                                                            EFFECT_TYPE_LOADER,
                                                        ),
                                                    ),
                                                    factory.create_variable_term(3),
                                                    factory.create_variable_term(2),
                                                    factory.create_variable_term(0),
                                                ]),
                                            ),
                                        ),
                                    ),
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
