// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::once;

use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator},
    lang::create_record,
    stdlib::Stdlib,
};

use crate::actor::loader::EFFECT_TYPE_LOADER;

pub fn import_loader<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Stdlib>,
{
    create_record(
        once((
            String::from("default"),
            factory.create_lambda_term(
                2,
                create_record(
                    [
                        (
                            String::from("load"),
                            factory.create_lambda_term(
                                1,
                                factory.create_application_term(
                                    factory.create_builtin_term(Stdlib::Effect),
                                    allocator.create_list([
                                        factory.create_string_term(
                                            allocator.create_static_string(EFFECT_TYPE_LOADER),
                                        ),
                                        factory.create_static_variable_term(2),
                                        factory.create_static_variable_term(1),
                                        factory.create_static_variable_term(0),
                                    ]),
                                ),
                            ),
                        ),
                        (
                            String::from("loadMany"),
                            factory.create_lambda_term(
                                1,
                                factory.create_application_term(
                                    factory.create_builtin_term(Stdlib::Map),
                                    allocator.create_pair(
                                        factory.create_static_variable_term(0),
                                        factory.create_lambda_term(
                                            1,
                                            factory.create_application_term(
                                                factory.create_builtin_term(Stdlib::Effect),
                                                allocator.create_list([
                                                    factory.create_string_term(
                                                        allocator.create_static_string(
                                                            EFFECT_TYPE_LOADER,
                                                        ),
                                                    ),
                                                    factory.create_static_variable_term(3),
                                                    factory.create_static_variable_term(2),
                                                    factory.create_static_variable_term(0),
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
