// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::once;

use reflex::core::{create_record, Builtin, Expression, ExpressionFactory, HeapAllocator};
use reflex_macros::blanket_trait;
use reflex_stdlib::stdlib;

use crate::actor::loader::EFFECT_TYPE_LOADER;

blanket_trait!(
    pub trait LoaderImportBuiltin:
        Builtin + From<stdlib::CollectList> + From<stdlib::Effect> + From<stdlib::Map>
    {
    }
);

pub fn import_loader<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: LoaderImportBuiltin,
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
                                    factory.create_builtin_term(stdlib::Effect),
                                    allocator.create_triple(
                                        factory.create_string_term(
                                            allocator.create_static_string(EFFECT_TYPE_LOADER),
                                        ),
                                        factory.create_application_term(
                                            factory.create_builtin_term(stdlib::CollectList),
                                            allocator.create_triple(
                                                factory.create_variable_term(2),
                                                factory.create_variable_term(1),
                                                factory.create_variable_term(0),
                                            ),
                                        ),
                                        factory.create_nil_term(),
                                    ),
                                ),
                            ),
                        ),
                        (
                            factory.create_string_term(allocator.create_static_string("loadMany")),
                            factory.create_lambda_term(
                                1,
                                factory.create_application_term(
                                    factory.create_builtin_term(stdlib::Map),
                                    allocator.create_pair(
                                        factory.create_variable_term(0),
                                        factory.create_lambda_term(
                                            1,
                                            factory.create_application_term(
                                                factory.create_builtin_term(stdlib::Effect),
                                                allocator.create_triple(
                                                    factory.create_string_term(
                                                        allocator.create_static_string(
                                                            EFFECT_TYPE_LOADER,
                                                        ),
                                                    ),
                                                    factory.create_application_term(
                                                        factory.create_builtin_term(
                                                            stdlib::CollectList,
                                                        ),
                                                        allocator.create_triple(
                                                            factory.create_variable_term(3),
                                                            factory.create_variable_term(2),
                                                            factory.create_variable_term(0),
                                                        ),
                                                    ),
                                                    factory.create_nil_term(),
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
                ),
            ),
        )),
        factory,
        allocator,
    )
}
