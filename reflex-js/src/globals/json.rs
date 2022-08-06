// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{create_record, Expression, ExpressionFactory, HeapAllocator};
use reflex_json::stdlib::Stdlib as JsonStdlib;
use reflex_stdlib::Stdlib;

pub fn global_json<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Stdlib> + From<JsonStdlib>,
{
    create_record(
        vec![
            (
                factory.create_string_term(allocator.create_static_string("parse")),
                factory.create_builtin_term(JsonStdlib::JsonDeserialize),
            ),
            (
                factory.create_string_term(allocator.create_static_string("stringify")),
                factory.create_lambda_term(
                    1,
                    factory.create_application_term(
                        factory.create_builtin_term(JsonStdlib::JsonSerialize),
                        allocator.create_unit_list(factory.create_application_term(
                            factory.create_builtin_term(Stdlib::ResolveDeep),
                            allocator.create_unit_list(factory.create_variable_term(0)),
                        )),
                    ),
                ),
            ),
        ],
        factory,
        allocator,
    )
}
