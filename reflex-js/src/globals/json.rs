// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{create_record, Expression, ExpressionFactory, HeapAllocator};
use reflex_json::stdlib::{JsonDeserialize, JsonSerialize};
use reflex_stdlib::ResolveDeep;

pub fn global_json<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<JsonDeserialize> + From<JsonSerialize> + From<ResolveDeep>,
{
    create_record(
        vec![
            (
                factory.create_string_term(allocator.create_static_string("parse")),
                factory.create_builtin_term(JsonDeserialize),
            ),
            (
                factory.create_string_term(allocator.create_static_string("stringify")),
                factory.create_lambda_term(
                    1,
                    factory.create_application_term(
                        factory.create_builtin_term(JsonSerialize),
                        allocator.create_unit_list(factory.create_application_term(
                            factory.create_builtin_term(ResolveDeep),
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
