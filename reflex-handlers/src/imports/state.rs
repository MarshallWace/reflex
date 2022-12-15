// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{create_record, Expression, ExpressionFactory, HeapAllocator};

use crate::stdlib::{DecrementVariable, GetVariable, IncrementVariable, Scan, SetVariable};

pub fn import_state<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<DecrementVariable>
        + From<GetVariable>
        + From<IncrementVariable>
        + From<Scan>
        + From<SetVariable>,
{
    create_record(
        [
            (
                factory.create_string_term(allocator.create_static_string("scan")),
                factory.create_builtin_term(Scan),
            ),
            (
                factory.create_string_term(allocator.create_static_string("get")),
                factory.create_builtin_term(GetVariable),
            ),
            (
                factory.create_string_term(allocator.create_static_string("set")),
                factory.create_builtin_term(SetVariable),
            ),
            (
                factory.create_string_term(allocator.create_static_string("increment")),
                factory.create_builtin_term(IncrementVariable),
            ),
            (
                factory.create_string_term(allocator.create_static_string("decrement")),
                factory.create_builtin_term(DecrementVariable),
            ),
            (
                factory.create_string_term(allocator.create_static_string("variable")),
                factory.create_lambda_term(
                    2,
                    factory.create_list_term(allocator.create_pair(
                        factory.create_application_term(
                            factory.create_builtin_term(GetVariable),
                            allocator.create_pair(
                                factory.create_variable_term(1),
                                factory.create_variable_term(0),
                            ),
                        ),
                        factory.create_lambda_term(
                            2,
                            factory.create_application_term(
                                factory.create_builtin_term(SetVariable),
                                allocator.create_triple(
                                    factory.create_variable_term(3),
                                    factory.create_variable_term(1),
                                    factory.create_variable_term(0),
                                ),
                            ),
                        ),
                    )),
                ),
            ),
        ],
        factory,
        allocator,
    )
}
