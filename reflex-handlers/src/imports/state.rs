// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{create_record, Builtin, Expression, ExpressionFactory, HeapAllocator};
use reflex_macros::blanket_trait;

blanket_trait!(
    pub trait StateImportBuiltin:
        Builtin
        + From<crate::stdlib::DecrementVariable>
        + From<crate::stdlib::GetVariable>
        + From<crate::stdlib::IncrementVariable>
        + From<crate::stdlib::Scan>
        + From<crate::stdlib::SetVariable>
    {
    }
);

pub fn import_state<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: StateImportBuiltin,
{
    create_record(
        [
            (
                factory.create_string_term(allocator.create_static_string("scan")),
                factory.create_builtin_term(crate::stdlib::Scan),
            ),
            (
                factory.create_string_term(allocator.create_static_string("get")),
                factory.create_builtin_term(crate::stdlib::GetVariable),
            ),
            (
                factory.create_string_term(allocator.create_static_string("set")),
                factory.create_builtin_term(crate::stdlib::SetVariable),
            ),
            (
                factory.create_string_term(allocator.create_static_string("increment")),
                factory.create_builtin_term(crate::stdlib::IncrementVariable),
            ),
            (
                factory.create_string_term(allocator.create_static_string("decrement")),
                factory.create_builtin_term(crate::stdlib::DecrementVariable),
            ),
            (
                factory.create_string_term(allocator.create_static_string("variable")),
                factory.create_lambda_term(
                    2,
                    factory.create_list_term(allocator.create_pair(
                        factory.create_application_term(
                            factory.create_builtin_term(crate::stdlib::GetVariable),
                            allocator.create_pair(
                                factory.create_variable_term(1),
                                factory.create_variable_term(0),
                            ),
                        ),
                        factory.create_lambda_term(
                            2,
                            factory.create_application_term(
                                factory.create_builtin_term(crate::stdlib::SetVariable),
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
