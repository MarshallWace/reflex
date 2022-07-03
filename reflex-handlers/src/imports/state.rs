// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator},
    lang::create_record,
    stdlib::Stdlib,
};

use crate::stdlib::Stdlib as HandlersStdlib;

pub fn import_state<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Stdlib> + From<HandlersStdlib>,
{
    create_record(
        vec![
            (
                String::from("scan"),
                factory.create_builtin_term(HandlersStdlib::Scan),
            ),
            (
                String::from("get"),
                factory.create_builtin_term(HandlersStdlib::GetVariable),
            ),
            (
                String::from("set"),
                factory.create_builtin_term(HandlersStdlib::SetVariable),
            ),
            (
                String::from("increment"),
                factory.create_builtin_term(HandlersStdlib::IncrementVariable),
            ),
            (
                String::from("decrement"),
                factory.create_builtin_term(HandlersStdlib::DecrementVariable),
            ),
            (
                String::from("variable"),
                factory.create_lambda_term(
                    2,
                    factory.create_list_term(allocator.create_pair(
                        factory.create_application_term(
                            factory.create_builtin_term(HandlersStdlib::GetVariable),
                            allocator.create_pair(
                                factory.create_variable_term(1),
                                factory.create_variable_term(0),
                            ),
                        ),
                        factory.create_lambda_term(
                            2,
                            factory.create_application_term(
                                factory.create_builtin_term(HandlersStdlib::SetVariable),
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
