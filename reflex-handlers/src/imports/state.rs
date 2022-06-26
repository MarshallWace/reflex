// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator},
    lang::create_struct,
    stdlib::Stdlib,
};

use crate::{
    actor::{assign::EFFECT_TYPE_ASSIGN, increment::EFFECT_TYPE_INCREMENT},
    stdlib::Stdlib as HandlersStdlib,
};

pub fn import_state<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Stdlib> + From<HandlersStdlib>,
{
    create_struct(
        vec![
            (
                String::from("assign"),
                factory.create_lambda_term(
                    2,
                    factory.create_application_term(
                        factory.create_builtin_term(Stdlib::Effect),
                        allocator.create_triple(
                            factory.create_string_term(
                                allocator.create_string(String::from(EFFECT_TYPE_ASSIGN)),
                            ),
                            factory.create_static_variable_term(1),
                            factory.create_static_variable_term(0),
                        ),
                    ),
                ),
            ),
            (
                String::from("getter"),
                factory.create_builtin_term(HandlersStdlib::Getter),
            ),
            (
                String::from("increment"),
                factory.create_lambda_term(
                    2,
                    factory.create_application_term(
                        factory.create_builtin_term(Stdlib::Effect),
                        allocator.create_triple(
                            factory.create_string_term(
                                allocator.create_string(String::from(EFFECT_TYPE_INCREMENT)),
                            ),
                            factory.create_static_variable_term(1),
                            factory.create_static_variable_term(0),
                        ),
                    ),
                ),
            ),
            (
                String::from("scan"),
                factory.create_builtin_term(HandlersStdlib::Scan),
            ),
            (
                String::from("setter"),
                factory.create_builtin_term(HandlersStdlib::Setter),
            ),
            (
                String::from("variable"),
                factory.create_builtin_term(HandlersStdlib::Variable),
            ),
        ],
        factory,
        allocator,
    )
}
