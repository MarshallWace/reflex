// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::stdlib::Stdlib as JsStdlib;
use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator},
    lang::create_record,
    stdlib::Stdlib,
};

pub fn global_object<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    create_record(
        vec![
            (
                String::from("entries"),
                factory.create_builtin_term(Stdlib::Entries),
            ),
            (
                String::from("fromEntries"),
                factory.create_builtin_term(JsStdlib::FromEntries),
            ),
            (
                String::from("keys"),
                factory.create_builtin_term(Stdlib::Keys),
            ),
            (
                String::from("values"),
                factory.create_builtin_term(Stdlib::Values),
            ),
        ],
        factory,
        allocator,
    )
}
