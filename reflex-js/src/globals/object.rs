// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::stdlib::Stdlib as JsStdlib;
use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator},
    lang::create_record,
};

pub fn global_object<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<JsStdlib>,
{
    create_record(
        vec![(
            String::from("fromEntries"),
            factory.create_builtin_term(JsStdlib::FromEntries),
        )],
        factory,
        allocator,
    )
}
