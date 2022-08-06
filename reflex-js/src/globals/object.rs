// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::stdlib::Stdlib as JsStdlib;
use reflex::core::{create_record, Expression, ExpressionFactory, HeapAllocator};
use reflex_stdlib::Stdlib;

pub fn global_object<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    create_record(
        [
            (
                factory.create_string_term(allocator.create_static_string("entries")),
                factory.create_builtin_term(Stdlib::Entries),
            ),
            (
                factory.create_string_term(allocator.create_static_string("fromEntries")),
                factory.create_builtin_term(JsStdlib::FromEntries),
            ),
            (
                factory.create_string_term(allocator.create_static_string("keys")),
                factory.create_builtin_term(Stdlib::Keys),
            ),
            (
                factory.create_string_term(allocator.create_static_string("values")),
                factory.create_builtin_term(Stdlib::Values),
            ),
        ],
        factory,
        allocator,
    )
}
