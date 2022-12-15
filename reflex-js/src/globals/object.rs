// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::stdlib::FromEntries;
use reflex::core::{create_record, Expression, ExpressionFactory, HeapAllocator};
use reflex_stdlib::{Entries, Keys, Values};

pub fn global_object<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Entries> + From<FromEntries> + From<Keys> + From<Values>,
{
    create_record(
        [
            (
                factory.create_string_term(allocator.create_static_string("entries")),
                factory.create_builtin_term(Entries),
            ),
            (
                factory.create_string_term(allocator.create_static_string("fromEntries")),
                factory.create_builtin_term(FromEntries),
            ),
            (
                factory.create_string_term(allocator.create_static_string("keys")),
                factory.create_builtin_term(Keys),
            ),
            (
                factory.create_string_term(allocator.create_static_string("values")),
                factory.create_builtin_term(Values),
            ),
        ],
        factory,
        allocator,
    )
}
