// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::once;

use reflex::core::{create_record, Expression, ExpressionFactory, HeapAllocator, SignalType};
use types::import_types;

use crate::stdlib::{Hash, Log, StructTypeFactory};

mod types;

pub fn import_utils<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Hash> + From<Log> + From<StructTypeFactory>,
{
    create_record(
        vec![
            (
                factory.create_string_term(allocator.create_static_string("graph")),
                factory.create_lambda_term(
                    1,
                    factory.create_recursive_term(factory.create_variable_term(0)),
                ),
            ),
            (
                factory.create_string_term(allocator.create_static_string("hash")),
                factory.create_builtin_term(Hash),
            ),
            (
                factory.create_string_term(allocator.create_static_string("log")),
                factory.create_builtin_term(Log),
            ),
            (
                factory.create_string_term(allocator.create_static_string("pending")),
                factory.create_lambda_term(
                    0,
                    factory.create_signal_term(allocator.create_signal_list(once(
                        allocator.create_signal(SignalType::Pending, allocator.create_empty_list()),
                    ))),
                ),
            ),
            (
                factory.create_string_term(allocator.create_static_string("Types")),
                import_types(factory, allocator),
            ),
        ],
        factory,
        allocator,
    )
}
