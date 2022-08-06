// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::{
    core::{create_record, Expression, ExpressionFactory, HeapAllocator},
    env::create_env_args_accessor,
};

pub fn global_process<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    create_record(
        vec![(
            factory.create_string_term(allocator.create_static_string("env")),
            factory.create_effect_term(create_env_args_accessor(factory, allocator)),
        )],
        factory,
        allocator,
    )
}
