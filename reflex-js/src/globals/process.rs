// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator},
    env::create_env_args_accessor,
    lang::create_record,
};

pub fn global_process<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    create_record(
        vec![(
            String::from("env"),
            factory.create_effect_term(create_env_args_accessor(factory, allocator)),
        )],
        factory,
        allocator,
    )
}
