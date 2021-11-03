// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator},
    lang::create_struct,
};

pub fn global_process<T: Expression>(
    env_vars: impl IntoIterator<Item = (String, T)>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    create_struct(
        vec![(
            String::from("env"),
            create_struct(env_vars, factory, allocator),
        )],
        factory,
        allocator,
    )
}
