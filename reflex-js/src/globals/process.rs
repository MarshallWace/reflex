// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::empty;

use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator},
    env::ENV_STATE_TOKEN,
    lang::create_record,
};

pub fn global_process<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    create_record(
        vec![(
            String::from("env"),
            factory.create_dynamic_variable_term(
                ENV_STATE_TOKEN,
                create_record(empty(), factory, allocator),
            ),
        )],
        factory,
        allocator,
    )
}
