// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{Expression, ExpressionFactory, HeapAllocator};
use reflex_stdlib::{Apply, CollectHashMap, ResolveHashMap};

pub fn global_map<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Apply> + From<CollectHashMap> + From<ResolveHashMap>,
{
    factory.create_lambda_term(
        1,
        factory.create_application_term(
            factory.create_builtin_term(ResolveHashMap),
            allocator.create_unit_list(factory.create_application_term(
                factory.create_builtin_term(Apply),
                allocator.create_pair(
                    factory.create_builtin_term(CollectHashMap),
                    factory.create_variable_term(0),
                ),
            )),
        ),
    )
}
