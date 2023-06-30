// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{create_record, Builtin, Expression, ExpressionFactory, HeapAllocator};
use reflex_stdlib::{Apply, ConstructRecord, Get, Keys, Map, ResolveList, Unzip, Values, Zip};

use crate::stdlib::Accessor;

pub fn global_object<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: Builtin
        + From<Accessor>
        + From<Apply>
        + From<ConstructRecord>
        + From<Get>
        + From<Keys>
        + From<Map>
        + From<ResolveList>
        + From<Unzip>
        + From<Values>
        + From<Zip>,
{
    create_record(
        [
            (
                factory.create_string_term(allocator.create_static_string("entries")),
                factory.create_lambda_term(
                    1,
                    factory.create_application_term(
                        factory.create_builtin_term(Zip),
                        allocator.create_pair(
                            factory.create_application_term(
                                factory.create_builtin_term(Keys),
                                allocator.create_unit_list(factory.create_variable_term(0)),
                            ),
                            factory.create_application_term(
                                factory.create_builtin_term(Values),
                                allocator.create_unit_list(factory.create_variable_term(0)),
                            ),
                        ),
                    ),
                ),
            ),
            (
                factory.create_string_term(allocator.create_static_string("fromEntries")),
                factory.create_lambda_term(
                    1,
                    factory.create_let_term(
                        factory.create_application_term(
                            factory.create_builtin_term(Unzip),
                            allocator.create_unit_list(factory.create_variable_term(0)),
                        ),
                        factory.create_application_term(
                            factory.create_builtin_term(ConstructRecord),
                            allocator.create_pair(
                                factory.create_application_term(
                                    factory.create_builtin_term(ResolveList),
                                    allocator.create_unit_list(factory.create_application_term(
                                        factory.create_builtin_term(Get),
                                        allocator.create_pair(
                                            factory.create_variable_term(0),
                                            factory.create_int_term(0),
                                        ),
                                    )),
                                ),
                                factory.create_application_term(
                                    factory.create_builtin_term(ResolveList),
                                    allocator.create_unit_list(factory.create_application_term(
                                        factory.create_builtin_term(Get),
                                        allocator.create_pair(
                                            factory.create_variable_term(0),
                                            factory.create_int_term(1),
                                        ),
                                    )),
                                ),
                            ),
                        ),
                    ),
                ),
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

#[cfg(test)]
mod tests {
    use reflex::{
        cache::SubstitutionCache,
        core::{evaluate, DependencyList, EvaluationResult, StateCache},
    };
    use reflex_lang::{allocator::DefaultAllocator, SharedTermFactory};
    use reflex_stdlib::{Abs, Concat};

    use crate::builtins::JsBuiltins;

    use super::*;

    #[test]
    fn entries() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let globals = global_object(&factory, &allocator);
        {
            let expression = factory.create_application_term(
                factory.create_application_term(
                    factory.create_builtin_term(Get),
                    allocator.create_pair(
                        globals.clone(),
                        factory.create_string_term(allocator.create_string("entries")),
                    ),
                ),
                allocator.create_unit_list(factory.create_list_term(allocator.create_list([
                    factory.create_string_term(allocator.create_static_string("foo")),
                    factory.create_string_term(allocator.create_static_string("bar")),
                    factory.create_string_term(allocator.create_static_string("baz")),
                ]))),
            );
            let result = evaluate(
                &expression,
                &StateCache::default(),
                &factory,
                &allocator,
                &mut SubstitutionCache::new(),
            );
            assert_eq!(
                result,
                EvaluationResult::new(
                    factory.create_list_term(allocator.create_triple(
                        factory.create_list_term(allocator.create_pair(
                            factory.create_int_term(0),
                            factory.create_string_term(allocator.create_static_string("foo")),
                        )),
                        factory.create_list_term(allocator.create_pair(
                            factory.create_int_term(1),
                            factory.create_string_term(allocator.create_static_string("bar")),
                        )),
                        factory.create_list_term(allocator.create_pair(
                            factory.create_int_term(2),
                            factory.create_string_term(allocator.create_static_string("baz")),
                        )),
                    )),
                    DependencyList::empty(),
                ),
            );
        }
        {
            let expression = factory.create_application_term(
                factory.create_application_term(
                    factory.create_builtin_term(Get),
                    allocator.create_pair(
                        globals.clone(),
                        factory.create_string_term(allocator.create_string("entries")),
                    ),
                ),
                allocator.create_unit_list(create_record(
                    [
                        (
                            factory.create_string_term(allocator.create_static_string("foo")),
                            factory.create_int_term(3),
                        ),
                        (
                            factory.create_string_term(allocator.create_static_string("bar")),
                            factory.create_int_term(4),
                        ),
                        (
                            factory.create_string_term(allocator.create_static_string("baz")),
                            factory.create_int_term(5),
                        ),
                    ],
                    &factory,
                    &allocator,
                )),
            );
            let result = evaluate(
                &expression,
                &StateCache::default(),
                &factory,
                &allocator,
                &mut SubstitutionCache::new(),
            );
            assert_eq!(
                result,
                EvaluationResult::new(
                    factory.create_list_term(allocator.create_triple(
                        factory.create_list_term(allocator.create_pair(
                            factory.create_string_term(allocator.create_static_string("foo")),
                            factory.create_int_term(3),
                        )),
                        factory.create_list_term(allocator.create_pair(
                            factory.create_string_term(allocator.create_static_string("bar")),
                            factory.create_int_term(4),
                        )),
                        factory.create_list_term(allocator.create_pair(
                            factory.create_string_term(allocator.create_static_string("baz")),
                            factory.create_int_term(5),
                        )),
                    )),
                    DependencyList::empty(),
                ),
            );
        }
        {
            let expression = factory.create_application_term(
                factory.create_application_term(
                    factory.create_builtin_term(Get),
                    allocator.create_pair(
                        globals.clone(),
                        factory.create_string_term(allocator.create_string("entries")),
                    ),
                ),
                allocator.create_unit_list(factory.create_hashmap_term([
                    (
                        factory.create_string_term(allocator.create_static_string("foo")),
                        factory.create_int_term(3),
                    ),
                    (
                        factory.create_string_term(allocator.create_static_string("bar")),
                        factory.create_int_term(4),
                    ),
                    (
                        factory.create_string_term(allocator.create_static_string("baz")),
                        factory.create_int_term(5),
                    ),
                ])),
            );
            let result = evaluate(
                &expression,
                &StateCache::default(),
                &factory,
                &allocator,
                &mut SubstitutionCache::new(),
            );
            assert_eq!(
                result,
                EvaluationResult::new(
                    factory.create_list_term(allocator.create_triple(
                        factory.create_list_term(allocator.create_pair(
                            factory.create_string_term(allocator.create_static_string("foo")),
                            factory.create_int_term(3),
                        )),
                        factory.create_list_term(allocator.create_pair(
                            factory.create_string_term(allocator.create_static_string("bar")),
                            factory.create_int_term(4),
                        )),
                        factory.create_list_term(allocator.create_pair(
                            factory.create_string_term(allocator.create_static_string("baz")),
                            factory.create_int_term(5),
                        )),
                    )),
                    DependencyList::empty(),
                ),
            );
        }
        {
            let expression = factory.create_application_term(
                factory.create_application_term(
                    factory.create_builtin_term(Get),
                    allocator.create_pair(
                        globals.clone(),
                        factory.create_string_term(allocator.create_string("entries")),
                    ),
                ),
                allocator.create_unit_list(factory.create_hashset_term([
                    factory.create_string_term(allocator.create_static_string("foo")),
                    factory.create_string_term(allocator.create_static_string("bar")),
                    factory.create_string_term(allocator.create_static_string("baz")),
                ])),
            );
            let result = evaluate(
                &expression,
                &StateCache::default(),
                &factory,
                &allocator,
                &mut SubstitutionCache::new(),
            );
            assert_eq!(
                result,
                EvaluationResult::new(
                    factory.create_list_term(allocator.create_triple(
                        factory.create_list_term(allocator.create_pair(
                            factory.create_string_term(allocator.create_static_string("foo")),
                            factory.create_string_term(allocator.create_static_string("foo")),
                        )),
                        factory.create_list_term(allocator.create_pair(
                            factory.create_string_term(allocator.create_static_string("bar")),
                            factory.create_string_term(allocator.create_static_string("bar")),
                        )),
                        factory.create_list_term(allocator.create_pair(
                            factory.create_string_term(allocator.create_static_string("baz")),
                            factory.create_string_term(allocator.create_static_string("baz")),
                        )),
                    )),
                    DependencyList::empty(),
                ),
            );
        }
    }

    #[test]
    fn from_entries() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let globals = global_object(&factory, &allocator);
        {
            let expression = factory.create_application_term(
                factory.create_application_term(
                    factory.create_builtin_term(Get),
                    allocator.create_pair(
                        globals.clone(),
                        factory.create_string_term(allocator.create_string("fromEntries")),
                    ),
                ),
                allocator.create_unit_list(factory.create_list_term(allocator.create_triple(
                    factory.create_list_term(allocator.create_pair(
                        factory.create_string_term(allocator.create_static_string("foo")),
                        factory.create_int_term(3),
                    )),
                    factory.create_list_term(allocator.create_pair(
                        factory.create_string_term(allocator.create_static_string("bar")),
                        factory.create_int_term(4),
                    )),
                    factory.create_list_term(allocator.create_pair(
                        factory.create_string_term(allocator.create_static_string("baz")),
                        factory.create_int_term(5),
                    )),
                ))),
            );
            let result = evaluate(
                &expression,
                &StateCache::default(),
                &factory,
                &allocator,
                &mut SubstitutionCache::new(),
            );
            assert_eq!(
                result,
                EvaluationResult::new(
                    create_record(
                        [
                            (
                                factory.create_string_term(allocator.create_static_string("foo")),
                                factory.create_int_term(3),
                            ),
                            (
                                factory.create_string_term(allocator.create_static_string("bar")),
                                factory.create_int_term(4),
                            ),
                            (
                                factory.create_string_term(allocator.create_static_string("baz")),
                                factory.create_int_term(5),
                            ),
                        ],
                        &factory,
                        &allocator
                    ),
                    DependencyList::empty(),
                ),
            );
        }
        {
            let expression = factory.create_application_term(
                factory.create_application_term(
                    factory.create_builtin_term(Get),
                    allocator.create_pair(
                        globals.clone(),
                        factory.create_string_term(allocator.create_string("fromEntries")),
                    ),
                ),
                allocator.create_unit_list(factory.create_list_term(allocator.create_triple(
                    factory.create_list_term(allocator.create_pair(
                        factory.create_application_term(
                            factory.create_builtin_term(Concat),
                            allocator.create_pair(
                                factory.create_string_term(allocator.create_static_string("key:")),
                                factory.create_string_term(allocator.create_static_string("foo")),
                            ),
                        ),
                        factory.create_application_term(
                            factory.create_builtin_term(Abs),
                            allocator.create_unit_list(factory.create_int_term(-3)),
                        ),
                    )),
                    factory.create_list_term(allocator.create_pair(
                        factory.create_application_term(
                            factory.create_builtin_term(Concat),
                            allocator.create_pair(
                                factory.create_string_term(allocator.create_static_string("key:")),
                                factory.create_string_term(allocator.create_static_string("bar")),
                            ),
                        ),
                        factory.create_application_term(
                            factory.create_builtin_term(Abs),
                            allocator.create_unit_list(factory.create_int_term(-4)),
                        ),
                    )),
                    factory.create_list_term(allocator.create_pair(
                        factory.create_application_term(
                            factory.create_builtin_term(Concat),
                            allocator.create_pair(
                                factory.create_string_term(allocator.create_static_string("key:")),
                                factory.create_string_term(allocator.create_static_string("baz")),
                            ),
                        ),
                        factory.create_application_term(
                            factory.create_builtin_term(Abs),
                            allocator.create_unit_list(factory.create_int_term(-5)),
                        ),
                    )),
                ))),
            );
            let result = evaluate(
                &expression,
                &StateCache::default(),
                &factory,
                &allocator,
                &mut SubstitutionCache::new(),
            );
            assert_eq!(
                result,
                EvaluationResult::new(
                    create_record(
                        [
                            (
                                factory
                                    .create_string_term(allocator.create_static_string("key:foo")),
                                factory.create_int_term(3),
                            ),
                            (
                                factory
                                    .create_string_term(allocator.create_static_string("key:bar")),
                                factory.create_int_term(4),
                            ),
                            (
                                factory
                                    .create_string_term(allocator.create_static_string("key:baz")),
                                factory.create_int_term(5),
                            ),
                        ],
                        &factory,
                        &allocator
                    ),
                    DependencyList::empty(),
                ),
            );
        }
    }
}
