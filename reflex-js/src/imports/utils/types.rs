// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::stdlib::Stdlib as JsStdlib;
use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator},
    lang::create_record,
};

pub(crate) fn import_types<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<JsStdlib>,
{
    let null = factory.create_nil_term();
    create_record(
        vec![
            (String::from("Boolean"), null.clone()),
            (String::from("Int"), null.clone()),
            (String::from("Float"), null.clone()),
            (String::from("String"), null.clone()),
            (
                String::from("Shape"),
                factory.create_builtin_term(JsStdlib::StructTypeFactory),
            ),
            (
                String::from("Fn"),
                factory.create_lambda_term(0, factory.create_lambda_term(1, null.clone())),
            ),
            (
                String::from("List"),
                factory.create_lambda_term(1, null.clone()),
            ),
            (
                String::from("Maybe"),
                factory.create_lambda_term(1, null.clone()),
            ),
        ],
        factory,
        allocator,
    )
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use crate::{
        builtins::JsBuiltins, imports::builtin_imports, parse_module, static_module_loader, Env,
    };
    use reflex::{
        allocator::DefaultAllocator,
        cache::SubstitutionCache,
        core::{
            evaluate, DependencyList, EvaluationResult, ExpressionFactory, HeapAllocator,
            StateCache,
        },
        lang::SharedTermFactory,
        stdlib::Stdlib,
    };

    #[test]
    fn struct_types() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        let path = Path::new("./foo.js");
        let loader = static_module_loader(builtin_imports(&factory, &allocator));
        let expression = parse_module(
            "
            import { Types } from 'reflex::utils';
            export default Types.Shape({
                foo: Types.Int,
                bar: Types.Int,
                baz: Types.Int,
            });
        ",
            &env,
            &path,
            &loader,
            &factory,
            &allocator,
        )
        .unwrap();
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
                factory.create_constructor_term(allocator.create_struct_prototype(vec![
                    allocator.create_static_string("foo"),
                    allocator.create_static_string("bar"),
                    allocator.create_static_string("baz"),
                ])),
                DependencyList::empty(),
            ),
        );
        let expression = parse_module(
            "
            import { Types } from 'reflex::utils';
            const Foo = Types.Shape({
                foo: Types.Int,
                bar: Types.Int,
                baz: Types.Int,
            });
            export default new Foo({ foo: 3, bar: 4, baz: 5 });
        ",
            &env,
            &path,
            &loader,
            &factory,
            &allocator,
        )
        .unwrap();
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
                factory.create_record_term(
                    allocator.create_struct_prototype(vec![
                        allocator.create_static_string("foo"),
                        allocator.create_static_string("bar"),
                        allocator.create_static_string("baz"),
                    ]),
                    allocator.create_list(vec![
                        factory.create_float_term(3.0),
                        factory.create_float_term(4.0),
                        factory.create_float_term(5.0),
                    ])
                ),
                DependencyList::empty(),
            ),
        );
        let expression = parse_module(
            "
            import { Types } from 'reflex::utils';
            const Foo = Types.Shape({
                foo: Types.Int,
                bar: Types.Int,
                baz: Types.Int,
            });
            export default new Foo({ baz: 5, bar: 4, foo: 3 });
        ",
            &env,
            &path,
            &loader,
            &factory,
            &allocator,
        )
        .unwrap();
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
                factory.create_record_term(
                    allocator.create_struct_prototype(vec![
                        allocator.create_static_string("foo"),
                        allocator.create_static_string("bar"),
                        allocator.create_static_string("baz"),
                    ]),
                    allocator.create_list(vec![
                        factory.create_float_term(3.0),
                        factory.create_float_term(4.0),
                        factory.create_float_term(5.0),
                    ])
                ),
                DependencyList::empty(),
            ),
        );
        let expression = parse_module(
            "
            import { Types } from 'reflex::utils';
            const Foo = Types.Shape({
                foo: Types.Int,
                bar: Types.Int,
                baz: Types.Int,
            });
            export default new Foo({ foo: 3 + 1, bar: 4 + 1, baz: 5 + 1 });
        ",
            &env,
            &path,
            &loader,
            &factory,
            &allocator,
        )
        .unwrap();
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
                factory.create_record_term(
                    allocator.create_struct_prototype(vec![
                        allocator.create_static_string("foo"),
                        allocator.create_static_string("bar"),
                        allocator.create_static_string("baz"),
                    ]),
                    allocator.create_list(vec![
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Add),
                            allocator.create_pair(
                                factory.create_float_term(3.0),
                                factory.create_float_term(1.0),
                            ),
                        ),
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Add),
                            allocator.create_pair(
                                factory.create_float_term(4.0),
                                factory.create_float_term(1.0),
                            ),
                        ),
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Add),
                            allocator.create_pair(
                                factory.create_float_term(5.0),
                                factory.create_float_term(1.0),
                            ),
                        ),
                    ])
                ),
                DependencyList::empty(),
            ),
        );
        let expression = parse_module(
            "
            import { Types } from 'reflex::utils';
            const Foo = Types.Shape({
                foo: Types.Int,
                bar: Types.Int,
                baz: Types.Int,
            });
            export default new Foo({ foo: 3, bar: 4, baz: 5 }).bar;
        ",
            &env,
            &path,
            &loader,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(
            &expression,
            &StateCache::default(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_float_term(4.0), DependencyList::empty(),),
        );
    }
}
