// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::stdlib::StructTypeFactory;
use reflex::core::{create_record, Expression, ExpressionFactory, HeapAllocator};

pub(crate) fn import_types<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<StructTypeFactory>,
{
    let null = factory.create_nil_term();
    create_record(
        vec![
            (
                factory.create_string_term(allocator.create_static_string("Boolean")),
                null.clone(),
            ),
            (
                factory.create_string_term(allocator.create_static_string("Int")),
                null.clone(),
            ),
            (
                factory.create_string_term(allocator.create_static_string("Float")),
                null.clone(),
            ),
            (
                factory.create_string_term(allocator.create_static_string("String")),
                null.clone(),
            ),
            (
                factory.create_string_term(allocator.create_static_string("Shape")),
                factory.create_builtin_term(StructTypeFactory),
            ),
            (
                factory.create_string_term(allocator.create_static_string("Fn")),
                factory.create_lambda_term(0, factory.create_lambda_term(1, null.clone())),
            ),
            (
                factory.create_string_term(allocator.create_static_string("List")),
                factory.create_lambda_term(1, null.clone()),
            ),
            (
                factory.create_string_term(allocator.create_static_string("Maybe")),
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
        cache::SubstitutionCache,
        core::{
            evaluate, DependencyList, EvaluationResult, ExpressionFactory, HeapAllocator,
            StateCache,
        },
    };
    use reflex_lang::{allocator::DefaultAllocator, SharedTermFactory};
    use reflex_stdlib::Add;

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
                factory.create_constructor_term(allocator.create_struct_prototype(
                    allocator.create_list([
                        factory.create_string_term(allocator.create_static_string("foo")),
                        factory.create_string_term(allocator.create_static_string("bar")),
                        factory.create_string_term(allocator.create_static_string("baz")),
                    ])
                )),
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
                    allocator.create_struct_prototype(allocator.create_list([
                        factory.create_string_term(allocator.create_static_string("foo")),
                        factory.create_string_term(allocator.create_static_string("bar")),
                        factory.create_string_term(allocator.create_static_string("baz")),
                    ])),
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
                    allocator.create_struct_prototype(allocator.create_list([
                        factory.create_string_term(allocator.create_static_string("foo")),
                        factory.create_string_term(allocator.create_static_string("bar")),
                        factory.create_string_term(allocator.create_static_string("baz")),
                    ])),
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
                    allocator.create_struct_prototype(allocator.create_list([
                        factory.create_string_term(allocator.create_static_string("foo")),
                        factory.create_string_term(allocator.create_static_string("bar")),
                        factory.create_string_term(allocator.create_static_string("baz")),
                    ])),
                    allocator.create_list(vec![
                        factory.create_application_term(
                            factory.create_builtin_term(Add),
                            allocator.create_pair(
                                factory.create_float_term(3.0),
                                factory.create_float_term(1.0),
                            ),
                        ),
                        factory.create_application_term(
                            factory.create_builtin_term(Add),
                            allocator.create_pair(
                                factory.create_float_term(4.0),
                                factory.create_float_term(1.0),
                            ),
                        ),
                        factory.create_application_term(
                            factory.create_builtin_term(Add),
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
