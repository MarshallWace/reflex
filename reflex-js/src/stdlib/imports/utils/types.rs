// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::str::FromStr;

use reflex::{
    core::{
        ArgType, Arity, Expression, ExpressionFactory, ExpressionList, FunctionArity,
        HeapAllocator, NativeAllocator,
    },
    lang::{create_struct, NativeFunction, ValueTerm},
};
use uuid::Uuid;

pub(crate) fn import_types<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    let null = factory.create_value_term(ValueTerm::Null);
    create_struct(
        vec![
            (String::from("Boolean"), null.clone()),
            (String::from("Int"), null.clone()),
            (String::from("Float"), null.clone()),
            (String::from("String"), null.clone()),
            (
                String::from("Shape"),
                factory.create_native_function_term(struct_type_factory()),
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

pub fn struct_type_factory<T: Expression>() -> NativeFunction<T> {
    NativeFunction::new_with_uuid(
        StructTypeFactory::uid(),
        StructTypeFactory::name(),
        StructTypeFactory::arity(),
        StructTypeFactory::apply,
    )
}
struct StructTypeFactory {}
impl StructTypeFactory {
    const NAME: &'static str = "StructTypeFactory";
    const UUID: &'static str = "69fdfb4f-7a4a-4414-8140-ededbdc9368c";
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
    fn uid() -> Uuid {
        Uuid::from_str(Self::UUID).unwrap()
    }
    fn name() -> Option<&'static str> {
        Some(Self::NAME)
    }
    fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
    fn apply<T: Expression>(
        args: ExpressionList<T>,
        factory: &dyn ExpressionFactory<T>,
        allocator: &dyn NativeAllocator<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
        let shape = args.next().unwrap();
        if let Some(shape) = factory.match_struct_term(&shape) {
            Ok(
                factory
                    .create_constructor_term(allocator.clone_struct_prototype(shape.prototype())),
            )
        } else {
            Err(format!(
                "Invalid shape definition: Expected <struct>, received {}",
                shape
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use crate::{parse_module, static_module_loader, stdlib::builtin_imports, Env};
    use reflex::{
        allocator::DefaultAllocator,
        cache::SubstitutionCache,
        core::{
            evaluate, DependencyList, EvaluationResult, ExpressionFactory, HeapAllocator,
            StateCache,
        },
        lang::{BuiltinTerm, TermFactory, ValueTerm},
    };

    #[test]
    fn struct_types() {
        let factory = TermFactory::default();
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
                    allocator.create_string(String::from("foo")),
                    allocator.create_string(String::from("bar")),
                    allocator.create_string(String::from("baz")),
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
                factory.create_struct_term(
                    allocator.create_struct_prototype(vec![
                        allocator.create_string(String::from("foo")),
                        allocator.create_string(String::from("bar")),
                        allocator.create_string(String::from("baz")),
                    ]),
                    allocator.create_list(vec![
                        factory.create_value_term(ValueTerm::Float(3.0)),
                        factory.create_value_term(ValueTerm::Float(4.0)),
                        factory.create_value_term(ValueTerm::Float(5.0)),
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
                factory.create_struct_term(
                    allocator.create_struct_prototype(vec![
                        allocator.create_string(String::from("foo")),
                        allocator.create_string(String::from("bar")),
                        allocator.create_string(String::from("baz")),
                    ]),
                    allocator.create_list(vec![
                        factory.create_value_term(ValueTerm::Float(3.0)),
                        factory.create_value_term(ValueTerm::Float(4.0)),
                        factory.create_value_term(ValueTerm::Float(5.0)),
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
                factory.create_struct_term(
                    allocator.create_struct_prototype(vec![
                        allocator.create_string(String::from("foo")),
                        allocator.create_string(String::from("bar")),
                        allocator.create_string(String::from("baz")),
                    ]),
                    allocator.create_list(vec![
                        factory.create_application_term(
                            factory.create_builtin_term(BuiltinTerm::Add),
                            allocator.create_pair(
                                factory.create_value_term(ValueTerm::Float(3.0)),
                                factory.create_value_term(ValueTerm::Float(1.0)),
                            ),
                        ),
                        factory.create_application_term(
                            factory.create_builtin_term(BuiltinTerm::Add),
                            allocator.create_pair(
                                factory.create_value_term(ValueTerm::Float(4.0)),
                                factory.create_value_term(ValueTerm::Float(1.0)),
                            ),
                        ),
                        factory.create_application_term(
                            factory.create_builtin_term(BuiltinTerm::Add),
                            allocator.create_pair(
                                factory.create_value_term(ValueTerm::Float(5.0)),
                                factory.create_value_term(ValueTerm::Float(1.0)),
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
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Float(4.0)),
                DependencyList::empty(),
            ),
        );
    }
}
