// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::any::TypeId;

use reflex::{
    core::{Arity, Expression, LambdaTerm, NativeFunction, Signal, SignalTerm, Term, VarArgs},
    hash::{hash_object, HashId},
    stdlib::{signal::SignalType, value::ValueTerm},
};

use crate::stdlib::imports::create_struct;

pub(crate) fn import_types() -> Expression {
    let null = Expression::new(Term::Value(ValueTerm::Null));
    create_struct(vec![
        ("Boolean", Expression::clone(&null)),
        ("Int", Expression::clone(&null)),
        ("Float", Expression::clone(&null)),
        ("String", Expression::clone(&null)),
        (
            "Shape",
            Expression::new(Term::Native(NativeFunction::new(
                StructTypeFactory::hash(),
                StructTypeFactory::arity(),
                StructTypeFactory::apply,
            ))),
        ),
        (
            "Fn",
            Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 0, Some(VarArgs::Lazy)),
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 1, None),
                    Expression::clone(&null),
                ))),
            ))),
        ),
        (
            "List",
            Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 1, None),
                Expression::clone(&null),
            ))),
        ),
        (
            "Maybe",
            Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 1, None),
                Expression::clone(&null),
            ))),
        ),
    ])
}

struct StructTypeFactory {}
impl StructTypeFactory {
    fn hash() -> HashId {
        hash_object(&TypeId::of::<Self>())
    }
    fn arity() -> Arity {
        Arity::from(1, 0, None)
    }
    fn apply(args: Vec<Expression>) -> Expression {
        if args.len() != 1 {
            return Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![Expression::new(Term::Value(ValueTerm::String(format!(
                    "Expected 1 argument, received {}",
                    args.len(),
                ))))],
            ))));
        }
        let mut args = args.into_iter();
        let shape = args.next().unwrap();
        let result = match shape.value() {
            Term::Struct(shape) => match shape.prototype() {
                Some(prototype) => Ok(Expression::new(Term::Constructor(
                    prototype.clone(),
                    VarArgs::Lazy,
                ))),
                None => Err(format!(
                    "Expected named struct shape definition fields, received, {}",
                    shape,
                )),
            },
            _ => Err(format!(
                "Expected struct shape definition, received {}",
                shape,
            )),
        };
        match result {
            Ok(result) => result,
            Err(error) => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![Expression::new(Term::Value(ValueTerm::String(error)))],
            )))),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use crate::{parse_module, static_module_loader, stdlib::builtin_imports, Env};
    use reflex::{
        cache::SubstitutionCache,
        core::{
            ApplicationTerm, DependencyList, DynamicState, EvaluationResult, Expression,
            StructPrototype, StructTerm, Term, VarArgs,
        },
        stdlib::{
            builtin::BuiltinTerm,
            value::{StringValue, ValueTerm},
        },
    };

    #[test]
    fn struct_types() {
        let env = Env::new();
        let path = Path::new("./foo.js");
        let loader = static_module_loader(builtin_imports());
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
        )
        .unwrap();
        let result = expression.evaluate(&DynamicState::new(), &mut SubstitutionCache::new());
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Constructor(
                    StructPrototype::new(vec![
                        ValueTerm::String(StringValue::from("foo")),
                        ValueTerm::String(StringValue::from("bar")),
                        ValueTerm::String(StringValue::from("baz")),
                    ]),
                    VarArgs::Lazy,
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
        )
        .unwrap();
        let result = expression.evaluate(&DynamicState::new(), &mut SubstitutionCache::new());
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Struct(StructTerm::new(
                    Some(StructPrototype::new(vec![
                        ValueTerm::String(StringValue::from("foo")),
                        ValueTerm::String(StringValue::from("bar")),
                        ValueTerm::String(StringValue::from("baz")),
                    ])),
                    vec![
                        Expression::new(Term::Value(ValueTerm::Float(3.0))),
                        Expression::new(Term::Value(ValueTerm::Float(4.0))),
                        Expression::new(Term::Value(ValueTerm::Float(5.0))),
                    ],
                ))),
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
        )
        .unwrap();
        let result = expression.evaluate(&DynamicState::new(), &mut SubstitutionCache::new());
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Struct(StructTerm::new(
                    Some(StructPrototype::new(vec![
                        ValueTerm::String(StringValue::from("foo")),
                        ValueTerm::String(StringValue::from("bar")),
                        ValueTerm::String(StringValue::from("baz")),
                    ])),
                    vec![
                        Expression::new(Term::Value(ValueTerm::Float(3.0))),
                        Expression::new(Term::Value(ValueTerm::Float(4.0))),
                        Expression::new(Term::Value(ValueTerm::Float(5.0))),
                    ],
                ))),
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
        )
        .unwrap();
        let result = expression.evaluate(&DynamicState::new(), &mut SubstitutionCache::new());
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Struct(StructTerm::new(
                    Some(StructPrototype::new(vec![
                        ValueTerm::String(StringValue::from("foo")),
                        ValueTerm::String(StringValue::from("bar")),
                        ValueTerm::String(StringValue::from("baz")),
                    ])),
                    vec![
                        Expression::new(Term::Application(ApplicationTerm::new(
                            Expression::new(Term::Builtin(BuiltinTerm::Add)),
                            vec![
                                Expression::new(Term::Value(ValueTerm::Float(3.0))),
                                Expression::new(Term::Value(ValueTerm::Float(1.0))),
                            ],
                        ))),
                        Expression::new(Term::Application(ApplicationTerm::new(
                            Expression::new(Term::Builtin(BuiltinTerm::Add)),
                            vec![
                                Expression::new(Term::Value(ValueTerm::Float(4.0))),
                                Expression::new(Term::Value(ValueTerm::Float(1.0))),
                            ],
                        ))),
                        Expression::new(Term::Application(ApplicationTerm::new(
                            Expression::new(Term::Builtin(BuiltinTerm::Add)),
                            vec![
                                Expression::new(Term::Value(ValueTerm::Float(5.0))),
                                Expression::new(Term::Value(ValueTerm::Float(1.0))),
                            ],
                        ))),
                    ],
                ))),
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
        )
        .unwrap();
        let result = expression.evaluate(&DynamicState::new(), &mut SubstitutionCache::new());
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Float(4.0))),
                DependencyList::empty(),
            ),
        );
    }
}
