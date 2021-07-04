// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::any::TypeId;

use reflex::{
    core::{
        ApplicationTerm, Arity, EnumIndex, EnumTerm, EnumVariantPrototype, Expression, LambdaTerm,
        NativeFunction, Signal, SignalTerm, StructTerm, Term, VarArgs, VariableTerm,
    },
    hash::{hash_object, HashId},
    stdlib::{builtin::BuiltinTerm, signal::SignalType, value::ValueTerm},
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
            "Enum",
            Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 1, None),
                Expression::new(Term::Application(ApplicationTerm::new(
                    Expression::new(Term::Native(NativeFunction::new(
                        EnumTypeFactory::hash(),
                        EnumTypeFactory::arity(),
                        EnumTypeFactory::apply,
                    ))),
                    vec![Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Builtin(BuiltinTerm::ResolveDeep)),
                        vec![Expression::new(Term::Variable(VariableTerm::scoped(0)))],
                    )))],
                ))),
            ))),
        ),
        (
            "EnumVariant",
            Expression::new(Term::Native(NativeFunction::new(
                EnumVariantFactory::hash(),
                EnumVariantFactory::arity(),
                EnumVariantFactory::apply,
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
                Some(prototype) => Ok(Expression::new(Term::StructConstructor(
                    VarArgs::Lazy,
                    prototype.clone(),
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

struct EnumTypeFactory {}
impl EnumTypeFactory {
    fn hash() -> HashId {
        hash_object(&TypeId::of::<Self>())
    }
    fn arity() -> Arity {
        Arity::from(1, 0, Some(VarArgs::Eager))
    }
    fn apply(args: Vec<Expression>) -> Expression {
        if args.len() < 1 {
            return Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![Expression::new(Term::Value(ValueTerm::String(format!(
                    "Expected 1 or more arguments, received {}",
                    args.len(),
                ))))],
            ))));
        }
        let mut args = args.into_iter();
        let shape = args.next().unwrap();
        let result = match shape.value() {
            Term::Struct(shape) => match shape.prototype() {
                Some(prototype) => {
                    let variants = shape
                        .fields()
                        .iter()
                        .enumerate()
                        .map(|(index, variant)| parse_enum_variant(variant, index))
                        .collect::<Result<Vec<_>, _>>();
                    match variants {
                        Ok(variants) => Ok(Expression::new(Term::Struct(StructTerm::new(
                            Some(prototype.clone()),
                            variants,
                        )))),
                        Err(error) => Err(error),
                    }
                }
                None => Err(format!("Expected enum definition, received {}", shape)),
            },
            _ => Err(format!("Expected enum definition, received {}", shape,)),
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
fn parse_enum_variant(variant: &Expression, index: EnumIndex) -> Result<Expression, String> {
    match variant.value() {
        Term::EnumConstructor(variant) => Ok(Expression::new(Term::EnumConstructor(
            EnumVariantPrototype::new(index, variant.arity()),
        ))),
        Term::Native(term) if term.uid() == EnumVariantFactory::hash() => Ok(Expression::new(
            Term::Enum(EnumTerm::new(index, Vec::new())),
        )),
        Term::Value(value)
            if match value {
                ValueTerm::String(_) | ValueTerm::Int(_) | ValueTerm::Float(_) => true,
                _ => false,
            } =>
        {
            Ok(Expression::clone(variant))
        }
        _ => Err(format!(
            "Expected enum variant definition, received {}",
            variant
        )),
    }
}

struct EnumVariantFactory {}
impl EnumVariantFactory {
    fn hash() -> HashId {
        hash_object(&TypeId::of::<Self>())
    }
    fn arity() -> Arity {
        Arity::from(0, 0, Some(VarArgs::Eager))
    }
    fn apply(args: Vec<Expression>) -> Expression {
        let num_args = args.len();
        Expression::new(Term::EnumConstructor(EnumVariantPrototype::new(
            0, num_args,
        )))
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use crate::{parse_module, static_module_loader, stdlib::builtin_imports, Env};
    use reflex::{
        cache::GenerationalGc,
        core::{
            ApplicationTerm, DependencyList, DynamicState, EnumTerm, EnumVariantPrototype,
            EvaluationResult, Expression, StructPrototype, StructTerm, Term, VarArgs,
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
        let result = expression.evaluate(&DynamicState::new(), &mut GenerationalGc::new());
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::StructConstructor(
                    VarArgs::Lazy,
                    StructPrototype::new(vec![
                        ValueTerm::String(StringValue::from("foo")),
                        ValueTerm::String(StringValue::from("bar")),
                        ValueTerm::String(StringValue::from("baz")),
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
        )
        .unwrap();
        let result = expression.evaluate(&DynamicState::new(), &mut GenerationalGc::new());
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
        let result = expression.evaluate(&DynamicState::new(), &mut GenerationalGc::new());
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
        let result = expression.evaluate(&DynamicState::new(), &mut GenerationalGc::new());
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
        let result = expression.evaluate(&DynamicState::new(), &mut GenerationalGc::new());
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Float(4.0))),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn static_enum_types() {
        let env = Env::new();
        let path = Path::new("./foo.js");
        let loader = static_module_loader(builtin_imports());
        let expression = parse_module(
            "
            import { Types } from 'reflex::utils';
            export default Types.Enum({
                Foo: \"FOO\",
                Bar: \"BAR\",
                Baz: \"BAZ\",
            });
        ",
            &env,
            &path,
            &loader,
        )
        .unwrap();
        let result = expression.evaluate(&DynamicState::new(), &mut GenerationalGc::new());
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Struct(StructTerm::new(
                    Some(StructPrototype::new(vec![
                        ValueTerm::String(StringValue::from("Foo")),
                        ValueTerm::String(StringValue::from("Bar")),
                        ValueTerm::String(StringValue::from("Baz")),
                    ])),
                    vec![
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from("FOO")))),
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from("BAR")))),
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from("BAZ")))),
                    ],
                ))),
                DependencyList::empty(),
            ),
        );
        let expression = parse_module(
            "
            import { Types } from 'reflex::utils';
            export default Types.Enum({
                Foo: \"FOO\",
                Bar: \"BAR\",
                Baz: \"BAZ\",
            }).Foo;
        ",
            &env,
            &path,
            &loader,
        )
        .unwrap();
        let result = expression.evaluate(&DynamicState::new(), &mut GenerationalGc::new());
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::String(StringValue::from("FOO")))),
                DependencyList::empty(),
            ),
        );
        let expression = parse_module(
            "
            import { Types } from 'reflex::utils';
            export default Types.Enum({
                Foo: \"FOO\",
                Bar: \"BAR\",
                Baz: \"BAZ\",
            }).Baz;
        ",
            &env,
            &path,
            &loader,
        )
        .unwrap();
        let result = expression.evaluate(&DynamicState::new(), &mut GenerationalGc::new());
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::String(StringValue::from("BAZ")))),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn parameterized_enum_types() {
        let env = Env::new();
        let path = Path::new("./foo.js");
        let loader = static_module_loader(builtin_imports());
        let expression = parse_module(
            "
            import { Types } from 'reflex::utils';
            export default Types.Enum({
                Static: Types.EnumVariant,
                Empty: Types.EnumVariant(),
                Single: Types.EnumVariant(Types.Int),
                Multiple: Types.EnumVariant(Types.Int, Types.Int, Types.Int),
            });
        ",
            &env,
            &path,
            &loader,
        )
        .unwrap();
        let result = expression.evaluate(&DynamicState::new(), &mut GenerationalGc::new());
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Struct(StructTerm::new(
                    Some(StructPrototype::new(vec![
                        ValueTerm::String(StringValue::from("Static")),
                        ValueTerm::String(StringValue::from("Empty")),
                        ValueTerm::String(StringValue::from("Single")),
                        ValueTerm::String(StringValue::from("Multiple")),
                    ])),
                    vec![
                        Expression::new(Term::Enum(EnumTerm::new(0, Vec::new()))),
                        Expression::new(Term::EnumConstructor(EnumVariantPrototype::new(1, 0))),
                        Expression::new(Term::EnumConstructor(EnumVariantPrototype::new(2, 1))),
                        Expression::new(Term::EnumConstructor(EnumVariantPrototype::new(3, 3))),
                    ],
                ))),
                DependencyList::empty(),
            ),
        );
        let expression = parse_module(
            "
            import { Types } from 'reflex::utils';
            export default Types.Enum({
                Static: Types.EnumVariant,
                Empty: Types.EnumVariant(),
                Single: Types.EnumVariant(Types.Int),
                Multiple: Types.EnumVariant(Types.Int, Types.Int, Types.Int),
            }).Static;
        ",
            &env,
            &path,
            &loader,
        )
        .unwrap();
        let result = expression.evaluate(&DynamicState::new(), &mut GenerationalGc::new());
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Enum(EnumTerm::new(0, Vec::new()))),
                DependencyList::empty(),
            ),
        );
        let expression = parse_module(
            "
            import { Types } from 'reflex::utils';
            export default Types.Enum({
                Static: Types.EnumVariant,
                Empty: Types.EnumVariant(),
                Single: Types.EnumVariant(Types.Int),
                Multiple: Types.EnumVariant(Types.Int, Types.Int, Types.Int),
            }).Empty();
        ",
            &env,
            &path,
            &loader,
        )
        .unwrap();
        let result = expression.evaluate(&DynamicState::new(), &mut GenerationalGc::new());
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Enum(EnumTerm::new(1, Vec::new()))),
                DependencyList::empty(),
            ),
        );
        let expression = parse_module(
            "
            import { Types } from 'reflex::utils';
            export default Types.Enum({
                Static: Types.EnumVariant,
                Empty: Types.EnumVariant(),
                Single: Types.EnumVariant(Types.Int),
                Multiple: Types.EnumVariant(Types.Int, Types.Int, Types.Int),
            }).Single(3);
        ",
            &env,
            &path,
            &loader,
        )
        .unwrap();
        let result = expression.evaluate(&DynamicState::new(), &mut GenerationalGc::new());
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Enum(EnumTerm::new(
                    2,
                    vec![Expression::new(Term::Value(ValueTerm::Float(3.0)))],
                ))),
                DependencyList::empty(),
            ),
        );
        let expression = parse_module(
            "
            import { Types } from 'reflex::utils';
            export default Types.Enum({
                Static: Types.EnumVariant,
                Empty: Types.EnumVariant(),
                Single: Types.EnumVariant(Types.Int),
                Multiple: Types.EnumVariant(Types.Int, Types.Int, Types.Int),
            }).Multiple(3, 4, 5);
        ",
            &env,
            &path,
            &loader,
        )
        .unwrap();
        let result = expression.evaluate(&DynamicState::new(), &mut GenerationalGc::new());
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Enum(EnumTerm::new(
                    3,
                    vec![
                        Expression::new(Term::Value(ValueTerm::Float(3.0))),
                        Expression::new(Term::Value(ValueTerm::Float(4.0))),
                        Expression::new(Term::Value(ValueTerm::Float(5.0))),
                    ],
                ))),
                DependencyList::empty(),
            ),
        );
    }
}
