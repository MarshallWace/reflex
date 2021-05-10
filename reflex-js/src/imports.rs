// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{any::TypeId, iter::once};

use reflex::{
    core::{
        ApplicationTerm, Arity, EnumConstructorArity, EnumIndex, EnumVariantPrototype, Expression,
        LambdaTerm, NativeFunction, RecursiveTerm, Signal, SignalTerm, StaticVariableTerm,
        StructPrototype, StructTerm, Term, VarArgs, VariableTerm,
    },
    hash::{hash_object, HashId},
    stdlib::{
        signal::SignalType,
        value::{StringValue, ValueTerm},
    },
};

pub fn builtin_imports() -> Vec<(&'static str, Expression)> {
    vec![
        ("reflex::utils", import_utils()),
        ("reflex::graphql", import_graphql()),
    ]
}

fn import_utils() -> Expression {
    create_struct(vec![
        ("Types", import_type_constructors()),
        (
            "graph",
            Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 1, None),
                Expression::new(Term::Recursive(RecursiveTerm::new(Expression::new(
                    Term::Variable(VariableTerm::Static(StaticVariableTerm::new(0))),
                )))),
            ))),
        ),
    ])
}

fn import_type_constructors() -> Expression {
    let null = Expression::new(Term::Value(ValueTerm::Null));
    create_struct(vec![
        (
            "Scalar",
            create_struct(vec![
                ("Boolean", Expression::clone(&null)),
                ("Int", Expression::clone(&null)),
                ("Float", Expression::clone(&null)),
                ("String", Expression::clone(&null)),
            ]),
        ),
        (
            "Struct",
            Expression::new(Term::Native(NativeFunction::new(
                StructTypeConstructor::hash(),
                StructTypeConstructor::arity(),
                StructTypeConstructor::apply,
            ))),
        ),
        (
            "Enum",
            Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(1, 0, None),
                Expression::new(Term::Application(ApplicationTerm::new(
                    Expression::new(Term::Native(NativeFunction::new(
                        StructFlattener::hash(),
                        StructFlattener::arity(),
                        StructFlattener::apply,
                    ))),
                    vec![
                        Expression::new(Term::Variable(VariableTerm::Static(
                            StaticVariableTerm::new(0),
                        ))),
                        Expression::new(Term::Native(NativeFunction::new(
                            EnumTypeConstructor::hash(),
                            EnumTypeConstructor::arity(),
                            EnumTypeConstructor::apply,
                        ))),
                    ],
                ))),
            ))),
        ),
        (
            "EnumVariant",
            Expression::new(Term::Native(NativeFunction::new(
                EnumVariantConstructor::hash(),
                EnumVariantConstructor::arity(),
                EnumVariantConstructor::apply,
            ))),
        ),
        (
            "Fn",
            Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 0, Some(VarArgs::Eager)),
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(1, 0, None),
                    Expression::clone(&null),
                ))),
            ))),
        ),
        (
            "List",
            Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(1, 0, None),
                Expression::clone(&null),
            ))),
        ),
        (
            "Maybe",
            Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(1, 0, None),
                Expression::clone(&null),
            ))),
        ),
    ])
}

struct StructTypeConstructor {}
impl StructTypeConstructor {
    fn hash() -> HashId {
        hash_object(TypeId::of::<Self>())
    }
    fn arity() -> Arity {
        Arity::from(1, 0, None)
    }
    fn apply(args: Vec<Expression>) -> Expression {
        if args.len() != 1 {
            return Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![ValueTerm::String(format!(
                    "Expected 1 argument, received {}",
                    args.len(),
                ))],
            ))));
        }
        let mut args = args.into_iter();
        let shape = args.next().unwrap();
        match shape.value() {
            Term::Struct(shape) => match shape.prototype() {
                Some(prototype) => Expression::new(Term::StructConstructor(prototype.clone())),
                None => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                    SignalType::Error,
                    vec![ValueTerm::String(format!(
                        "Expected named struct shape definition fields, received, {}",
                        shape,
                    ))],
                )))),
            },
            _ => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![ValueTerm::String(format!(
                    "Expected struct shape definition, received {}",
                    shape,
                ))],
            )))),
        }
    }
}

struct StructFlattener {}
impl StructFlattener {
    fn hash() -> HashId {
        hash_object(TypeId::of::<Self>())
    }
    fn arity() -> Arity {
        Arity::from(1, 1, None)
    }
    fn apply(args: Vec<Expression>) -> Expression {
        if args.len() != 2 {
            return Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![ValueTerm::String(format!(
                    "Expected 2 arguments, received {}",
                    args.len(),
                ))],
            ))));
        }
        let mut args = args.into_iter();
        let input = args.next().unwrap();
        let transform = args.next().unwrap();
        match input.value() {
            Term::Struct(input) => match input.prototype() {
                Some(prototype) => Expression::new(Term::Application(ApplicationTerm::new(
                    transform,
                    once(Expression::new(Term::StructConstructor(prototype.clone())))
                        .chain(input.fields().into_iter().map(Expression::clone))
                        .collect(),
                ))),
                None => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                    SignalType::Error,
                    vec![ValueTerm::String(format!(
                        "Expected named struct fields, received anonymous struct {}",
                        input,
                    ))],
                )))),
            },
            _ => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![ValueTerm::String(format!(
                    "Expected <struct>, received {}",
                    input,
                ))],
            )))),
        }
    }
}

struct EnumTypeConstructor {}
impl EnumTypeConstructor {
    fn hash() -> HashId {
        hash_object(TypeId::of::<Self>())
    }
    fn arity() -> Arity {
        Arity::from(1, 0, Some(VarArgs::Eager))
    }
    fn apply(args: Vec<Expression>) -> Expression {
        if args.len() < 1 {
            return Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![ValueTerm::String(format!(
                    "Expected 1 or more arguments, received {}",
                    args.len(),
                ))],
            ))));
        }
        let mut args = args.into_iter();
        let shape = args.next().unwrap();
        match shape.value() {
            Term::StructConstructor(keys) => {
                let num_variants = args.len();
                let variants = args.enumerate().fold(
                    Ok(Vec::with_capacity(num_variants)),
                    |results, (index, variant)| {
                        let mut variants = results?;
                        match variant.value() {
                            Term::EnumConstructor(prototype) => {
                                variants.push(Expression::new(Term::EnumConstructor(
                                    EnumVariantPrototype::new(
                                        index as EnumIndex,
                                        prototype.arity(),
                                    ),
                                )));
                                Ok(variants)
                            }
                            _ => Err(format!(
                                "Expected enum variant definition, received {}",
                                variant
                            )),
                        }
                    },
                );
                match variants {
                    Ok(variants) => {
                        Expression::new(Term::Struct(StructTerm::new(Some(keys.clone()), variants)))
                    }
                    Err(error) => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                        SignalType::Error,
                        vec![ValueTerm::String(error)],
                    )))),
                }
            }
            _ => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![ValueTerm::String(format!(
                    "Expected enum definition, received {}",
                    shape,
                ))],
            )))),
        }
    }
}
struct EnumVariantConstructor {}
impl EnumVariantConstructor {
    fn hash() -> HashId {
        hash_object(TypeId::of::<Self>())
    }
    fn arity() -> Arity {
        Arity::from(0, 0, Some(VarArgs::Eager))
    }
    fn apply(args: Vec<Expression>) -> Expression {
        let num_args = args.len() as EnumConstructorArity;
        Expression::new(Term::EnumConstructor(EnumVariantPrototype::new(
            0, num_args,
        )))
    }
}

fn import_graphql() -> Expression {
    create_struct(vec![(
        "Resolver",
        Expression::new(Term::StructConstructor(StructPrototype::new(vec![
            Expression::new(Term::Value(ValueTerm::String(StringValue::from("query")))),
            Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                "mutation",
            )))),
            Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                "subscription",
            )))),
        ]))),
    )])
}

fn create_struct<'a>(fields: impl IntoIterator<Item = (&'a str, Expression)>) -> Expression {
    let (keys, values) = fields
        .into_iter()
        .map(|(key, value)| {
            (
                Expression::new(Term::Value(ValueTerm::String(StringValue::from(key)))),
                value,
            )
        })
        .unzip();
    Expression::new(Term::Struct(StructTerm::new(
        Some(StructPrototype::new(keys)),
        values,
    )))
}
