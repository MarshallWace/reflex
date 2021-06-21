// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::any::TypeId;

use reflex::{
    core::{
        ApplicationTerm, Arity, Expression, LambdaTerm, NativeFunction, Signal, SignalTerm,
        StructPrototype, StructTerm, Term, VarArgs, VariableTerm,
    },
    hash::{hash_object, HashId},
    stdlib::{
        builtin::BuiltinTerm,
        signal::SignalType,
        value::{StringValue, ValueTerm},
    },
};

use crate::{
    builtins::flatten_deep,
    stdlib::{global_json_parse, imports::create_struct},
};

pub(crate) fn import_http() -> Expression {
    create_struct(vec![
        ("fetch", import_http_fetch()),
        ("Request", import_http_request()),
    ])
}

fn import_http_fetch() -> Expression {
    Expression::new(Term::Lambda(LambdaTerm::new(
        Arity::from(0, 1, None),
        Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 1, None),
                Expression::new(Term::Application(ApplicationTerm::new(
                    Expression::new(Term::Lambda(LambdaTerm::new(
                        Arity::from(0, 1, None),
                        Expression::new(Term::Struct(StructTerm::new(
                            Some(StructPrototype::new(vec![
                                ValueTerm::String(StringValue::from("status")),
                                ValueTerm::String(StringValue::from("ok")),
                                ValueTerm::String(StringValue::from("text")),
                                ValueTerm::String(StringValue::from("json")),
                            ])),
                            vec![
                                Expression::new(Term::Application(ApplicationTerm::new(
                                    Expression::new(Term::Builtin(BuiltinTerm::Get)),
                                    vec![
                                        Expression::new(Term::Variable(VariableTerm::scoped(0))),
                                        Expression::new(Term::Value(ValueTerm::Int(0))),
                                    ],
                                ))),
                                Expression::new(Term::Application(ApplicationTerm::new(
                                    Expression::new(Term::Builtin(BuiltinTerm::Lt)),
                                    vec![
                                        Expression::new(Term::Application(ApplicationTerm::new(
                                            Expression::new(Term::Builtin(BuiltinTerm::Get)),
                                            vec![
                                                Expression::new(Term::Variable(
                                                    VariableTerm::scoped(0),
                                                )),
                                                Expression::new(Term::Value(ValueTerm::Int(0))),
                                            ],
                                        ))),
                                        Expression::new(Term::Value(ValueTerm::Int(400))),
                                    ],
                                ))),
                                Expression::new(Term::Lambda(LambdaTerm::new(
                                    Arity::from(0, 0, None),
                                    Expression::new(Term::Application(ApplicationTerm::new(
                                        Expression::new(Term::Builtin(BuiltinTerm::Get)),
                                        vec![
                                            Expression::new(Term::Variable(VariableTerm::scoped(
                                                0,
                                            ))),
                                            Expression::new(Term::Value(ValueTerm::Int(1))),
                                        ],
                                    ))),
                                ))),
                                Expression::new(Term::Lambda(LambdaTerm::new(
                                    Arity::from(0, 0, None),
                                    Expression::new(Term::Application(ApplicationTerm::new(
                                        global_json_parse(),
                                        vec![Expression::new(Term::Application(
                                            ApplicationTerm::new(
                                                Expression::new(Term::Builtin(BuiltinTerm::Get)),
                                                vec![
                                                    Expression::new(Term::Variable(
                                                        VariableTerm::scoped(0),
                                                    )),
                                                    Expression::new(Term::Value(ValueTerm::Int(1))),
                                                ],
                                            ),
                                        ))],
                                    ))),
                                ))),
                            ],
                        ))),
                    ))),
                    vec![Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Builtin(BuiltinTerm::Effect)),
                        vec![
                            Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                "reflex::http::fetch",
                            )))),
                            Expression::new(Term::Application(ApplicationTerm::new(
                                Expression::new(Term::Builtin(BuiltinTerm::Get)),
                                vec![
                                    Expression::new(Term::Variable(VariableTerm::scoped(0))),
                                    Expression::new(Term::Value(ValueTerm::String(
                                        StringValue::from("url"),
                                    ))),
                                ],
                            ))),
                            Expression::new(Term::Application(ApplicationTerm::new(
                                Expression::new(Term::Builtin(BuiltinTerm::Get)),
                                vec![
                                    Expression::new(Term::Variable(VariableTerm::scoped(0))),
                                    Expression::new(Term::Value(ValueTerm::String(
                                        StringValue::from("method"),
                                    ))),
                                ],
                            ))),
                            Expression::new(Term::Application(ApplicationTerm::new(
                                flatten_deep(),
                                vec![Expression::new(Term::Application(ApplicationTerm::new(
                                    Expression::new(Term::Builtin(BuiltinTerm::Get)),
                                    vec![
                                        Expression::new(Term::Variable(VariableTerm::scoped(0))),
                                        Expression::new(Term::Value(ValueTerm::String(
                                            StringValue::from("headers"),
                                        ))),
                                    ],
                                )))],
                            ))),
                            Expression::new(Term::Application(ApplicationTerm::new(
                                Expression::new(Term::Builtin(BuiltinTerm::Get)),
                                vec![
                                    Expression::new(Term::Variable(VariableTerm::scoped(0))),
                                    Expression::new(Term::Value(ValueTerm::String(
                                        StringValue::from("body"),
                                    ))),
                                ],
                            ))),
                        ],
                    )))],
                ))),
            ))),
            vec![Expression::new(Term::Application(ApplicationTerm::new(
                to_request(),
                vec![Expression::new(Term::Variable(VariableTerm::scoped(0)))],
            )))],
        ))),
    )))
}

fn import_http_request() -> Expression {
    Expression::new(Term::StructConstructor(
        VarArgs::Eager,
        http_request_prototype(),
    ))
}

fn http_request_prototype() -> StructPrototype {
    StructPrototype::new(vec![
        ValueTerm::String(StringValue::from("url")),
        ValueTerm::String(StringValue::from("method")),
        ValueTerm::String(StringValue::from("headers")),
        ValueTerm::String(StringValue::from("body")),
    ])
}

fn to_request() -> Expression {
    Expression::new(Term::Native(NativeFunction::new(
        ToRequest::hash(),
        ToRequest::arity(),
        ToRequest::apply,
    )))
}
struct ToRequest {}
impl ToRequest {
    fn arity() -> Arity {
        Arity::from(1, 0, None)
    }
    fn hash() -> HashId {
        hash_object(&TypeId::of::<Self>())
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
        let target = args.next().unwrap();
        let result = match target.value() {
            Term::Value(ValueTerm::String(_)) => {
                let method =
                    Expression::new(Term::Value(ValueTerm::String(StringValue::from("GET"))));
                let headers = Expression::new(Term::Struct(StructTerm::new(
                    Some(StructPrototype::new(Vec::new())),
                    Vec::new(),
                )));
                let body = Expression::new(Term::Value(ValueTerm::Null));
                Ok(Expression::new(Term::Struct(StructTerm::new(
                    Some(http_request_prototype()),
                    vec![target, method, headers, body],
                ))))
            }
            Term::Struct(value) => match value.prototype() {
                Some(prototype) => {
                    let request_prototype = http_request_prototype();
                    if hash_object(&prototype) == hash_object(&request_prototype) {
                        Ok(target)
                    } else {
                        let request = request_prototype.apply(
                            prototype
                                .keys()
                                .iter()
                                .zip(value.fields().iter().map(Expression::clone)),
                        );
                        match request {
                            Some(request) => Ok(Expression::new(Term::Struct(request))),
                            _ => Err(format!("{}", target)),
                        }
                    }
                }
                _ => Err(format!("{}", target)),
            },
            _ => Err(format!("{}", target)),
        };
        match result {
            Ok(result) => result,
            Err(error) => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![Expression::new(Term::Value(ValueTerm::String(format!(
                    "Expected HTTP request, received {}",
                    error
                ))))],
            )))),
        }
    }
}
