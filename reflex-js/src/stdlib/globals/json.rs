// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::any::TypeId;

use reflex::{
    core::{
        ApplicationTerm, Arity, Expression, LambdaTerm, NativeFunction, Signal, SignalTerm,
        StructTerm, Term, VariableTerm,
    },
    hash::{hash_object, HashId},
    stdlib::{
        collection::{vector::VectorTerm, CollectionTerm},
        signal::SignalType,
        value::{StringValue, ValueTerm},
    },
};
use reflex_json::parse;

use crate::{builtins::flatten_deep, stdlib::globals::create_struct};

pub(crate) fn global_json() -> Expression {
    create_struct(vec![
        (String::from("parse"), global_json_parse()),
        (String::from("stringify"), global_json_stringify()),
    ])
}

pub fn global_json_parse() -> Expression {
    Expression::new(Term::Native(NativeFunction::new(
        JsonParse::hash(),
        JsonParse::arity(),
        JsonParse::apply,
    )))
}
struct JsonParse {}
impl JsonParse {
    fn hash() -> HashId {
        hash_object(&TypeId::of::<Self>())
    }
    fn arity() -> Arity {
        Arity::from(1, 0, None)
    }
    fn apply(args: Vec<Expression>) -> Expression {
        let mut args = args.into_iter();
        let source = args.next().unwrap();
        let result = match source.value() {
            Term::Value(ValueTerm::String(value)) => parse(value),
            _ => Err(format!(
                "Invalid JSON.parse() call: expected string argument, received {}",
                source
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

pub fn global_json_stringify() -> Expression {
    Expression::new(Term::Lambda(LambdaTerm::new(
        Arity::from(0, 1, None),
        Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Native(NativeFunction::new(
                JsonStringify::hash(),
                JsonStringify::arity(),
                JsonStringify::apply,
            ))),
            vec![Expression::new(Term::Application(ApplicationTerm::new(
                flatten_deep(),
                vec![Expression::new(Term::Variable(VariableTerm::scoped(0)))],
            )))],
        ))),
    )))
}
struct JsonStringify {}
impl JsonStringify {
    fn hash() -> HashId {
        hash_object(&TypeId::of::<Self>())
    }
    fn arity() -> Arity {
        Arity::from(1, 0, None)
    }
    fn apply(args: Vec<Expression>) -> Expression {
        let mut args = args.into_iter();
        let source = args.next().unwrap();
        match json_stringify(source.value()) {
            Ok(result) => {
                Expression::new(Term::Value(ValueTerm::String(StringValue::from(result))))
            }
            Err(error) => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![Expression::new(Term::Value(ValueTerm::String(format!(
                    "Invalid JSON.stringify() call: unable to serialize {}",
                    error
                ))))],
            )))),
        }
    }
}

pub fn json_stringify(term: &Term) -> Result<String, &Term> {
    match term {
        Term::Value(value) => stringify_value_term(term, value),
        Term::Struct(value) => stringify_struct_term(term, value),
        Term::Collection(value) => match value {
            CollectionTerm::Vector(value) => stringify_vector_term(term, value),
            _ => Err(term),
        },
        _ => Err(term),
    }
}

fn stringify_value_term<'a>(input: &'a Term, value: &'a ValueTerm) -> Result<String, &'a Term> {
    match value {
        ValueTerm::Hash(_) | ValueTerm::Symbol(_) => Err(input),
        ValueTerm::Null => Ok(String::from("null")),
        ValueTerm::Boolean(value) => Ok(String::from(if *value { "true" } else { "false" })),
        ValueTerm::Int(value) => Ok(format!("{}", value)),
        ValueTerm::Float(value) => Ok(format!("{}", value)),
        ValueTerm::String(value) => Ok(json_stringify_string(value)),
    }
}

pub fn json_stringify_string(value: &str) -> String {
    let mut result = String::with_capacity(value.len() + 2);
    result.push('"');
    for current in value.chars() {
        let escape_char = match current {
            '\\' | '"' | '\'' => Some(current),
            '\n' | '\u{2028}' | '\u{2029}' => Some('n'),
            '\r' => Some('r'),
            _ => None,
        };
        match escape_char {
            Some(escaped) => {
                result.push('\\');
                result.push(escaped);
            }
            None => {
                result.push(current);
            }
        }
    }
    result.push('"');
    result
}

fn stringify_struct_term<'a>(input: &'a Term, value: &'a StructTerm) -> Result<String, &'a Term> {
    match value.prototype() {
        None => Err(input),
        Some(prototype) => {
            let fields = prototype
                .keys()
                .iter()
                .zip(value.fields())
                .map(|(key, value)| match key {
                    ValueTerm::String(key) => {
                        let value = json_stringify(value.value())?;
                        Ok(format!("{}:{}", json_stringify_string(&key), value))
                    }
                    _ => Err(input),
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(format!("{{{}}}", fields.join(",")))
        }
    }
}

fn stringify_vector_term<'a>(_input: &'a Term, value: &'a VectorTerm) -> Result<String, &'a Term> {
    let items = value
        .items()
        .iter()
        .map(|item| json_stringify(item.value()))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(format!("[{}]", items.join(",")))
}
