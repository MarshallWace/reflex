// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::any::TypeId;

use reflex::{
    core::{
        ApplicationTerm, Arity, Expression, LambdaTerm, NativeFunction, Signal, SignalTerm,
        StaticVariableTerm, StructPrototype, StructTerm, Term, VariableTerm,
    },
    hash::{hash_object, HashId},
    stdlib::{
        builtin::BuiltinTerm,
        collection::{
            hashmap::HashMapTerm, hashset::HashSetTerm, vector::VectorTerm, CollectionTerm,
        },
        signal::SignalType,
        value::{StringValue, ValueTerm},
    },
};

use crate::{parse, Env};

pub fn builtin_globals() -> Vec<(&'static str, Expression)> {
    vec![
        ("Boolean", builtin_global_boolean()),
        ("Math", builtin_global_math()),
        ("Map", builtin_global_map_constructor()),
        ("Set", builtin_global_set_constructor()),
        ("JSON", builtin_global_json()),
        ("fetch", builtin_fetch()),
    ]
}

pub fn builtin_process(
    env_vars: impl IntoIterator<Item = (&'static str, Expression)>,
) -> Expression {
    create_struct(vec![("env", create_struct(env_vars))])
}

fn builtin_global_boolean() -> Expression {
    Expression::new(Term::Lambda(LambdaTerm::new(
        Arity::from(1, 0, None),
        Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::If)),
            vec![
                Expression::new(Term::Variable(VariableTerm::Static(
                    StaticVariableTerm::new(0),
                ))),
                Expression::new(Term::Value(ValueTerm::Boolean(true))),
                Expression::new(Term::Value(ValueTerm::Boolean(false))),
            ],
        ))),
    )))
}

fn builtin_global_math() -> Expression {
    create_struct(vec![
        ("abs", Expression::new(Term::Builtin(BuiltinTerm::Abs))),
        ("ceil", Expression::new(Term::Builtin(BuiltinTerm::Ceil))),
        ("floor", Expression::new(Term::Builtin(BuiltinTerm::Floor))),
        ("max", Expression::new(Term::Builtin(BuiltinTerm::Max))),
        ("min", Expression::new(Term::Builtin(BuiltinTerm::Min))),
        ("round", Expression::new(Term::Builtin(BuiltinTerm::Round))),
    ])
}

fn builtin_global_map_constructor() -> Expression {
    Expression::new(Term::Native(NativeFunction::new(
        MapConstructor::hash(),
        MapConstructor::arity(),
        MapConstructor::apply,
    )))
}
struct MapConstructor {}
impl MapConstructor {
    fn hash() -> HashId {
        hash_object(TypeId::of::<Self>())
    }
    fn arity() -> Arity {
        Arity::from(1, 0, None)
    }
    fn apply(args: Vec<Expression>) -> Expression {
        let mut args = args.into_iter();
        let entries = args.next().unwrap();
        let entries = match entries.value() {
            Term::Collection(CollectionTerm::Vector(entries)) => Ok(entries),
            _ => Err(format!("Invalid Map constructor: {}", entries)),
        };
        match entries {
            Ok(entries) => {
                let entries = entries.iterate().into_iter().collect::<Vec<_>>();
                let has_dynamic_keys = entries.iter().any(|entry| match entry.value() {
                    Term::Struct(entry) => match entry.fields().get(0) {
                        Some(key) => match key.value() {
                            Term::Value(_) => false,
                            _ => true,
                        },
                        _ => true,
                    },
                    Term::Collection(entry) => match entry {
                        CollectionTerm::Vector(entry) => match entry.get(0) {
                            Some(key) => match key.value() {
                                Term::Value(_) => false,
                                _ => true,
                            },
                            _ => true,
                        },
                        _ => true,
                    },
                    _ => true,
                });
                match has_dynamic_keys {
                    false => HashMapTerm::collect(entries),
                    true => {
                        let get = Expression::new(Term::Builtin(BuiltinTerm::Get));
                        let first = Expression::new(Term::Value(ValueTerm::Int(0)));
                        let second = Expression::new(Term::Value(ValueTerm::Int(1)));
                        let (keys, values): (Vec<_>, Vec<_>) = entries
                            .into_iter()
                            .map(|entry| {
                                (
                                    Expression::new(Term::Application(ApplicationTerm::new(
                                        Expression::clone(&get),
                                        vec![Expression::clone(&entry), Expression::clone(&first)],
                                    ))),
                                    Expression::new(Term::Application(ApplicationTerm::new(
                                        Expression::clone(&get),
                                        vec![entry, Expression::clone(&second)],
                                    ))),
                                )
                            })
                            .unzip();
                        Expression::new(Term::Application(ApplicationTerm::new(
                            dynamic_map_constructor(),
                            vec![
                                Expression::new(Term::Application(ApplicationTerm::new(
                                    Expression::new(Term::Builtin(BuiltinTerm::CollectArgs)),
                                    keys,
                                ))),
                                Expression::new(Term::Collection(CollectionTerm::Vector(
                                    VectorTerm::new(values),
                                ))),
                            ],
                        )))
                    }
                }
            }
            Err(error) => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![ValueTerm::String(error)],
            )))),
        }
    }
}

fn dynamic_map_constructor() -> Expression {
    Expression::new(Term::Native(NativeFunction::new(
        DynamicMapConstructor::hash(),
        DynamicMapConstructor::arity(),
        DynamicMapConstructor::apply,
    )))
}
struct DynamicMapConstructor {}
impl DynamicMapConstructor {
    fn hash() -> HashId {
        hash_object(TypeId::of::<Self>())
    }
    fn arity() -> Arity {
        Arity::from(2, 0, None)
    }
    fn apply(args: Vec<Expression>) -> Expression {
        let mut args = args.into_iter();
        let keys = args.next().unwrap();
        let values = args.next().unwrap();
        let keys = match keys.value() {
            Term::Collection(CollectionTerm::Vector(keys)) => Some(keys),
            _ => None,
        };
        let values = match values.value() {
            Term::Collection(CollectionTerm::Vector(values)) => Some(values),
            _ => None,
        };
        match (keys, values) {
            (Some(keys), Some(values)) if keys.len() == values.len() => {
                Expression::new(Term::Collection(CollectionTerm::HashMap(HashMapTerm::new(
                    keys.iterate().into_iter().zip(values.iterate().into_iter()),
                ))))
            }
            _ => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![ValueTerm::String(StringValue::from(
                    "Invalid Map constructor values",
                ))],
            )))),
        }
    }
}

fn builtin_global_set_constructor() -> Expression {
    Expression::new(Term::Native(NativeFunction::new(
        SetConstructor::hash(),
        SetConstructor::arity(),
        SetConstructor::apply,
    )))
}
struct SetConstructor {}
impl SetConstructor {
    fn hash() -> HashId {
        hash_object(TypeId::of::<Self>())
    }
    fn arity() -> Arity {
        Arity::from(1, 0, None)
    }
    fn apply(args: Vec<Expression>) -> Expression {
        let mut args = args.into_iter();
        let entries = args.next().unwrap();
        let entries = match entries.value() {
            Term::Collection(CollectionTerm::Vector(entries)) => Ok(entries),
            _ => Err(format!("Invalid Set constructor: {}", entries)),
        };
        match entries {
            Ok(entries) => {
                let entries = entries.iterate().into_iter().collect::<Vec<_>>();
                let has_dynamic_keys = entries.iter().any(|entry| match entry.value() {
                    Term::Value(_) => false,
                    _ => true,
                });
                match has_dynamic_keys {
                    false => HashSetTerm::collect(entries),
                    true => Expression::new(Term::Application(ApplicationTerm::new(
                        dynamic_set_constructor(),
                        vec![Expression::new(Term::Application(ApplicationTerm::new(
                            Expression::new(Term::Builtin(BuiltinTerm::CollectArgs)),
                            entries,
                        )))],
                    ))),
                }
            }
            Err(error) => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![ValueTerm::String(error)],
            )))),
        }
    }
}

fn dynamic_set_constructor() -> Expression {
    Expression::new(Term::Native(NativeFunction::new(
        DynamicSetConstructor::hash(),
        DynamicSetConstructor::arity(),
        DynamicSetConstructor::apply,
    )))
}
struct DynamicSetConstructor {}
impl DynamicSetConstructor {
    fn hash() -> HashId {
        hash_object(TypeId::of::<Self>())
    }
    fn arity() -> Arity {
        Arity::from(1, 0, None)
    }
    fn apply(args: Vec<Expression>) -> Expression {
        let mut args = args.into_iter();
        let entries = args.next().unwrap();
        let entries = match entries.value() {
            Term::Collection(CollectionTerm::Vector(entries)) => Some(entries),
            _ => None,
        };
        match entries {
            Some(entries) => Expression::new(Term::Collection(CollectionTerm::HashSet(
                HashSetTerm::new(entries.iterate().into_iter()),
            ))),
            _ => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![ValueTerm::String(StringValue::from(
                    "Invalid Set constructor values",
                ))],
            )))),
        }
    }
}

fn builtin_global_json() -> Expression {
    create_struct(vec![
        ("parse", builtin_global_json_parse()),
        ("stringify", builtin_global_json_stringify()),
    ])
}

fn builtin_global_json_parse() -> Expression {
    Expression::new(Term::Native(NativeFunction::new(
        JsonParse::hash(),
        JsonParse::arity(),
        JsonParse::apply,
    )))
}
struct JsonParse {}
impl JsonParse {
    fn hash() -> HashId {
        hash_object(TypeId::of::<Self>())
    }
    fn arity() -> Arity {
        Arity::from(1, 0, None)
    }
    fn apply(args: Vec<Expression>) -> Expression {
        let mut args = args.into_iter();
        let source = args.next().unwrap();
        let result = match source.value() {
            Term::Value(ValueTerm::String(value)) => {
                // TODO: Implement proper JSON parser
                parse(&format!("({})", value), &Env::new())
            }
            _ => Err(format!(
                "Invalid JSON.parse() call: expected string argument, received {}",
                source
            )),
        };
        match result {
            Ok(result) => result,
            Err(error) => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![ValueTerm::String(error)],
            )))),
        }
    }
}

fn builtin_global_json_stringify() -> Expression {
    Expression::new(Term::Native(NativeFunction::new(
        JsonStringify::hash(),
        JsonStringify::arity(),
        JsonStringify::apply,
    )))
}
struct JsonStringify {}
impl JsonStringify {
    fn hash() -> HashId {
        hash_object(TypeId::of::<Self>())
    }
    fn arity() -> Arity {
        Arity::from(1, 0, None)
    }
    fn apply(args: Vec<Expression>) -> Expression {
        let mut args = args.into_iter();
        // TODO: Flatten nested fields before JSON stringifying
        let source = args.next().unwrap();
        match stringify(source.value()) {
            Ok(result) => {
                Expression::new(Term::Value(ValueTerm::String(StringValue::from(result))))
            }
            Err(error) => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![ValueTerm::String(format!(
                    "Invalid JSON.stringify() call: unable to serialize {}",
                    error
                ))],
            )))),
        }
    }
}

pub fn stringify(term: &Term) -> Result<String, &Term> {
    match term {
        Term::Value(value) => stringify_value_term(term, value),
        Term::Struct(value) => stringify_struct_term(term, value),
        Term::Collection(value) => stringify_collection_term(term, value),
        _ => Err(term),
    }
}

fn stringify_value_term<'a>(input: &'a Term, value: &'a ValueTerm) -> Result<String, &'a Term> {
    match value {
        ValueTerm::Null => Ok(String::from("null")),
        ValueTerm::Boolean(value) => Ok(String::from(if *value { "true" } else { "false" })),
        ValueTerm::Int(value) => Ok(format!("{}", value)),
        ValueTerm::Float(value) => Ok(format!("{}", value)),
        ValueTerm::String(value) => Ok(stringify_string_value(value)),
        ValueTerm::Array(items) => {
            let items = items
                .iter()
                .map(|value| stringify_value_term(input, value))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(format!("[{}]", items.join(",")))
        }
        ValueTerm::Symbol(_) => Err(input),
    }
}

fn stringify_string_value(value: &str) -> String {
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
                .map(|(key, value)| match key.value() {
                    Term::Value(ValueTerm::String(key)) => {
                        let value = stringify(value.value())?;
                        Ok(format!("{}:{}", stringify_string_value(&key), value))
                    }
                    key => Err(key),
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(format!("{{{}}}", fields.join(",")))
        }
    }
}

fn stringify_collection_term<'a>(
    input: &'a Term,
    value: &'a CollectionTerm,
) -> Result<String, &'a Term> {
    match value {
        CollectionTerm::Vector(value) => stringify_vector_term(input, value),
        _ => Err(input),
    }
}

fn stringify_vector_term<'a>(_input: &'a Term, value: &'a VectorTerm) -> Result<String, &'a Term> {
    let items = value
        .items()
        .iter()
        .map(|item| stringify(item.value()))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(format!("[{}]", items.join(",")))
}

fn builtin_fetch() -> Expression {
    Expression::new(Term::Lambda(LambdaTerm::new(
        Arity::from(0, 1, None),
        Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Effect)),
            vec![
                Expression::new(Term::Value(ValueTerm::String(StringValue::from("fetch")))),
                Expression::new(Term::Variable(VariableTerm::Static(
                    StaticVariableTerm::new(0),
                ))),
            ],
        ))),
    )))
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
