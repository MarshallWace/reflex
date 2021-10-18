// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{iter::once, str::FromStr};

use reflex::{
    core::{
        ArgType, Arity, Expression, ExpressionFactory, ExpressionList, FunctionArity,
        HeapAllocator, NativeAllocator, Signal, SignalType, StringValue,
    },
    lang::{BuiltinTerm, NativeFunction, ValueTerm},
};
use uuid::Uuid;

use crate::stdlib::{
    globals::{
        json::{json_parse, json_stringify},
        map::map_constructor,
        object::from_entries,
        set::set_constructor,
        string::encode_uri_component,
    },
    imports::{struct_type_factory, to_request},
};

pub fn builtin_plugins<T: Expression>() -> impl IntoIterator<Item = NativeFunction<T>> {
    vec![
        construct(),
        dispatch(),
        encode_uri_component(),
        from_entries(),
        format_error_message(),
        json_parse(),
        json_stringify(),
        map_constructor(),
        set_constructor(),
        struct_type_factory(),
        throw(),
        to_request(),
        to_string(),
    ]
}

pub fn throw<T: Expression>() -> NativeFunction<T> {
    NativeFunction::new_with_uuid(Throw::uid(), Throw::name(), Throw::arity(), Throw::apply)
}
struct Throw {}
impl Throw {
    const NAME: &'static str = "Throw";
    const UUID: &'static str = "fb9bef4b-da7a-46ef-af03-50ed2984274c";
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
        let error = args.next().unwrap();
        if !error.is_static() {
            return Err(String::from(
                "Thrown exceptions cannot contain dynamic values",
            ));
        }
        let signals = if let Some(errors) = parse_aggregate_error(&error, &factory) {
            allocator.create_signal_list(
                errors
                    .iter()
                    .cloned()
                    .map(|error| create_error_signal(error, &allocator))
                    .collect(),
            )
        } else {
            allocator.create_signal_list(once(create_error_signal(error, &allocator)).collect())
        };
        Ok(factory.create_signal_term(signals))
    }
}
fn parse_aggregate_error<'a, T: Expression + 'a>(
    target: &'a T,
    factory: &impl ExpressionFactory<T>,
) -> Option<&'a ExpressionList<T>> {
    factory.match_struct_term(target).and_then(|target| {
        target.get("name").and_then(|error_type| {
            factory.match_value_term(error_type).and_then(|error_type| {
                error_type.match_string().and_then(|name| {
                    if name.as_str() == "AggregateError" {
                        target.get("errors").and_then(|errors| {
                            factory
                                .match_vector_term(errors)
                                .map(|errors| errors.items())
                        })
                    } else {
                        None
                    }
                })
            })
        })
    })
}
fn create_error_signal<T: Expression>(error: T, allocator: &impl HeapAllocator<T>) -> Signal<T> {
    allocator.create_signal(SignalType::Error, allocator.create_unit_list(error))
}

pub fn construct<T: Expression>() -> NativeFunction<T> {
    NativeFunction::new_with_uuid(
        Construct::uid(),
        Construct::name(),
        Construct::arity(),
        Construct::apply,
    )
}
struct Construct {}
impl Construct {
    const NAME: &'static str = "Construct";
    const UUID: &'static str = "5a68dc6b-71fa-407f-8039-bcd87323f2bf";
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: Some(ArgType::Lazy),
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
        let target = args.next().unwrap();
        if let Some(constructor) = factory.match_constructor_term(&target) {
            let prototype = constructor.prototype();
            let properties = args.next().unwrap();
            prototype
                .parse_struct(&properties, &factory, &allocator)
                .ok_or_else(|| format!("Invalid constructor call: {} {}", constructor, properties))
        } else {
            Ok(factory.create_application_term(target, allocator.create_list(args.collect())))
        }
    }
}

pub fn dispatch<T: Expression>() -> NativeFunction<T> {
    NativeFunction::new_with_uuid(
        Dispatch::uid(),
        Dispatch::name(),
        Dispatch::arity(),
        Dispatch::apply,
    )
}
struct Dispatch {}
impl Dispatch {
    const NAME: &'static str = "Dispatch";
    const UUID: &'static str = "f37c66ee-cd73-496d-8420-1ce83ab924ad";
    const ARITY: FunctionArity<3, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict, ArgType::Lazy],
        optional: [],
        variadic: Some(ArgType::Lazy),
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
        let target = args.next().unwrap();
        let method_name = args.next().unwrap();
        let fallback = args.next().unwrap();
        let builtin_method = match factory.match_value_term(&method_name) {
            Some(ValueTerm::String(method_name)) => {
                get_builtin_field(Some(&target), method_name.as_str(), &factory)
            }
            _ => None,
        };
        match builtin_method {
            Some(method) => Ok(factory.create_application_term(
                method,
                allocator.create_list(once(target).chain(args).collect()),
            )),
            None => Ok(fallback),
        }
    }
}

pub fn to_string<T: Expression>() -> NativeFunction<T> {
    NativeFunction::new_with_uuid(
        ToString::uid(),
        ToString::name(),
        ToString::arity(),
        ToString::apply,
    )
}
struct ToString {}
impl ToString {
    const NAME: &'static str = "ToString";
    const UUID: &'static str = "7f651286-8d00-4854-a956-0a54dfe662d0";
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: Some(ArgType::Lazy),
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
        if args.len() != 1 {
            return Err(format!("Expected 1 argument, received {}", args.len(),));
        }
        let operand = args.next().unwrap();
        match factory.match_value_term(&operand) {
            Some(ValueTerm::String(_)) => Ok(operand),
            Some(value) => Ok(factory.create_value_term(ValueTerm::String(
                allocator.create_string(format_value(value)),
            ))),
            _ => Err(format!("Expected printable value, received {}", operand)),
        }
    }
}

pub fn format_error_message<T: Expression>() -> NativeFunction<T> {
    NativeFunction::new_with_uuid(
        FormatErrorMessage::uid(),
        FormatErrorMessage::name(),
        FormatErrorMessage::arity(),
        FormatErrorMessage::apply,
    )
}
const UNKNOWN_ERROR_MESSAGE: &'static str = "Unknown error";
struct FormatErrorMessage {}
impl FormatErrorMessage {
    const NAME: &'static str = "FormatErrorMessage";
    const UUID: &'static str = "3f88d05d-47f1-4b49-b16e-b8dc2f4ee61c";
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: Some(ArgType::Lazy),
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
        if args.len() != 1 {
            return Err(format!("Expected 1 argument, received {}", args.len(),));
        }
        let operand = args.next().unwrap();
        let message = parse_error_message(&operand, &factory)
            .or_else(|| {
                if let Some(value) = factory.match_vector_term(&operand) {
                    let max_displayed_errors = 10;
                    let num_errors = value.items().len();
                    let messages = value
                        .items()
                        .iter()
                        .take(if num_errors > max_displayed_errors {
                            max_displayed_errors - 1
                        } else {
                            num_errors
                        })
                        .map(|item| {
                            parse_error_message(item, &factory)
                                .unwrap_or_else(|| String::from(UNKNOWN_ERROR_MESSAGE))
                        });
                    Some(
                        messages
                            .chain(if num_errors > max_displayed_errors {
                                Some(format!(
                                    "...{} more errors",
                                    num_errors - max_displayed_errors - 1
                                ))
                            } else {
                                None
                            })
                            .collect::<Vec<_>>()
                            .join("\n"),
                    )
                } else {
                    None
                }
            })
            .unwrap_or_else(|| String::from(UNKNOWN_ERROR_MESSAGE));
        Ok(factory.create_value_term(ValueTerm::String(allocator.create_string(message))))
    }
}
fn parse_error_message<T: Expression>(
    target: &T,
    factory: &impl ExpressionFactory<T>,
) -> Option<String> {
    if let Some(value) = factory.match_value_term(&target) {
        Some(format_value(value))
    } else if let Some(value) = factory.match_struct_term(&target) {
        value.get("message").and_then(|value| {
            factory.match_value_term(value).and_then(|value| {
                value
                    .match_string()
                    .map(|value| String::from(value.as_str()))
            })
        })
    } else {
        None
    }
}

pub(crate) fn get_builtin_field<T: Expression>(
    target: Option<&T>,
    method: &str,
    factory: &impl ExpressionFactory<T>,
) -> Option<T> {
    None.or_else(|| {
        if target.is_none()
            || target
                .map(|target| match_string_value_term(target, factory))
                .is_some()
        {
            get_builtin_string_field(method, factory)
        } else {
            None
        }
    })
    .or_else(|| {
        if target.is_none()
            || target
                .map(|target| factory.match_vector_term(target))
                .is_some()
        {
            get_builtin_vector_field(method, factory)
        } else {
            None
        }
    })
    .or_else(|| {
        if target.is_none()
            || target
                .map(|target| factory.match_hashmap_term(target))
                .is_some()
        {
            get_builtin_hashmap_field(method, factory)
        } else {
            None
        }
    })
    .or_else(|| {
        if target.is_none()
            || target
                .map(|target| factory.match_hashset_term(target))
                .is_some()
        {
            get_builtin_hashset_field(method, factory)
        } else {
            None
        }
    })
}

fn get_builtin_string_field<T: Expression>(
    method: &str,
    factory: &impl ExpressionFactory<T>,
) -> Option<T> {
    match method {
        "replace" => Some(factory.create_builtin_term(BuiltinTerm::Replace)),
        "split" => Some(factory.create_builtin_term(BuiltinTerm::Split)),
        _ => None,
    }
}

fn get_builtin_vector_field<T: Expression>(
    method: &str,
    factory: &impl ExpressionFactory<T>,
) -> Option<T> {
    match method {
        "entries" => Some(factory.create_builtin_term(BuiltinTerm::Entries)),
        "filter" => Some(factory.create_builtin_term(BuiltinTerm::Filter)),
        "keys" => Some(factory.create_builtin_term(BuiltinTerm::Keys)),
        "map" => Some(factory.create_builtin_term(BuiltinTerm::Map)),
        "push" => Some(factory.create_builtin_term(BuiltinTerm::Push)),
        "reduce" => Some(factory.create_builtin_term(BuiltinTerm::Reduce)),
        "slice" => Some(factory.create_builtin_term(BuiltinTerm::Slice)),
        "unshift" => Some(factory.create_builtin_term(BuiltinTerm::PushFront)),
        "values" => Some(factory.create_builtin_term(BuiltinTerm::Values)),
        _ => None,
    }
}

fn get_builtin_hashmap_field<T: Expression>(
    method: &str,
    factory: &impl ExpressionFactory<T>,
) -> Option<T> {
    match method {
        "entries" => Some(factory.create_builtin_term(BuiltinTerm::Entries)),
        "get" => Some(factory.create_builtin_term(BuiltinTerm::Get)),
        "has" => Some(factory.create_builtin_term(BuiltinTerm::Contains)),
        "keys" => Some(factory.create_builtin_term(BuiltinTerm::Keys)),
        "set" => Some(factory.create_builtin_term(BuiltinTerm::Insert)),
        "values" => Some(factory.create_builtin_term(BuiltinTerm::Values)),
        _ => None,
    }
}

fn get_builtin_hashset_field<T: Expression>(
    method: &str,
    factory: &impl ExpressionFactory<T>,
) -> Option<T> {
    match method {
        "add" => Some(factory.create_builtin_term(BuiltinTerm::Push)),
        "entries" => Some(factory.create_builtin_term(BuiltinTerm::Entries)),
        "has" => Some(factory.create_builtin_term(BuiltinTerm::Contains)),
        "values" => Some(factory.create_builtin_term(BuiltinTerm::Values)),
        _ => None,
    }
}

pub fn format_value<TString: StringValue>(value: &ValueTerm<TString>) -> String {
    match value {
        ValueTerm::Null => String::from("null"),
        ValueTerm::Boolean(value) => format!("{}", value),
        ValueTerm::Int(value) => format!("{}", value),
        ValueTerm::Float(value) => format!("{}", value),
        ValueTerm::String(value) => String::from(value.as_str()),
        ValueTerm::Symbol(_) => format!("{}", value),
        ValueTerm::Hash(value) => format!("{:016x}", value),
    }
}

fn match_string_value_term<'a, T: Expression>(
    target: &'a T,
    factory: &impl ExpressionFactory<T>,
) -> Option<&'a T::String> {
    factory
        .match_value_term(target)
        .and_then(|target| match target {
            ValueTerm::String(value) => Some(value),
            _ => None,
        })
}
