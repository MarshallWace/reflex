// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{fmt, rc::Rc};

use crate::utils::format_value;

#[derive(Debug, PartialEq)]
pub enum Value {
    Nil,
    Boolean(bool),
    Int(i32),
    Float(f64),
    String(StringValue),
}
impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Value::Nil => Value::Nil,
            Value::Boolean(value) => Value::Boolean(*value),
            Value::Int(value) => Value::Int(*value),
            Value::Float(value) => Value::Float(*value),
            Value::String(value) => Value::String(value.clone()),
        }
    }
}
impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", format_value(self))
    }
}

pub enum StringValue {
    Literal(&'static str),
    Runtime(Rc<String>),
}
impl StringValue {
    pub fn literal(value: &'static str) -> StringValue {
        StringValue::Literal(value)
    }
    pub fn new(value: String) -> StringValue {
        StringValue::Runtime(Rc::new(value))
    }
    pub fn get(&self) -> &str {
        match self {
            StringValue::Literal(value) => value,
            StringValue::Runtime(value) => value,
        }
    }
}
impl PartialEq for StringValue {
    fn eq(&self, other: &StringValue) -> bool {
        match (self, other) {
            (&StringValue::Literal(left), &StringValue::Literal(right)) => {
                std::ptr::eq(left, right)
            }
            (left, right) => left.get() == right.get(),
        }
    }
}
impl Clone for StringValue {
    fn clone(&self) -> Self {
        match self {
            StringValue::Literal(value) => StringValue::Literal(value),
            StringValue::Runtime(value) => StringValue::Runtime(Rc::clone(value)),
        }
    }
}
impl fmt::Debug for StringValue {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StringValue::Literal(value) => write!(formatter, "{:?}", value),
            StringValue::Runtime(value) => write!(formatter, "{:?}", value),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::StringValue;

    #[test]
    fn string_equality() {
        let left = StringValue::literal("foo");
        let right = StringValue::literal("foo");
        assert_eq!(left, right);
        let left = StringValue::literal("foo");
        let right = StringValue::literal("bar");
        assert_ne!(left, right);
        let left = StringValue::new(String::from("foo"));
        let right = StringValue::new(String::from("foo"));
        assert_eq!(left, right);
        let left = StringValue::new(String::from("foo"));
        let right = StringValue::new(String::from("bar"));
        assert_ne!(left, right);
        let left = StringValue::literal("foo");
        let right = StringValue::new(String::from("foo"));
        assert_eq!(left, right);
        let left = StringValue::literal("foo");
        let right = StringValue::new(String::from("bar"));
        assert_ne!(left, right);
        let left = StringValue::new(String::from("foo"));
        let right = StringValue::literal("foo");
        assert_eq!(left, right);
        let left = StringValue::new(String::from("foo"));
        let right = StringValue::literal("bar");
        assert_ne!(left, right);
    }
}
