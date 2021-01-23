// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{fmt, rc::Rc};

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Nil,
    Boolean(bool),
    Int(i32),
    Float(f64),
    String(StringValue),
}
impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let formatted = match self {
            Value::Nil => String::from("Nil"),
            Value::Boolean(value) => format!("{:?}", value),
            Value::Int(value) => format!("{:?}", value),
            Value::Float(value) => format!("{:?}", value),
            Value::String(value) => format!("{:?}", value),
        };
        write!(f, "{}", formatted)
    }
}

#[derive(Clone)]
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
impl fmt::Display for StringValue {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "{:?}", self.get())
    }
}
impl fmt::Debug for StringValue {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, formatter)
    }
}

#[cfg(test)]
mod tests {
    use super::{StringValue, Value};

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

    #[test]
    fn format_values() {
        assert_eq!(format!("{}", Value::Nil), "Nil");
        assert_eq!(format!("{}", Value::Boolean(true)), "true");
        assert_eq!(format!("{}", Value::Boolean(false)), "false");
        assert_eq!(format!("{}", Value::Int(0)), "0");
        assert_eq!(format!("{}", Value::Int(3)), "3");
        assert_eq!(format!("{}", Value::Int(-3)), "-3");
        assert_eq!(format!("{}", Value::Float(0.0)), "0.0");
        assert_eq!(format!("{}", Value::Float(3.0)), "3.0");
        assert_eq!(format!("{}", Value::Float(-3.0)), "-3.0");
        assert_eq!(
            format!("{}", Value::String(StringValue::literal("foo"))),
            "\"foo\""
        );
        assert_eq!(
            format!("{}", Value::String(StringValue::new(String::from("foo")))),
            "\"foo\""
        );
    }
}
