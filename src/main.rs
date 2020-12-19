// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{fmt, rc::Rc, time::Instant};

#[derive(Debug)]
enum Expression {
    Pending,
    Error(String),
    Value(Value),
    Dynamic(Node),
}

#[derive(Debug)]
enum Result {
    Pending,
    Error(String),
    Value(Value),
}

#[derive(Debug)]
enum Value {
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

enum StringValue {
    Literal(&'static str),
    Runtime(Rc<String>),
}
impl StringValue {
    fn literal(value: &'static str) -> StringValue {
        StringValue::Literal(value)
    }
    fn new(value: String) -> StringValue {
        StringValue::Runtime(Rc::new(value))
    }
    fn get(&self) -> &str {
        match self {
            StringValue::Literal(value) => value,
            StringValue::Runtime(value) => value,
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
            StringValue::Literal(value) => formatter.write_str(&format!("{:?}", value)),
            StringValue::Runtime(value) => formatter.write_str(&format!("{:?}", value)),
        }
    }
}

#[derive(Debug)]
enum Node {
    Add(AddNode),
}

#[derive(Debug)]
struct AddNode {
    left: Box<Expression>,
    right: Box<Expression>,
}
impl AddNode {
    fn create(left: Expression, right: Expression) -> AddNode {
        AddNode {
            left: Box::new(left),
            right: Box::new(right),
        }
    }
    fn execute(&self, store: &Store) -> Result {
        let left = store.evaluate(&*self.left);
        let right = store.evaluate(&*self.right);
        match (&left, &right) {
            (Result::Error(_), _) => left,
            (_, Result::Error(_)) => right,
            (Result::Pending, _) | (_, Result::Pending) => Result::Pending,
            (Result::Value(Value::Int(left)), Result::Value(Value::Int(right))) => {
                Result::Value(Value::Int(left + right))
            }
            (Result::Value(Value::Float(left)), Result::Value(Value::Float(right))) => {
                Result::Value(Value::Float(left + right))
            }
            (Result::Value(left), Result::Value(right)) => Result::Error(format!(
                "Expected (Int, Int) or (Float, Float), received ({:?}, {:?})",
                left, right
            )),
        }
    }
}

struct Store {}
impl Store {
    fn create() -> Store {
        Store {}
    }
    fn evaluate(&self, value: &Expression) -> Result {
        match value {
            Expression::Pending => Result::Pending,
            Expression::Error(message) => Result::Error(message.to_owned()),
            Expression::Value(value) => Result::Value(value.clone()),
            Expression::Dynamic(node) => match node {
                Node::Add(node) => node.execute(self),
            },
        }
    }
}

fn main() {
    let store = Store::create();
    let target = Expression::Dynamic(Node::Add(AddNode::create(
        Expression::Dynamic(Node::Add(AddNode::create(
            Expression::Value(Value::Int(3)),
            Expression::Value(Value::Int(4)),
        ))),
        Expression::Value(Value::Int(5)),
    )));
    let start = Instant::now();
    let result = store.evaluate(&target);
    let duration = start.elapsed();
    print_result(&result);
    println!("Time: {:?}", duration)
}

fn print_result(result: &Result) {
    match result {
        Result::Pending => {
            println!("Pending...");
        }
        Result::Error(message) => {
            println!("Error: {}", message);
        }
        Result::Value(value) => {
            println!("Result: {}", format_value(value));
        }
    }
}

fn format_value(value: &Value) -> String {
    match value {
        Value::Nil => String::from("Nil"),
        Value::Boolean(value) => format!("{:?}", value),
        Value::Int(value) => format!("{:?}", value),
        Value::Float(value) => format!("{:?}", value),
        Value::String(value) => format!("{:?}", value.get()),
    }
}
