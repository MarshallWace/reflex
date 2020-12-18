// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{rc::Rc, time::Instant};

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
    String(Rc<String>),
}
impl Value {
    fn string(value: String) -> Value {
        Value::String(Rc::from(value))
    }
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

enum Node {
    Add(AddNode),
}

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
            Expression::Error(message) => Result::Error(message.clone()),
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
            println!("Result: {:?}", value);
        }
    }
}
