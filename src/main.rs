// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::time::Instant;

enum Expression {
    Result(Result),
    Node(Node),
}

#[derive(Debug, Clone)]
enum Result {
    Pending,
    Error(String),
    Value(Value),
}

#[derive(Debug, Clone)]
enum Value {
    Nil,
    Boolean(bool),
    Int(i32),
    Float(f64),
    String(String),
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
        if let (Result::Value(left), Result::Value(right)) = (&left, &right) {
            if let (Value::Int(left), Value::Int(right)) = (&left, &right) {
                return Result::Value(Value::Int(left + right));
            } else if let (Value::Float(left), Value::Float(right)) = (&left, &right) {
                return Result::Value(Value::Float(left + right));
            } else {
                return Result::Error(String::from(
                    "Expected (Int, Int) or (Float, Float), received (?, ?)",
                ));
            }
        } else if let Result::Error(_) = left {
            return left;
        } else if let Result::Error(_) = right {
            return right;
        } else if let Result::Pending = left {
            return left;
        } else if let Result::Pending = right {
            return right;
        }
        return Result::Error(String::from("Invalid result"));
    }
}

struct Store {}
impl Store {
    fn create() -> Store {
        Store {}
    }
    fn evaluate(&self, value: &Expression) -> Result {
        match value {
            Expression::Result(result) => result.clone(),
            Expression::Node(node) => match node {
                Node::Add(node) => node.execute(self),
            },
        }
    }
}

fn main() {
    let store = Store::create();
    let target = Expression::Node(Node::Add(AddNode::create(
        Expression::Node(Node::Add(AddNode::create(
            Expression::Result(Result::Value(Value::Int(3))),
            Expression::Result(Result::Value(Value::Int(4))),
        ))),
        Expression::Result(Result::Value(Value::Int(5))),
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
