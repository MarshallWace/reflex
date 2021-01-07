// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    expression::Expression,
    node::{Node,NodeFactoryResult},
    operation::evaluate::{Evaluate, Evaluate2},
    env::Env,
    value::Value,
    utils::format_type,
};

#[derive(Debug, PartialEq)]
pub struct AddNode {
    left: Box<Expression>,
    right: Box<Expression>,
}
impl AddNode {
    pub fn new(left: Expression, right: Expression) -> AddNode {
        AddNode {
            left: Box::new(left),
            right: Box::new(right),
        }
    }
    pub const fn name() -> &'static str {
        "add"
    }
    pub fn factory(args: Vec<Expression>) -> NodeFactoryResult {
        if args.len() != 2 {
            return NodeFactoryResult::Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.into_iter();
        let left = args.next().unwrap();
        let right = args.next().unwrap();
        NodeFactoryResult::Some(Node::Add(AddNode::new(left, right)))
    }
}
impl Evaluate for AddNode {
    fn evaluate(&self, env: &Env) -> Expression {
        Evaluate2::evaluate(self, env)
    }
}
impl Evaluate2 for AddNode {
    fn dependencies(&self) -> (&Expression, &Expression) {
        (&self.left, &self.right)
    }
    fn run(&self, _env: &Env, left: &Expression, right: &Expression) -> Expression {
        match (left, right) {
            (Expression::Value(Value::Int(left)), Expression::Value(Value::Int(right))) => {
                Expression::Value(Value::Int(left + right))
            }
            (Expression::Value(Value::Float(left)), Expression::Value(Value::Float(right))) => {
                Expression::Value(Value::Float(left + right))
            }
            (left, right) => Expression::Error(format!(
                "Expected (Int, Int) or (Float, Float), received ({}, {})",
                format_type(left),
                format_type(right),
            )),
        }
    }
}
