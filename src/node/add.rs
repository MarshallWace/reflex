// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::rc::Rc;

use crate::{
    expression::Expression,
    node::{Node,NodeFactoryResult},
    operation::evaluate::{Evaluate, Evaluate2},
    env::Env,
    value::Value,
    utils::format_type,
};

#[derive(Debug, PartialEq, Clone)]
pub struct AddNode {
    left: Rc<Expression>,
    right: Rc<Expression>,
}
impl AddNode {
    pub fn new(left: Rc<Expression>, right: Rc<Expression>) -> AddNode {
        AddNode {
            left,
            right,
        }
    }
    pub const fn name() -> &'static str {
        "add"
    }
    pub fn factory(args: Vec<Rc<Expression>>) -> NodeFactoryResult {
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
    fn evaluate(&self, env: &Env) -> Rc<Expression> {
        Evaluate2::evaluate(self, env)
    }
}
impl Evaluate2 for AddNode {
    fn dependencies(&self) -> (&Rc<Expression>, &Rc<Expression>) {
        (&self.left, &self.right)
    }
    fn run(&self, _env: &Env, left: &Rc<Expression>, right: &Rc<Expression>) -> Rc<Expression> {
        match (&**left, &**right) {
            (Expression::Value(Value::Int(left)), Expression::Value(Value::Int(right))) => {
                Rc::new(Expression::Value(Value::Int(left + right)))
            }
            (Expression::Value(Value::Float(left)), Expression::Value(Value::Float(right))) => {
                Rc::new(Expression::Value(Value::Float(left + right)))
            }
            (left, right) => Rc::new(Expression::Error(format!(
                "Expected (Int, Int) or (Float, Float), received ({}, {})",
                format_type(left),
                format_type(right),
            ))),
        }
    }
}
