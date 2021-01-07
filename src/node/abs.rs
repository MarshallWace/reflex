// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    expression::Expression,
    node::{Node, NodeFactoryResult},
    operation::evaluate::{Evaluate, Evaluate1},
    env::Env,
    value::Value,
    utils::format_type,
};

#[derive(Debug, PartialEq)]
pub struct AbsNode {
    target: Box<Expression>,
}
impl AbsNode {
    pub fn new(target: Expression) -> AbsNode {
        AbsNode {
            target: Box::new(target),
        }
    }
    pub const fn name() -> &'static str {
        "abs"
    }
    pub fn factory(args: Vec<Expression>) -> NodeFactoryResult {
        if args.len() != 1 {
            return NodeFactoryResult::Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.into_iter();
        let target = args.next().unwrap();
        NodeFactoryResult::Some(Node::Abs(AbsNode::new(target)))
    }
}
impl Evaluate for AbsNode {
    fn evaluate(&self, env: &Env) -> Expression {
        Evaluate1::evaluate(self, env)
    }
}
impl Evaluate1 for AbsNode {
    fn dependencies(&self) -> &Expression {
        &self.target
    }
    fn run(&self, _env: &Env, target: &Expression) -> Expression {
        match target {
            Expression::Value(Value::Int(target)) => Expression::Value(Value::Int(target.abs())),
            Expression::Value(Value::Float(target)) => {
                Expression::Value(Value::Float(target.abs()))
            }
            target => Expression::Error(format!(
                "Expected Int or Float, received {}",
                format_type(target),
            )),
        }
    }
}
