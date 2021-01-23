// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::rc::Rc;

use crate::{
    env::Env,
    expression::{
        node::{CompoundNode, NodeFactoryResult},
        operations::evaluate::Evaluate1,
        Evaluate, Expression, Node, Value,
    },
    utils::format_type,
};

#[derive(Debug, PartialEq, Clone)]
pub struct AbsNode {
    target: Rc<Expression>,
}
impl AbsNode {
    pub fn new(target: Rc<Expression>) -> AbsNode {
        AbsNode { target }
    }
    pub const fn name() -> &'static str {
        "abs"
    }
}
impl CompoundNode for AbsNode {
    fn factory(args: Vec<Rc<Expression>>) -> NodeFactoryResult {
        if args.len() != 1 {
            return NodeFactoryResult::Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.into_iter();
        let target = args.next().unwrap();
        NodeFactoryResult::Ok(Node::Abs(AbsNode::new(target)))
    }
    fn expressions(&self) -> Vec<&Rc<Expression>> {
        vec![&self.target]
    }
}
impl Evaluate for AbsNode {
    fn evaluate(&self, env: &Env) -> Rc<Expression> {
        Evaluate1::evaluate(self, env)
    }
}
impl Evaluate1 for AbsNode {
    fn dependencies(&self) -> &Rc<Expression> {
        &self.target
    }
    fn run(&self, _env: &Env, target: &Rc<Expression>) -> Rc<Expression> {
        match &**target {
            Expression::Value(Value::Int(target)) => {
                Rc::new(Expression::Value(Value::Int(target.abs())))
            }
            Expression::Value(Value::Float(target)) => {
                Rc::new(Expression::Value(Value::Float(target.abs())))
            }
            target => Rc::new(Expression::Error(format!(
                "Expected Int or Float, received {}",
                format_type(target),
            ))),
        }
    }
}
