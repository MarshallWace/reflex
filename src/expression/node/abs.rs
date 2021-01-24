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

#[cfg(test)]
mod tests {
    use crate::{
        env::Env,
        expression::{Evaluate, Expression, Node, Value},
        parser,
    };

    #[test]
    fn abs_expressions() {
        let env = Env::new();
        let expression = parser::parse("(abs 0)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Int(0)));
        let expression = parser::parse("(abs -0)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Int(0)));
        let expression = parser::parse("(abs 3)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Int(3)));
        let expression = parser::parse("(abs -3)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Int(3)));

        let expression = parser::parse("(abs 0.0)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Float(0.0)));
        let expression = parser::parse("(abs -0.0)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Float(0.0)));
        let expression = parser::parse("(abs 3.142)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Float(3.142)));
        let expression = parser::parse("(abs -3.142)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Float(3.142)));
    }

    #[test]
    fn invalid_abs_expression_operands() {
        let env = Env::new();
        let expression = parser::parse("(abs null)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result,
            Expression::Error(String::from("Expected Int or Float, received Nil"))
        );
        let expression = parser::parse("(abs false)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result,
            Expression::Error(String::from(
                "Expected Int or Float, received Boolean(false)"
            ))
        );
        let expression = parser::parse("(abs \"3\")", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result,
            Expression::Error(String::from(
                "Expected Int or Float, received String(\"3\")"
            ))
        );
    }
}
