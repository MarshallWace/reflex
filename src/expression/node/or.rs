// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::rc::Rc;

use crate::{
    env::Env,
    expression::{
        node::{CompoundNode, NodeFactoryResult},
        operations::evaluate::Evaluate2,
        Evaluate, Expression, Node, Value,
    },
    utils::format_type,
};

#[derive(Debug, PartialEq, Clone)]
pub struct OrNode {
    left: Rc<Expression>,
    right: Rc<Expression>,
}
impl OrNode {
    pub fn new(left: Rc<Expression>, right: Rc<Expression>) -> OrNode {
        OrNode { left, right }
    }
    pub const fn name() -> &'static str {
        "or"
    }
}
impl CompoundNode for OrNode {
    fn factory(args: Vec<Rc<Expression>>) -> NodeFactoryResult {
        if args.len() != 2 {
            return NodeFactoryResult::Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.into_iter();
        let left = args.next().unwrap();
        let right = args.next().unwrap();
        NodeFactoryResult::Ok(Node::Or(OrNode::new(left, right)))
    }
    fn expressions(&self) -> Vec<&Rc<Expression>> {
        vec![&self.left, &self.right]
    }
}
impl Evaluate for OrNode {
    fn evaluate(&self, env: &Env) -> Rc<Expression> {
        Evaluate2::evaluate(self, env)
    }
}
impl Evaluate2 for OrNode {
    fn dependencies(&self) -> (&Rc<Expression>, &Rc<Expression>) {
        (&self.left, &self.right)
    }
    fn run(&self, _env: &Env, left: &Rc<Expression>, right: &Rc<Expression>) -> Rc<Expression> {
        match (&**left, &**right) {
            (Expression::Value(Value::Boolean(left)), Expression::Value(Value::Boolean(right))) => {
                Rc::new(Expression::Value(Value::Boolean(*left && *right)))
            }
            (left, right) => Rc::new(Expression::Error(format!(
                "Expected (Boolean, Boolean), received ({}, {})",
                format_type(left),
                format_type(right),
            ))),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{env::Env, expression::{Evaluate, Expression, Node, Value}, parser};

    #[test]
    fn or_expressions() {
        let env = Env::new();
        let expression = parser::parse("(or false false)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Boolean(false)));
        let expression = parser::parse("(or false true)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Boolean(false)));
        let expression = parser::parse("(or true false)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Boolean(false)));
        let expression = parser::parse("(or true true)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Boolean(true)));
    }

    #[test]
    fn invalid_or_expression_arguments() {
        let env = Env::new();
        let expression = parser::parse("(or true null)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Error(String::from("Expected (Boolean, Boolean), received (Boolean(true), Nil)")));
        let expression = parser::parse("(or true 0)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Error(String::from("Expected (Boolean, Boolean), received (Boolean(true), Int(0))")));
        let expression = parser::parse("(or true 0.0)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Error(String::from("Expected (Boolean, Boolean), received (Boolean(true), Float(0.0))")));
        let expression = parser::parse("(or true \"\")", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Error(String::from("Expected (Boolean, Boolean), received (Boolean(true), String(\"\"))")));

        let expression = parser::parse("(or null true)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Error(String::from("Expected (Boolean, Boolean), received (Nil, Boolean(true))")));
        let expression = parser::parse("(or 0 true)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Error(String::from("Expected (Boolean, Boolean), received (Int(0), Boolean(true))")));
        let expression = parser::parse("(or 0.0 true)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Error(String::from("Expected (Boolean, Boolean), received (Float(0.0), Boolean(true))")));
        let expression = parser::parse("(or \"\" true)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Error(String::from("Expected (Boolean, Boolean), received (String(\"\"), Boolean(true))")));
    }
}
