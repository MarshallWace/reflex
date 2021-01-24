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
pub struct NotNode {
    target: Rc<Expression>,
}
impl NotNode {
    pub fn new(target: Rc<Expression>) -> NotNode {
        NotNode { target }
    }
    pub const fn name() -> &'static str {
        "not"
    }
}
impl CompoundNode for NotNode {
    fn factory(args: Vec<Rc<Expression>>) -> NodeFactoryResult {
        if args.len() != 1 {
            return NodeFactoryResult::Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.into_iter();
        let target = args.next().unwrap();
        NodeFactoryResult::Ok(Node::Not(NotNode::new(target)))
    }
    fn expressions(&self) -> Vec<&Rc<Expression>> {
        vec![&self.target]
    }
}
impl Evaluate for NotNode {
    fn evaluate(&self, env: &Env) -> Rc<Expression> {
        Evaluate1::evaluate(self, env)
    }
}
impl Evaluate1 for NotNode {
    fn dependencies(&self) -> &Rc<Expression> {
        &self.target
    }
    fn run(&self, _env: &Env, target: &Rc<Expression>) -> Rc<Expression> {
        match &**target {
            Expression::Value(Value::Boolean(target)) => {
                Rc::new(Expression::Value(Value::Boolean(!*target)))
            }
            target => Rc::new(Expression::Error(format!(
                "Expected Boolean, received {}",
                format_type(target),
            ))),
        }
    }
}


#[cfg(test)]
mod tests {
    use crate::{env::Env, expression::{Evaluate, Expression, Node, Value}, parser};

    #[test]
    fn not_expressions() {
        let env = Env::new();
        let expression = parser::parse("(not true)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Boolean(false)));
        let expression = parser::parse("(not false)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Boolean(true)));
    }

    #[test]
    fn invalid_or_expression_arguments() {
        let env = Env::new();
        let expression = parser::parse("(not null)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Error(String::from("Expected Boolean, received Nil")));
        let expression = parser::parse("(not 0)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Error(String::from("Expected Boolean, received Int(0)")));
        let expression = parser::parse("(not 0.0)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Error(String::from("Expected Boolean, received Float(0.0)")));
        let expression = parser::parse("(not \"\")", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Error(String::from("Expected Boolean, received String(\"\")")));
    }
}
