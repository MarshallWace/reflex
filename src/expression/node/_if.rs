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
pub struct IfNode {
    condition: Rc<Expression>,
    consequent: Rc<Expression>,
    alternate: Rc<Expression>,
}
impl IfNode {
    pub fn new(
        condition: Rc<Expression>,
        consequent: Rc<Expression>,
        alternate: Rc<Expression>,
    ) -> IfNode {
        IfNode {
            condition,
            consequent,
            alternate,
        }
    }
    pub const fn name() -> &'static str {
        "if"
    }
}
impl CompoundNode for IfNode {
    fn factory(args: Vec<Rc<Expression>>) -> NodeFactoryResult {
        if args.len() != 3 {
            return NodeFactoryResult::Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.into_iter();
        let condition = args.next().unwrap();
        let consequent = args.next().unwrap();
        let alternate = args.next().unwrap();
        NodeFactoryResult::Ok(Node::If(IfNode::new(condition, consequent, alternate)))
    }
    fn expressions(&self) -> Vec<&Rc<Expression>> {
        vec![&self.condition, &self.consequent, &self.alternate]
    }
}
impl Evaluate for IfNode {
    fn evaluate(&self, env: &Env) -> Rc<Expression> {
        Evaluate1::evaluate(self, env)
    }
}
impl Evaluate1 for IfNode {
    fn dependencies(&self) -> &Rc<Expression> {
        &self.condition
    }
    fn run(&self, _env: &Env, condition: &Rc<Expression>) -> Rc<Expression> {
        match &**condition {
            Expression::Value(Value::Boolean(condition)) => {
                if *condition {
                    Rc::clone(&self.consequent)
                } else {
                    Rc::clone(&self.alternate)
                }
            }
            _ => Rc::new(Expression::Error(format!(
                "Expected Boolean, received {}",
                format_type(condition),
            ))),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{env::Env, expression::{Evaluate, Expression, Node, Value}, parser};


    #[test]
    fn conditional_expressions() {
        let env = Env::new();
        let expression = parser::parse("(if true 3 4)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Int(3)));
        let expression = parser::parse("(if false 3 4)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Int(4)));
    }

    #[test]
    fn conditional_expression_short_circuiting() {
        let env = Env::new();
        let expression = parser::parse("(if true (add 3 4) (throw \"foo\"))", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Int(3 + 4)));
        let expression = parser::parse("(if false (throw \"foo\") (add 3 4))", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Int(3 + 4)));
    }

    #[test]
    fn invalid_conditional_expression_conditions() {
        let env = Env::new();
        let expression = parser::parse("(if null true false)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Error(String::from("Expected Boolean, received Nil")));
        let expression = parser::parse("(if 0 true false)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Error(String::from("Expected Boolean, received Int(0)")));
        let expression = parser::parse("(if 0.0 true false)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Error(String::from("Expected Boolean, received Float(0.0)")));
        let expression = parser::parse("(if \"\" true false)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Error(String::from("Expected Boolean, received String(\"\")")));
    }
}
