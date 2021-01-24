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
pub struct PowNode {
    left: Rc<Expression>,
    right: Rc<Expression>,
}
impl PowNode {
    pub fn new(left: Rc<Expression>, right: Rc<Expression>) -> PowNode {
        PowNode { left, right }
    }
    pub const fn name() -> &'static str {
        "pow"
    }
}
impl CompoundNode for PowNode {
    fn factory(args: Vec<Rc<Expression>>) -> NodeFactoryResult {
        if args.len() != 2 {
            return NodeFactoryResult::Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.into_iter();
        let left = args.next().unwrap();
        let right = args.next().unwrap();
        NodeFactoryResult::Ok(Node::Pow(PowNode::new(left, right)))
    }
    fn expressions(&self) -> Vec<&Rc<Expression>> {
        vec![&self.left, &self.right]
    }
}
impl Evaluate for PowNode {
    fn evaluate(&self, env: &Env) -> Rc<Expression> {
        Evaluate2::evaluate(self, env)
    }
}
impl Evaluate2 for PowNode {
    fn dependencies(&self) -> (&Rc<Expression>, &Rc<Expression>) {
        (&self.left, &self.right)
    }
    fn run(&self, _env: &Env, left: &Rc<Expression>, right: &Rc<Expression>) -> Rc<Expression> {
        match (&**left, &**right) {
            (Expression::Value(Value::Int(left)), Expression::Value(Value::Int(right))) => {
                Rc::new(Expression::Value(Value::Int(left.pow(right))))
            }
            (Expression::Value(Value::Float(left)), Expression::Value(Value::Float(right))) => {
                Rc::new(Expression::Value(Value::Float(left.pow(right))))
            }
            (left, right) => Rc::new(Expression::Error(format!(
                "Expected (Int, Int) or (Float, Float), received ({}, {})",
                format_type(left),
                format_type(right),
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
    fn exponentiation_expressions() {
        let env = Env::new();
        let expression = parser::parse("(pow 0 0)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Int(0.pow(0))));
        let expression = parser::parse("(pow 3 4)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Int(3.pow(4))));
        let expression = parser::parse("(pow -3 4)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Int(-3.pow(4))));
        let expression = parser::parse("(pow 3 -4)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Int(3.pow(-4))));
        let expression = parser::parse("(pow -3 -4)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Int(-3.pow(-4))));

        let expression = parser::parse("(pow 0.0 0.0)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Float(0.0.pow(0.0))));
        let expression = parser::parse("(pow 2.718 3.142)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Float(2.718.pow(3.142))));
        let expression = parser::parse("(pow -2.718 3.142)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Float(-2.718.pow(3.142))));
        let expression = parser::parse("(pow 2.718 -3.142)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Float(2.718.pow(-3.142))));
        let expression = parser::parse("(pow -2.718 -3.142)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Float(-2.718.pow(-3.142))));
    }

    #[test]
    fn invalid_exponentiation_expression_operands() {
        let env = Env::new();
        let expression = parser::parse("(pow 3 3.142)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result,
            Expression::Error(String::from(
                "Expected (Int, Int) or (Float, Float), received (Int(3), Float(3.142))"
            ))
        );
        let expression = parser::parse("(pow 3.142 3)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result,
            Expression::Error(String::from(
                "Expected (Int, Int) or (Float, Float), received (Float(3.142), Int(3))"
            ))
        );

        let expression = parser::parse("(pow 3 null)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result,
            Expression::Error(String::from(
                "Expected (Int, Int) or (Float, Float), received (Int(3), Nil)"
            ))
        );
        let expression = parser::parse("(pow 3 false)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result,
            Expression::Error(String::from(
                "Expected (Int, Int) or (Float, Float), received (Int(3), Boolean(false))"
            ))
        );
        let expression = parser::parse("(pow 3 \"3\")", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result,
            Expression::Error(String::from(
                "Expected (Int, Int) or (Float, Float), received (Int(3), String(\"3\"))"
            ))
        );

        let expression = parser::parse("(pow null 3)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result,
            Expression::Error(String::from(
                "Expected (Int, Int) or (Float, Float), received (Nil, Int(3))"
            ))
        );
        let expression = parser::parse("(pow false 3)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result,
            Expression::Error(String::from(
                "Expected (Int, Int) or (Float, Float), received (Boolean(false), Int(3))"
            ))
        );
        let expression = parser::parse("(pow \"3\" 3)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result,
            Expression::Error(String::from(
                "Expected (Int, Int) or (Float, Float), received (String(\"3\"), Int(3))"
            ))
        );

        let expression = parser::parse("(pow 3.142 null)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result,
            Expression::Error(String::from(
                "Expected (Int, Int) or (Float, Float), received (Float(3.142), Nil)"
            ))
        );
        let expression = parser::parse("(pow 3.142 false)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result,
            Expression::Error(String::from(
                "Expected (Int, Int) or (Float, Float), received (Float(3.142), Boolean(false))"
            ))
        );
        let expression = parser::parse("(pow 3.142 \"3\")", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result,
            Expression::Error(String::from(
                "Expected (Int, Int) or (Float, Float), received (Float(3.142), String(\"3\"))"
            ))
        );

        let expression = parser::parse("(pow null 3.142)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result,
            Expression::Error(String::from(
                "Expected (Int, Int) or (Float, Float), received (Nil, Float(3.142))"
            ))
        );
        let expression = parser::parse("(pow false 3.142)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result,
            Expression::Error(String::from(
                "Expected (Int, Int) or (Float, Float), received (Boolean(false), Float(3.142))"
            ))
        );
        let expression = parser::parse("(pow \"3\" 3.142)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result,
            Expression::Error(String::from(
                "Expected (Int, Int) or (Float, Float), received (String(\"3\"), Float(3.142))"
            ))
        );
    }
}
