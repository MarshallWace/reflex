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
pub struct GetNode {
    target: Rc<Expression>,
    key: Rc<Expression>,
}
impl GetNode {
    pub fn new(target: Rc<Expression>, key: Rc<Expression>) -> GetNode {
        GetNode { target, key }
    }
    pub const fn name() -> &'static str {
        "get"
    }
}
impl CompoundNode for GetNode {
    fn factory(args: Vec<Rc<Expression>>) -> NodeFactoryResult {
        if args.len() != 2 {
            return NodeFactoryResult::Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.into_iter();
        let target = args.next().unwrap();
        let key = args.next().unwrap();
        NodeFactoryResult::Ok(Node::Get(GetNode::new(target, key)))
    }
    fn expressions(&self) -> Vec<&Rc<Expression>> {
        vec![&self.target, &self.key]
    }
}
impl Evaluate for GetNode {
    fn evaluate(&self, env: &Env) -> Rc<Expression> {
        Evaluate2::evaluate(self, env)
    }
}
impl Evaluate2 for GetNode {
    fn dependencies(&self) -> (&Rc<Expression>, &Rc<Expression>) {
        (&self.target, &self.key)
    }
    fn run(&self, _env: &Env, target: &Rc<Expression>, key: &Rc<Expression>) -> Rc<Expression> {
        match &**target {
            Expression::Object(target) => match &**key {
                Expression::Value(Value::String(key)) => match target.get(key) {
                    Some(value) => value,
                    None => Rc::new(Expression::Error(format!("Field not found: {}", key.get()))),
                },
                _ => Rc::new(Expression::Error(format!(
                    "Unable to get field: {}",
                    format_type(key)
                ))),
            },
            _ => Rc::new(Expression::Error(format!(
                "Unable to get field of non-object: {}",
                format_type(target)
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
    fn get_object_fields() {
        let env = Env::new();
        let expression = parser::parse(
            "(get { first: 1, second: 2, third: 3 } \"second\")",
            &Node::factory,
        )
        .unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Int(2)));
    }

    #[test]
    fn get_undefined_object_field() {
        let env = Env::new();
        let expression = parser::parse(
            "(get { first: 1, second: 2, third: 3 } \"fourth\")",
            &Node::factory,
        )
        .unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result,
            Expression::Error(String::from("Field not found: fourth"))
        );
    }

    #[test]
    fn get_invalid_object_field() {
        let env = Env::new();
        let expression =
            parser::parse("(get { first: 1, second: 2, third: 3 } 3)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result,
            Expression::Error(String::from("Unable to get field: Int(3)"))
        );
    }

    #[test]
    fn get_non_object_field() {
        let env = Env::new();
        let expression = parser::parse("(get 3 \"foo\")", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result,
            Expression::Error(String::from("Unable to get field of non-object: Int(3)"))
        );
    }
}
