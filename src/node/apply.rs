// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::rc::Rc;

use crate::{
    env::Env,
    expression::Expression,
    operation::evaluate::{Evaluate, Evaluate1},
};

use super::{Node, NodeFactoryResult};

#[derive(Debug, PartialEq, Clone)]
pub struct ApplyNode {
    target: Rc<Expression>,
    args: Vec<Rc<Expression>>,
}
impl ApplyNode {
    pub fn new(target: Rc<Expression>, args: Vec<Rc<Expression>>) -> ApplyNode {
        ApplyNode { target, args }
    }
    pub const fn name() -> &'static str {
        "apply"
    }
    pub fn factory(args: Vec<Rc<Expression>>) -> NodeFactoryResult {
        if args.len() < 1 {
            return NodeFactoryResult::Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.into_iter();
        let target = args.next().unwrap();
        let args = args.collect();
        NodeFactoryResult::Ok(Node::Apply(ApplyNode::new(target, args)))
    }
}
impl Evaluate for ApplyNode {
    fn evaluate(&self, env: &Env) -> Rc<Expression> {
        Evaluate1::evaluate(self, env)
    }
}
impl Evaluate1 for ApplyNode {
    fn dependencies(&self) -> &Rc<Expression> {
        &self.target
    }
    fn run(&self, env: &Env, target: &Rc<Expression>) -> Rc<Expression> {
        match &**target {
            Expression::Function(arg_names, body) => {
                let args = &self.args;
                if args.len() != arg_names.len() {
                    return Rc::new(Expression::Error(format!(
                        "Expected {} arguments, received {}",
                        arg_names.len(),
                        args.len()
                    )));
                }
                if args.len() == 0 {
                    return body.evaluate(env);
                }
                let bindings = arg_names
                    .iter()
                    .map(|arg_name| String::from(arg_name))
                    .zip(args.iter().map(|value| Rc::clone(value)));
                let child_env = env.set(bindings);
                body.evaluate(&child_env)
            }
            _ => Rc::new(Expression::Error(String::from("Target expression is not a function"))),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{env::Env, expression::Expression, node::Node, operation::evaluate::Evaluate, parser, value::Value};

    #[test]
    fn nullary_functions() {
        let env = Env::new();
        let expression = parser::parse("(apply (fn () (add 3 4)))", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Int(3 + 4)));
    }

    #[test]
    fn unary_functions() {
        let env = Env::new();
        let expression =
            parser::parse("(apply (fn (foo) (add foo 4)) 3)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Int(3 + 4)));
    }

    #[test]
    fn binary_functions() {
        let env = Env::new();
        let expression = parser::parse(
            "(apply (fn (foo bar) (add foo bar)) 3 4)",
            &Node::factory,
        )
        .unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Int(3 + 4)));
    }

    #[test]
    fn ternary_functions() {
        let env = Env::new();
        let expression = parser::parse(
            "(apply (fn (foo bar baz) (add foo (add bar baz))) 3 4 5)",
            &Node::factory,
        )
        .unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Int(3 + 4 + 5)));
    }
}
