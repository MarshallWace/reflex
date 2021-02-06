// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{iter, rc::Rc};

use crate::{
    env::Env,
    expression::{
        node::{CompoundNode, NodeFactoryResult},
        operations::evaluate::Evaluate1,
        Bind, Closure, Evaluate, Expression, Function, Node,
    },
};

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
}
impl CompoundNode for ApplyNode {
    fn factory(args: Vec<Rc<Expression>>) -> NodeFactoryResult {
        if args.len() < 1 {
            return NodeFactoryResult::Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.into_iter();
        let target = args.next().unwrap();
        let args = args.collect();
        NodeFactoryResult::Ok(Node::Apply(ApplyNode::new(target, args)))
    }
    fn expressions(&self) -> Vec<&Rc<Expression>> {
        iter::once(&self.target).chain(self.args.iter()).collect()
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
            Expression::Closure(Closure {
                env: captured_env,
                function:
                    Function {
                        arity,
                        captures: _,
                        body,
                    },
            }) => apply_function(*arity, body, &self.args, env, Some(captured_env)),
            Expression::Function(Function {
                arity,
                captures: _,
                body,
            }) => apply_function(*arity, body, &self.args, env, None),
            _ => Rc::new(Expression::Error(String::from(
                "Target expression is not a function",
            ))),
        }
    }
}

fn apply_function(
    arity: usize,
    body: &Rc<Expression>,
    args: &Vec<Rc<Expression>>,
    env: &Env,
    captured_env: Option<&Env>,
) -> Rc<Expression> {
    if args.len() != arity {
        return Rc::new(Expression::Error(format!(
            "Expected {} arguments, received {}",
            arity,
            args.len()
        )));
    }
    if arity == 0 {
        return body.evaluate(match captured_env {
            Some(captured_env) => captured_env,
            None => env,
        });
    }
    let inner_env = match captured_env {
        Some(captured_env) => captured_env.extend(args.iter().map(|arg| arg.bind(env))),
        None => env.extend(args.iter().cloned()),
    };
    body.evaluate(&inner_env)
}

#[cfg(test)]
mod tests {
    use crate::{
        env::Env,
        expression::{Evaluate, Expression, Node, Value},
        parser,
    };

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
        let expression = parser::parse("(apply (fn (foo) (add foo 4)) 3)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Int(3 + 4)));
    }

    #[test]
    fn binary_functions() {
        let env = Env::new();
        let expression =
            parser::parse("(apply (fn (foo bar) (add foo bar)) 3 4)", &Node::factory).unwrap();
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

    #[test]
    fn immediately_invoked_closures() {
        let env = Env::new();
        let expression = parser::parse(
            "(apply (apply (fn (one two) (fn (three four) (add one three))) 1 2) 3 4)",
            &Node::factory,
        )
        .unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Int(1 + 3)));
    }

    #[test]
    fn deferred_closures() {
        let env = Env::new();
        let expression = parser::parse(
            "(apply (fn (transform value) (apply transform (apply value))) (apply (fn (constant) (fn (value) (add value constant))) 4) (apply (fn (value) (fn () value)) 3))",
            &Node::factory,
        )
        .unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Int(3 + 4)));
    }
}
