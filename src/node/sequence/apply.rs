// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{fmt, iter::once};

use crate::{
    env::Env,
    expression::{
        AstNode, CompoundNode, EvaluationResult, EvaluationResultType, Expression,
        NodeFactoryResult, NodeType, RuntimeState,
    },
    node::{
        core::{CoreNode, FunctionApplicationNode},
        sequence::cons::collect_list_items,
        Node,
    },
};

#[derive(Debug, PartialEq, Clone)]
pub struct ApplyNode {
    target: Expression<Node>,
    args: Expression<Node>,
}
impl ApplyNode {
    pub fn new(target: Expression<Node>, args: Expression<Node>) -> Self {
        ApplyNode { target, args }
    }
}
impl AstNode<Node> for ApplyNode {
    fn factory(args: &[Expression<Node>]) -> NodeFactoryResult<Self> {
        if args.len() != 2 {
            return Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.iter().map(Expression::clone);
        let target = args.next().unwrap();
        let args = args.next().unwrap();
        Ok(Self::new(target, args))
    }
}
impl<'a> CompoundNode<'a, Node> for ApplyNode {
    type Expressions = std::iter::Chain<
        std::iter::Once<&'a Expression<Node>>,
        std::iter::Once<&'a Expression<Node>>,
    >;
    fn expressions(&'a self) -> Self::Expressions {
        once(&self.target).chain(once(&self.args))
    }
}
impl NodeType<Node> for ApplyNode {
    fn hash(&self) -> u32 {
        CompoundNode::hash(self)
    }
    fn capture_depth(&self) -> usize {
        CompoundNode::capture_depth(self)
    }
    fn evaluate(
        &self,
        env: &Env<Node>,
        state: &RuntimeState<Node>,
    ) -> Option<EvaluationResult<Node>> {
        let target = collect_list_items(&self.args, env, state, |args| {
            Expression::new(Node::Core(CoreNode::FunctionApplication(
                FunctionApplicationNode::new(Expression::clone(&self.target), args),
            )))
        });
        let (expression, dependencies) = target.unwrap();
        Some(match expression {
            EvaluationResultType::Signal(signal) => {
                EvaluationResult::signal_with_dependencies(signal, dependencies)
            }
            EvaluationResultType::Expression(expression) => expression
                .evaluate(env, state)
                .add_dependencies(dependencies),
        })
    }
}
impl fmt::Display for ApplyNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        env::Env,
        expression::{EvaluationResult, Expression, RuntimeState},
        node::{
            core::{CoreNode, ValueNode},
            parser, Node,
        },
        signal::Signal,
    };

    #[test]
    fn apply_expressions() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("(apply (lambda () 3) (list))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(3)
            )),))
        );
        let expression =
            parser::parse("(apply (lambda (foo bar) (+ foo bar)) (list 3 4))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(3 + 4)
            )),))
        );
        let expression = parser::parse(
            "(apply (lambda (first second third) (+ (abs first) (+ second third))) (list -3 4 5))",
        )
        .unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int((-3 as i32).abs() + (4 + 5))
            )),))
        );
    }

    #[test]
    fn dynamic_argument_lists() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse(
            "(apply (lambda (foo bar) (+ foo bar)) (cons 3 ((lambda () (cons 4 null)))))",
        )
        .unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(3 + 4)
            )),))
        );
        let expression = parser::parse(
            "(apply (lambda (foo bar) (+ foo bar)) (cons 3 ((lambda (foo) (cons 4 foo)) null)))",
        )
        .unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(3 + 4)
            )),))
        );
        let expression = parser::parse(
            "(apply (lambda (foo bar) (+ foo bar)) ((lambda (foo) foo) (cons ((lambda (first) first) 3) ((lambda (third) (cons ((lambda (second) second) 4) third)) null))))",
        )
        .unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(3 + 4)
            )),))
        );
        let expression = parser::parse(
    "(apply (lambda (first second third) (+ first (+ second third))) ((lambda (first) ((lambda (second) ((lambda (third) (cons first (cons second (cons third null)))) 5)) 4)) 3))"
        ).unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(3 + 4 + 5)
            )))),
        )
    }

    #[test]
    fn invalid_apply_expressions() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("(apply 3 (list))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from(
                "Target expression is not a function: 3"
            ))),
        );
        let expression = parser::parse("(apply (lambda (foo) foo) 3)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from("Expected list, received 3"))),
        );
        let expression = parser::parse("(apply (lambda (foo) foo) (list))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from(
                "Expected 1 arguments, received 0"
            ))),
        );
        let expression = parser::parse("(apply (lambda (foo) foo) (list 3 4))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from(
                "Expected 1 arguments, received 2"
            ))),
        );
    }
}
