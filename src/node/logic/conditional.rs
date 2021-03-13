// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{fmt, iter::once};

use crate::{
    env::Env,
    expression::{
        AstNode, CompoundNode, EvaluationResult, Expression, NodeFactoryResult, NodeType,
    },
    node::{
        core::{CoreNode, ErrorNode, ValueNode},
        Evaluate1, Node,
    },
};

#[derive(Debug, PartialEq, Clone)]
pub struct ConditionalNode {
    condition: Expression<Node>,
    consequent: Expression<Node>,
    alternate: Expression<Node>,
}
impl ConditionalNode {
    pub fn new(
        condition: Expression<Node>,
        consequent: Expression<Node>,
        alternate: Expression<Node>,
    ) -> Self {
        ConditionalNode {
            condition,
            consequent,
            alternate,
        }
    }
}
impl AstNode<Node> for ConditionalNode {
    fn factory(args: &[Expression<Node>]) -> NodeFactoryResult<Self> {
        if args.len() != 3 {
            return Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.iter().map(Expression::clone);
        let condition = args.next().unwrap();
        let consequent = args.next().unwrap();
        let alternate = args.next().unwrap();
        Ok(Self::new(condition, consequent, alternate))
    }
}
impl<'a> CompoundNode<'a> for ConditionalNode {
    type Expressions = std::iter::Chain<
        std::iter::Chain<
            std::iter::Once<&'a Expression<Node>>,
            std::iter::Once<&'a Expression<Node>>,
        >,
        std::iter::Once<&'a Expression<Node>>,
    >;
    fn expressions(&'a self) -> Self::Expressions {
        once(&self.condition)
            .chain(once(&self.consequent))
            .chain(once(&self.alternate))
    }
}
impl NodeType<Node> for ConditionalNode {
    fn hash(&self) -> u32 {
        CompoundNode::hash(self)
    }
    fn capture_depth(&self) -> usize {
        CompoundNode::capture_depth(self)
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<EvaluationResult<Node>> {
        Evaluate1::evaluate(self, env)
    }
}
impl Evaluate1 for ConditionalNode {
    fn dependencies(&self) -> &Expression<Node> {
        &self.condition
    }
    fn run(&self, condition: &Expression<Node>) -> Expression<Node> {
        match condition.value() {
            Node::Core(CoreNode::Value(ValueNode::Boolean(condition))) => {
                if *condition {
                    Expression::clone(&self.consequent)
                } else {
                    Expression::clone(&self.alternate)
                }
            }
            _ => Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(&format!(
                "Expected Boolean, received {}",
                condition
            ))))),
        }
    }
}
impl fmt::Display for ConditionalNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        env::Env,
        expression::Expression,
        node::{
            core::{CoreNode, ErrorNode, ValueNode},
            parser, Node,
        },
    };

    #[test]
    fn conditional_expressions() {
        let env = Env::new();
        let expression = parser::parse("(if #t 3 4)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3))))
        );
        let expression = parser::parse("(if #f 3 4)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4))))
        );
    }

    #[test]
    fn conditional_expression_short_circuiting() {
        let env = Env::new();
        let expression = parser::parse("(if #t (+ 3 4) (error \"foo\"))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3 + 4))))
        );
        let expression = parser::parse("(if #f (error \"foo\") (+ 3 4))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3 + 4))))
        );
    }

    #[test]
    fn invalid_conditional_expression_conditions() {
        let env = Env::new();
        let expression = parser::parse("(if null #t #f)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected Boolean, received null"
            ))))
        );
        let expression = parser::parse("(if 0 #t #f)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected Boolean, received 0"
            ))))
        );
        let expression = parser::parse("(if 0.0 #t #f)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected Boolean, received 0.0"
            ))))
        );
        let expression = parser::parse("(if \"\" #t #f)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected Boolean, received \"\""
            ))))
        );
    }
}
