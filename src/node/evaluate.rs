// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    env::Env,
    expression::{combine_dependency_lists, with_dependencies, EvaluationResult, Expression},
    node::{core::CoreNode, Node},
};

pub trait Evaluate0 {
    fn run(&self) -> Expression<Node>;
    fn evaluate(&self, env: &Env<Node>) -> Option<EvaluationResult<Node>> {
        Some(self.run().evaluate(env))
    }
}

pub trait Evaluate1 {
    fn dependencies(&self) -> &Expression<Node>;
    fn run(&self, dep1: &Expression<Node>) -> Expression<Node>;
    fn evaluate(&self, env: &Env<Node>) -> Option<EvaluationResult<Node>> {
        let dep1 = self.dependencies();
        let dep1 = dep1.evaluate(env);
        Some(match dep1.expression.value() {
            Node::Core(CoreNode::Error(_)) | Node::Core(CoreNode::Pending(_)) => dep1,
            _ => {
                let EvaluationResult {
                    expression: expression1,
                    dependencies: dependencies1,
                } = dep1;
                with_dependencies(dependencies1, self.run(&expression1).evaluate(env))
            }
        })
    }
}

pub trait Evaluate2 {
    fn dependencies(&self) -> (&Expression<Node>, &Expression<Node>);
    fn run(&self, dep1: &Expression<Node>, dep2: &Expression<Node>) -> Expression<Node>;
    fn evaluate(&self, env: &Env<Node>) -> Option<EvaluationResult<Node>> {
        let (dep1, dep2) = self.dependencies();
        let dep1 = dep1.evaluate(env);
        let dep2 = dep2.evaluate(env);
        Some(match (dep1.expression.value(), dep2.expression.value()) {
            (Node::Core(CoreNode::Error(_)), _) => dep1,
            (_, Node::Core(CoreNode::Error(_))) => dep2,
            (Node::Core(CoreNode::Pending(_)), _) => dep1,
            (_, Node::Core(CoreNode::Pending(_))) => dep2,
            (_, _) => {
                let EvaluationResult {
                    expression: expression1,
                    dependencies: dependencies1,
                } = dep1;
                let EvaluationResult {
                    expression: expression2,
                    dependencies: dependencies2,
                } = dep2;
                with_dependencies(
                    combine_dependency_lists(dependencies1, dependencies2),
                    self.run(&expression1, &expression2).evaluate(env),
                )
            }
        })
    }
}
