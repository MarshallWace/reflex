// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    env::Env,
    expression::{EvaluationResult, Expression},
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
        Some(match dep1.value() {
            Node::Core(CoreNode::Error(_)) | Node::Core(CoreNode::Pending(_)) => dep1,
            _ => self
                .run(&dep1.expression())
                .evaluate(env)
                .with_dependencies_from(dep1),
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
        Some(match (dep1.value(), dep2.value()) {
            (Node::Core(CoreNode::Error(_)), _) => dep1,
            (_, Node::Core(CoreNode::Error(_))) => dep2,
            (Node::Core(CoreNode::Pending(_)), _) => dep1,
            (_, Node::Core(CoreNode::Pending(_))) => dep2,
            (_, _) => self
                .run(&dep1.expression(), &dep2.expression())
                .evaluate(env)
                .with_dependencies_from(dep1)
                .with_dependencies_from(dep2),
        })
    }
}
