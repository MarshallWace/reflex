// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    env::Env,
    expression::Expression,
    node::{core::CoreNode, Node},
};

pub trait Evaluate1 {
    fn dependencies(&self) -> &Expression<Node>;
    fn run(&self, env: &Env<Node>, dep1: &Expression<Node>) -> Expression<Node>;
    fn evaluate(&self, env: &Env<Node>) -> Option<Expression<Node>> {
        let dep1 = self.dependencies();
        let dep1 = dep1.evaluate(env);
        Some(match dep1.value() {
            Node::Core(CoreNode::Error(_)) | Node::Core(CoreNode::Pending(_)) => dep1,
            _ => self.run(env, &dep1).evaluate(env),
        })
    }
}

pub trait Evaluate2 {
    fn dependencies(&self) -> (&Expression<Node>, &Expression<Node>);
    fn run(
        &self,
        env: &Env<Node>,
        dep1: &Expression<Node>,
        dep2: &Expression<Node>,
    ) -> Expression<Node>;
    fn evaluate(&self, env: &Env<Node>) -> Option<Expression<Node>> {
        let (dep1, dep2) = self.dependencies();
        let dep1 = dep1.evaluate(env);
        let dep2 = dep2.evaluate(env);
        Some(match (dep1.value(), dep2.value()) {
            (Node::Core(CoreNode::Error(_)), _) => dep1,
            (_, Node::Core(CoreNode::Error(_))) => dep2,
            (Node::Core(CoreNode::Pending(_)), _) => dep1,
            (_, Node::Core(CoreNode::Pending(_))) => dep2,
            (_, _) => self.run(env, &dep1, &dep2).evaluate(env),
        })
    }
}
