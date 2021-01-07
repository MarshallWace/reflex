// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{expression::Expression, env::Env};

pub trait Evaluate {
    fn evaluate(&self, env: &Env) -> Expression;
}

pub trait Evaluate1: Evaluate {
    fn dependencies(&self) -> &Expression;
    fn run(&self, env: &Env, dep1: &Expression) -> Expression;
    fn evaluate(&self, env: &Env) -> Expression {
        let dep1 = self.dependencies();
        let dep1 = dep1.evaluate(env);
        match &dep1 {
            Expression::Error(_) | Expression::Pending => dep1,
            dep1 => self.run(env, dep1).evaluate(env),
        }
    }
}

pub trait Evaluate2: Evaluate {
    fn dependencies(&self) -> (&Expression, &Expression);
    fn run(&self, env: &Env, dep1: &Expression, dep2: &Expression) -> Expression;
    fn evaluate(&self, env: &Env) -> Expression {
        let (dep1, dep2) = self.dependencies();
        let dep1 = dep1.evaluate(env);
        let dep2 = dep2.evaluate(env);
        match (&dep1, &dep2) {
            (Expression::Error(_), _) => dep1,
            (_, Expression::Error(_)) => dep2,
            (Expression::Pending, _) | (_, Expression::Pending) => Expression::Pending,
            (dep1, dep2) => self.run(env, dep1, dep2).evaluate(env),
        }
    }
}
