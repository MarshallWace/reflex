// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{store::Store, types::Expression};

pub trait Evaluate {
    fn evaluate(&self, store: &Store) -> Expression;
}

pub trait Evaluate1: Evaluate {
    fn dependencies(&self) -> &Expression;
    fn run(&self, store: &Store, dep1: &Expression) -> Expression;
    fn evaluate(&self, store: &Store) -> Expression {
        let dep1 = self.dependencies();
        let dep1 = dep1.evaluate(store);
        match &dep1 {
            Expression::Error(_) | Expression::Pending => dep1,
            dep1 => self.run(store, dep1).evaluate(store),
        }
    }
}

pub trait Evaluate2: Evaluate {
    fn dependencies(&self) -> (&Expression, &Expression);
    fn run(&self, store: &Store, dep1: &Expression, dep2: &Expression) -> Expression;
    fn evaluate(&self, store: &Store) -> Expression {
        let (dep1, dep2) = self.dependencies();
        let dep1 = dep1.evaluate(store);
        let dep2 = dep2.evaluate(store);
        match (&dep1, &dep2) {
            (Expression::Error(_), _) => dep1,
            (_, Expression::Error(_)) => dep2,
            (Expression::Pending, _) | (_, Expression::Pending) => Expression::Pending,
            (dep1, dep2) => self.run(store, dep1, dep2).evaluate(store),
        }
    }
}
