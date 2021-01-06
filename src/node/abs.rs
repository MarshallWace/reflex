// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    expression::Expression,
    operation::evaluate::{Evaluate, Evaluate1},
    store::Store,
    value::Value,
    utils::format_type,
};

#[derive(Debug, PartialEq)]
pub struct AbsNode {
    target: Box<Expression>,
}
impl AbsNode {
    pub fn new(target: Expression) -> AbsNode {
        AbsNode {
            target: Box::new(target),
        }
    }
}
impl Evaluate for AbsNode {
    fn evaluate(&self, store: &Store) -> Expression {
        Evaluate1::evaluate(self, store)
    }
}
impl Evaluate1 for AbsNode {
    fn dependencies(&self) -> &Expression {
        &self.target
    }
    fn run(&self, _store: &Store, target: &Expression) -> Expression {
        match target {
            Expression::Value(Value::Int(target)) => Expression::Value(Value::Int(target.abs())),
            Expression::Value(Value::Float(target)) => {
                Expression::Value(Value::Float(target.abs()))
            }
            target => Expression::Error(format!(
                "Expected Int or Float, received {}",
                format_type(target),
            )),
        }
    }
}
