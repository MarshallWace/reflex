// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::rc::Rc;

use crate::expression::Expression;

#[derive(Debug, PartialEq, Clone)]
pub struct Env {
    values: Vec<Rc<Expression>>,
}
impl Env {
    pub fn new() -> Env {
        Env { values: Vec::new() }
    }
    pub fn get(&self, index: usize) -> Option<Rc<Expression>> {
        self.values.iter().rev().skip(index).next().map(Rc::clone)
    }
    pub fn set(&self, values: impl IntoIterator<Item = Rc<Expression>>) -> Env {
        Env {
            values: self
                .values
                .iter()
                .map(|value| Rc::clone(value))
                .chain(values.into_iter())
                .collect::<Vec<_>>(),
        }
    }
}
