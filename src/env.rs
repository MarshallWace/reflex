// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::rc::Rc;

use crate::expression::{Expression, StackOffset};

#[derive(Debug, PartialEq, Clone)]
pub struct Env {
    values: Vec<Rc<Expression>>,
}
impl Env {
    pub fn new() -> Env {
        Env { values: Vec::new() }
    }
    pub fn get(&self, offset: StackOffset) -> Rc<Expression> {
        Rc::clone(&self.values[self.values.len() - offset - 1])
    }
    pub fn extend(&self, values: impl IntoIterator<Item = Rc<Expression>>) -> Env {
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
