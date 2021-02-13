// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::expression::{Expression, NodeType};

pub type StackOffset = usize;

#[derive(Debug, PartialEq, Clone)]
pub struct Env<T: NodeType<T>> {
    bindings: Vec<Expression<T>>,
}
impl<T: NodeType<T>> Env<T> {
    pub fn new() -> Self {
        Env {
            bindings: Vec::new(),
        }
    }
    pub fn from(values: impl IntoIterator<Item = Expression<T>>) -> Env<T> {
        Env {
            bindings: values.into_iter().collect(),
        }
    }
    pub fn get(&self, offset: StackOffset) -> &Expression<T> {
        &self.bindings[self.bindings.len() - offset - 1]
    }
    pub fn capture(&self, offset: StackOffset) -> Env<T> {
        Env {
            bindings: self
                .bindings
                .iter()
                .skip(self.bindings.len() - offset)
                .cloned()
                .collect(),
        }
    }
    pub fn extend(&self, values: impl IntoIterator<Item = Expression<T>>) -> Env<T> {
        Env::from(self.bindings.iter().cloned().chain(values.into_iter()))
    }
}
