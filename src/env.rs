// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    borrow::BorrowMut,
    cell::{Ref, RefCell},
    rc::Rc,
};

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
    pub fn extend_recursive(
        &self,
        values: impl FnOnce(&MutableEnv<T>) -> Vec<Expression<T>>,
    ) -> MutableEnv<T> {
        let child_env = MutableEnv::from(self);
        (*child_env.state)
            .borrow_mut()
            .as_mut()
            .unwrap()
            .bindings
            .extend(values(&child_env));
        child_env
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct MutableEnv<T: NodeType<T>> {
    state: Rc<RefCell<Option<Env<T>>>>,
}
impl<T: NodeType<T>> MutableEnv<T> {
    fn from(env: &Env<T>) -> Self {
        MutableEnv {
            state: Rc::new(RefCell::new(Some(Env {
                bindings: env.bindings.iter().cloned().collect(),
            }))),
        }
    }
    pub fn get(&self) -> Ref<'_, Env<T>> {
        let state = (*self.state).borrow();
        Ref::map(state, |value| value.as_ref().unwrap())
    }
}
impl<T: NodeType<T>> Drop for MutableEnv<T> {
    fn drop(&mut self) {
        if (*self.state).borrow().is_none() {
            return;
        }
        let state = self.state.borrow_mut();
        let disposed_env = state.replace(None);
        if let Some(mut env) = disposed_env {
            env.bindings.clear();
        }
    }
}
