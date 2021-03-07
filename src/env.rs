// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    borrow::BorrowMut,
    cell::{Ref, RefCell},
    rc::Rc,
};

use crate::{
    expression::{Expression, NodeType},
    hash::combine_hashes,
};

pub type StackOffset = usize;

#[derive(Debug, PartialEq, Clone)]
pub struct Env<T: NodeType<T>> {
    hash: u32,
    bindings: Vec<Expression<T>>,
}
impl<T: NodeType<T>> Env<T> {
    pub fn new() -> Self {
        Env {
            hash: 0,
            bindings: Vec::new(),
        }
    }
    pub fn from(values: impl IntoIterator<Item = Expression<T>>) -> Env<T> {
        let bindings = values.into_iter().collect::<Vec<_>>();
        let hash = hash_expressions(&bindings);
        Env { hash, bindings }
    }
    pub fn hash(&self) -> u32 {
        self.hash
    }
    pub fn get(&self, offset: StackOffset) -> &Expression<T> {
        &self.bindings[self.bindings.len() - offset - 1]
    }
    pub fn capture(&self, offset: StackOffset) -> Env<T> {
        let bindings = self
            .bindings
            .iter()
            .skip(self.bindings.len() - offset)
            .cloned()
            .collect::<Vec<_>>();
        let hash = hash_expressions(&bindings);
        Env { hash, bindings }
    }
    pub fn extend(&self, values: impl IntoIterator<Item = Expression<T>>) -> Env<T> {
        Env::from(self.bindings.iter().cloned().chain(values.into_iter()))
    }
    pub fn extend_recursive(
        &self,
        values: impl FnOnce(&MutableEnv<T>) -> Vec<Expression<T>>,
    ) -> MutableEnv<T> {
        let child_env = MutableEnv::from(self);
        {
            let mut state = (*child_env.state).borrow_mut();
            let env = state.as_mut().unwrap();
            env.bindings.extend(values(&child_env));
        }
        child_env
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct MutableEnv<T: NodeType<T>> {
    state: Rc<RefCell<Option<Env<T>>>>,
    hash: u32,
}
impl<T: NodeType<T>> MutableEnv<T> {
    fn from(env: &Env<T>) -> Self {
        MutableEnv {
            hash: env.hash,
            state: Rc::new(RefCell::new(Some(env.clone()))),
        }
    }
    pub fn hash(&self) -> u32 {
        self.hash
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

fn hash_expressions<T: NodeType<T>>(expressions: &Vec<Expression<T>>) -> u32 {
    combine_hashes(
        &expressions
            .iter()
            .map(|expression| expression.hash())
            .collect::<Vec<_>>(),
    )
}
