// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{fmt, rc::Rc};

use crate::{env::Env, expression::{Expression, StackOffset}};

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub arity: usize,
    pub captures: Option<Vec<StackOffset>>,
    pub body: Rc<Expression>,
}
impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Function({} -> {})", self.arity, self.body)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Closure {
    pub env: Env,
    pub function: Function,
}
impl fmt::Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Closure({} -> {})", self.function.arity, self.function.body)
    }
}
