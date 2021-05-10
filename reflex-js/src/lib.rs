// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::Expression;
use std::collections::HashMap;

mod builtins;
mod globals;
mod imports;
mod parser;

pub use globals::{builtin_globals, builtin_process};
pub use imports::builtin_imports;
pub use parser::parse;

pub struct Env {
    globals: HashMap<&'static str, Expression>,
    imports: HashMap<&'static str, Expression>,
}
impl Env {
    pub fn new() -> Self {
        Self {
            globals: HashMap::new(),
            imports: HashMap::new(),
        }
    }
    pub fn with_globals(
        mut self,
        values: impl IntoIterator<Item = (&'static str, Expression)>,
    ) -> Self {
        self.globals.extend(values);
        self
    }
    pub fn with_global(mut self, key: &'static str, value: Expression) -> Self {
        self.globals.insert(key, value);
        self
    }
    pub fn with_imports(
        mut self,
        values: impl IntoIterator<Item = (&'static str, Expression)>,
    ) -> Self {
        self.imports.extend(values);
        self
    }
    pub fn with_import(mut self, key: &'static str, value: Expression) -> Self {
        self.imports.insert(key, value);
        self
    }
    pub fn import(&self, name: &str) -> Option<Expression> {
        self.imports.get(name).map(Expression::clone)
    }
    pub fn global(&self, name: &str) -> Option<Expression> {
        self.globals.get(name).map(Expression::clone)
    }
}
