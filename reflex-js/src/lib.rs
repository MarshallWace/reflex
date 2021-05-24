// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::Expression;
use std::collections::HashMap;

pub mod builtins;
mod loader;
pub use loader::{dynamic_module_loader, static_module_loader};
mod parser;
pub use parser::{parse, parse_module};
pub mod stdlib {
    pub mod globals;
    pub mod imports;
    pub use globals::{
        builtin_globals, global_json_parse, global_json_stringify, global_process, json_stringify,
        json_stringify_string,
    };
    pub use imports::builtin_imports;
}

pub struct Env {
    globals: HashMap<&'static str, Expression>,
}
impl Env {
    pub fn new() -> Self {
        Self {
            globals: HashMap::new(),
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
    pub fn global(&self, name: &str) -> Option<Expression> {
        self.globals.get(name).map(Expression::clone)
    }
}
