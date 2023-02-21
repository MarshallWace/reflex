// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::collections::HashMap;

use reflex::core::Expression;

mod loader;
pub use loader::{
    compose_module_loaders, create_js_env, create_module_loader, static_module_loader,
};
mod parser;
pub use parser::{parse, parse_module, JsParserBuiltin};

pub mod builtins;
pub mod globals;
pub mod imports;
pub use globals::builtin_globals;
pub use imports::builtin_imports;

pub mod stdlib;

#[derive(Clone)]
pub struct Env<T: Expression> {
    globals: HashMap<&'static str, T>,
}
impl<T: Expression> Env<T> {
    pub fn new() -> Self {
        Self {
            globals: HashMap::new(),
        }
    }
    pub fn with_globals(mut self, values: impl IntoIterator<Item = (&'static str, T)>) -> Self {
        self.globals.extend(values);
        self
    }
    pub fn with_global(mut self, key: &'static str, value: T) -> Self {
        self.globals.insert(key, value);
        self
    }
    pub fn global(&self, name: &str) -> Option<T> {
        self.globals.get(name).cloned()
    }
}
