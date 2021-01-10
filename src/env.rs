// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{collections::HashMap, rc::Rc};

use crate::expression::Expression;

#[derive(Debug, PartialEq, Clone)]
pub struct Env {
    values: HashMap<String, Rc<Expression>>,
}
impl Env {
    pub fn new() -> Env {
        Env {
            values: HashMap::new(),
        }
    }
    pub fn get(&self, key: &str) -> Option<Rc<Expression>> {
        self.values.get(key).map(|value| Rc::clone(value))
    }
    pub fn set(&self, values: impl IntoIterator<Item = (String, Rc<Expression>)>) -> Env {
        let child_values = {
            let mut child_values = self
                .values
                .iter()
                .map(|(key, value)| (String::from(key), Rc::clone(value)))
                .collect::<HashMap<_, _>>();
            child_values.extend(
                values
                    .into_iter()
                    .map(|(key, value)| (String::from(key), value)),
            );
            child_values
        };
        let env = Env {
            values: child_values,
        };
        env
    }
}
