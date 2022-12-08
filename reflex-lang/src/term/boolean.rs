// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::collections::HashSet;

use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;

use reflex::core::{
    BooleanTermType, DependencyList, Eagerness, GraphNode, Internable, SerializeJson, StackOffset,
};

#[derive(PartialEq, Eq, Clone, Copy, Serialize, Deserialize, Hash)]
pub struct BooleanTerm {
    value: bool,
}
impl BooleanTerm {
    pub fn new(value: bool) -> Self {
        Self { value }
    }
}
impl BooleanTermType for BooleanTerm {
    fn value(&self) -> bool {
        self.value
    }
}
impl GraphNode for BooleanTerm {
    fn size(&self) -> usize {
        1
    }
    fn capture_depth(&self) -> StackOffset {
        0
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        HashSet::new()
    }
    fn count_variable_usages(&self, _offset: StackOffset) -> usize {
        0
    }
    fn dynamic_dependencies(&self, _deep: bool) -> DependencyList {
        DependencyList::empty()
    }
    fn has_dynamic_dependencies(&self, _deep: bool) -> bool {
        false
    }
    fn is_static(&self) -> bool {
        true
    }
    fn is_atomic(&self) -> bool {
        true
    }
    fn is_complex(&self) -> bool {
        false
    }
}

impl Internable for BooleanTerm {
    fn should_intern(&self, _eager: Eagerness) -> bool {
        true
    }
}

impl std::fmt::Display for BooleanTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}
impl std::fmt::Debug for BooleanTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}
impl SerializeJson for BooleanTerm {
    fn to_json(&self) -> Result<JsonValue, String> {
        Ok(JsonValue::Bool(self.value))
    }

    fn patch(&self, target: &Self) -> Result<Option<JsonValue>, String> {
        if self.value == target.value {
            Ok(None)
        } else {
            target.to_json().map(Option::Some)
        }
    }
}
