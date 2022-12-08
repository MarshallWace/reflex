// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::collections::HashSet;

use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;

use reflex::core::{
    DependencyList, Eagerness, GraphNode, IntTermType, IntValue, Internable, SerializeJson,
    StackOffset,
};

#[derive(PartialEq, Eq, Clone, Copy, Serialize, Deserialize, Hash)]
pub struct IntTerm {
    value: IntValue,
}
impl IntTerm {
    pub fn new(value: IntValue) -> Self {
        Self { value }
    }
}
impl IntTermType for IntTerm {
    fn value(&self) -> IntValue {
        self.value
    }
}
impl GraphNode for IntTerm {
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

impl Internable for IntTerm {
    fn should_intern(&self, _eager: Eagerness) -> bool {
        true
    }
}

impl std::fmt::Display for IntTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}
impl std::fmt::Debug for IntTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}
impl SerializeJson for IntTerm {
    fn to_json(&self) -> Result<JsonValue, String> {
        Ok(JsonValue::Number(self.value.into()))
    }
    fn patch(&self, target: &Self) -> Result<Option<JsonValue>, String> {
        if self.value == target.value {
            Ok(None)
        } else {
            target.to_json().map(Some)
        }
    }
}
