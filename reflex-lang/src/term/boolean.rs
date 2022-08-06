// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::collections::HashSet;

use serde::{Deserialize, Serialize};

use reflex::core::{
    BooleanTermType, DependencyList, GraphNode, SerializeJson, StackOffset, TermHash,
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
impl TermHash for BooleanTerm {}
impl BooleanTermType for BooleanTerm {
    fn value(&self) -> bool {
        self.value
    }
}
impl GraphNode for BooleanTerm {
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
    fn to_json(&self) -> Result<serde_json::Value, String> {
        Ok(serde_json::Value::Bool(self.value))
    }
}
