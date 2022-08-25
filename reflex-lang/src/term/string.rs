// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::collections::HashSet;

use serde::{Deserialize, Serialize};

use reflex::core::{
    DependencyList, Eagerness, GraphNode, Internable, SerializeJson, StackOffset, StringTermType,
    StringValue, TermHash,
};

#[derive(PartialEq, Eq, Clone, Copy, Serialize, Deserialize, Hash)]
pub struct StringTerm<TString: StringValue> {
    value: TString,
}
impl<TString: StringValue> StringTerm<TString> {
    pub fn new(value: TString) -> Self {
        Self { value }
    }
}
impl<TString: StringValue> TermHash for StringTerm<TString> {}
impl<TString: StringValue> StringTermType<TString> for StringTerm<TString> {
    fn value(&self) -> &TString {
        &self.value
    }
}
impl<TString: StringValue> GraphNode for StringTerm<TString> {
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

impl<TString: StringValue> Internable for StringTerm<TString> {
    fn should_intern(&self, _eager: Eagerness) -> bool {
        true
    }
}

impl<TString: StringValue> std::fmt::Display for StringTerm<TString> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.value)
    }
}
impl<TString: StringValue> std::fmt::Debug for StringTerm<TString> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}
impl<TString: StringValue> SerializeJson for StringTerm<TString> {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        Ok(serde_json::Value::String(String::from(self.value.as_str())))
    }
}
