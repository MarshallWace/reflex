// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{collections::HashSet, hash::Hash};

use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;

use reflex::core::{
    DependencyList, Eagerness, Expression, GraphNode, Internable, SerializeJson, StackOffset,
    StringTermType, StringValue,
};

#[derive(PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
pub struct StringTerm<T: Expression> {
    value: T::String,
}

impl<T: Expression> Hash for StringTerm<T>
where
    T::String: Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}

impl<T: Expression> StringTerm<T> {
    pub fn new(value: T::String) -> Self {
        Self { value }
    }
}
impl<T: Expression> StringTermType<T> for StringTerm<T> {
    fn value<'a>(&'a self) -> T::StringRef<'a>
    where
        T::String: 'a,
        T: 'a,
        Self: 'a,
    {
        (&self.value).into()
    }
}
impl<T: Expression> GraphNode for StringTerm<T> {
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

impl<T: Expression> Internable for StringTerm<T> {
    fn should_intern(&self, _eager: Eagerness) -> bool {
        true
    }
}

impl<T: Expression> std::fmt::Display for StringTerm<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.value)
    }
}
impl<T: Expression> std::fmt::Debug for StringTerm<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}
impl<T: Expression> SerializeJson for StringTerm<T> {
    fn to_json(&self) -> Result<JsonValue, String> {
        Ok(JsonValue::String(String::from(self.value.as_str())))
    }
    fn patch(&self, target: &Self) -> Result<Option<JsonValue>, String> {
        if self.value == target.value {
            Ok(None)
        } else {
            target.to_json().map(Some)
        }
    }
}
