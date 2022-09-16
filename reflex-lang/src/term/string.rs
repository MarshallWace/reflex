// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::collections::HashSet;

use serde::{Deserialize, Serialize};

use reflex::core::{
    DependencyList, Eagerness, Expression, GraphNode, Internable, SerializeJson, StackOffset,
    StringTermType, StringValue, TermHash,
};

#[derive(PartialEq, Eq, Clone, Copy, Serialize, Deserialize, Hash)]
pub struct StringTerm<T: Expression> {
    value: T::String,
}
impl<T: Expression> StringTerm<T> {
    pub fn new(value: T::String) -> Self {
        Self { value }
    }
}
impl<T: Expression> TermHash for StringTerm<T> {}
impl<T: Expression> StringTermType<T> for StringTerm<T> {
    fn value<'a>(&'a self) -> T::Ref<'a, T::String>
    where
        T::String: 'a,
        T: 'a,
        Self: 'a,
    {
        (&self.value).into()
    }
}
impl<T: Expression> GraphNode for StringTerm<T> {
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
    fn to_json(&self) -> Result<serde_json::Value, String> {
        Ok(serde_json::Value::String(String::from(self.value.as_str())))
    }
}
