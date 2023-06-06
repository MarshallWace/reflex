// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::collections::HashSet;

use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;

use reflex::core::{
    ConditionListType, DependencyList, Eagerness, Expression, GraphNode, Internable, SerializeJson,
    SignalTermType, StackOffset,
};

#[derive(Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct SignalTerm<T: Expression> {
    signals: T::SignalList,
}

impl<T: Expression> std::hash::Hash for SignalTerm<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.signals.id().hash(state);
    }
}

impl<T: Expression> SignalTerm<T> {
    pub fn new(signals: T::SignalList) -> Self {
        Self { signals }
    }
}
impl<T: Expression> SignalTermType<T> for SignalTerm<T> {
    fn signals<'a>(&'a self) -> T::SignalListRef<'a>
    where
        T::SignalList: 'a,
        T: 'a,
        Self: 'a,
    {
        (&self.signals).into()
    }
}
impl<T: Expression> GraphNode for SignalTerm<T> {
    fn size(&self) -> usize {
        1 + self.signals.len()
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
        false
    }
    fn is_complex(&self) -> bool {
        false
    }
}

impl<T: Expression> Internable for SignalTerm<T> {
    fn should_intern(&self, _eager: Eagerness) -> bool {
        true
    }
}

impl<T: Expression> std::fmt::Display for SignalTerm<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}]",
            self.signals
                .iter()
                .map(|signal| format!("{}", signal))
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}

impl<T: Expression> SerializeJson for SignalTerm<T> {
    fn to_json(&self) -> Result<JsonValue, String> {
        Err(format!("Unable to serialize term: {}", self))
    }
    fn patch(&self, target: &Self) -> Result<Option<JsonValue>, String> {
        Err(format!(
            "Unable to create patch for terms: {}, {}",
            self, target
        ))
    }
}
