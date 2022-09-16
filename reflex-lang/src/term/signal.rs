// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::collections::HashSet;

use serde::{Deserialize, Serialize};

use reflex::core::{
    ConditionListType, DependencyList, Eagerness, Expression, GraphNode, Internable, RefType,
    SerializeJson, SignalTermType, StackOffset, TermHash,
};

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct SignalTerm<T: Expression> {
    signals: T::SignalList<T>,
}
impl<T: Expression> TermHash for SignalTerm<T> {}
impl<T: Expression> SignalTerm<T> {
    pub fn new(signals: T::SignalList<T>) -> Self {
        Self { signals }
    }
}
impl<T: Expression> SignalTermType<T> for SignalTerm<T> {
    fn signals<'a>(&'a self) -> T::Ref<'a, T::SignalList<T>>
    where
        T::SignalList<T>: 'a,
        T: 'a,
        Self: 'a,
    {
        (&self.signals).into()
    }
}
impl<T: Expression> GraphNode for SignalTerm<T> {
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
                .map(|item| item.as_deref())
                .map(|signal| format!("{}", signal))
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}

impl<T: Expression> SerializeJson for SignalTerm<T> {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        Err(format!("Unable to serialize term: {}", self))
    }
}
