// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::collections::HashSet;

use serde::{Deserialize, Serialize};

use reflex::core::{
    transform_expression_list, CompoundNode, DependencyList, DynamicState, Eagerness,
    EvaluationCache, Expression, ExpressionFactory, ExpressionListSlice, ExpressionListType,
    GraphNode, HeapAllocator, Internable, RecordTermType, Rewritable, SerializeJson, StackOffset,
    StructPrototypeType, Substitutions, TermHash,
};

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct RecordTerm<T: Expression> {
    prototype: T::StructPrototype,
    values: T::ExpressionList,
}
impl<T: Expression> TermHash for RecordTerm<T> {}
impl<T: Expression> RecordTerm<T> {
    pub fn new(prototype: T::StructPrototype, values: T::ExpressionList) -> Self {
        Self { prototype, values }
    }
    pub fn entries(&self) -> impl IntoIterator<Item = (&T, &T)> {
        self.prototype.keys().iter().zip(self.values.iter())
    }
}
impl<T: Expression> RecordTermType<T> for RecordTerm<T> {
    fn prototype(&self) -> &T::StructPrototype {
        &self.prototype
    }
    fn values(&self) -> &T::ExpressionList {
        &self.values
    }
}
impl<T: Expression> GraphNode for RecordTerm<T> {
    fn capture_depth(&self) -> StackOffset {
        self.values.capture_depth()
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        self.values.free_variables()
    }
    fn count_variable_usages(&self, offset: StackOffset) -> usize {
        self.values.count_variable_usages(offset)
    }
    fn dynamic_dependencies(&self, deep: bool) -> DependencyList {
        if deep {
            self.values.dynamic_dependencies(deep)
        } else {
            DependencyList::empty()
        }
    }
    fn has_dynamic_dependencies(&self, deep: bool) -> bool {
        if deep {
            self.values.has_dynamic_dependencies(deep)
        } else {
            false
        }
    }
    fn is_static(&self) -> bool {
        true
    }
    fn is_atomic(&self) -> bool {
        self.values.is_atomic()
    }
    fn is_complex(&self) -> bool {
        true
    }
}
pub type RecordTermChildren<'a, T> = ExpressionListSlice<'a, T>;
impl<'a, T: Expression + 'a> CompoundNode<'a, T> for RecordTerm<T> {
    type Children = RecordTermChildren<'a, T>;
    fn children(&'a self) -> Self::Children {
        self.values.iter()
    }
}
impl<T: Expression + Rewritable<T>> Rewritable<T> for RecordTerm<T> {
    fn substitute_static(
        &self,
        substitutions: &Substitutions<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        transform_expression_list(&self.values, allocator, |expression| {
            expression.substitute_static(substitutions, factory, allocator, cache)
        })
        .map(|fields| {
            factory.create_record_term(allocator.clone_struct_prototype(&self.prototype), fields)
        })
    }
    fn substitute_dynamic(
        &self,
        deep: bool,
        state: &impl DynamicState<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        if deep {
            transform_expression_list(&self.values, allocator, |expression| {
                expression.substitute_dynamic(deep, state, factory, allocator, cache)
            })
            .map(|fields| {
                factory
                    .create_record_term(allocator.clone_struct_prototype(&self.prototype), fields)
            })
        } else {
            None
        }
    }
    fn hoist_free_variables(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Option<T> {
        transform_expression_list(&self.values, allocator, |expression| {
            expression.hoist_free_variables(factory, allocator)
        })
        .map(|fields| {
            factory.create_record_term(allocator.clone_struct_prototype(&self.prototype), fields)
        })
    }
    fn normalize(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        transform_expression_list(&self.values, allocator, |expression| {
            expression.normalize(factory, allocator, cache)
        })
        .map(|fields| {
            factory.create_record_term(allocator.clone_struct_prototype(&self.prototype), fields)
        })
    }
}

impl<T: Expression> Internable for RecordTerm<T> {
    fn should_intern(&self, _eager: Eagerness) -> bool {
        self.capture_depth() == 0
    }
}

impl<T: Expression> std::fmt::Display for RecordTerm<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.prototype.keys().len() {
            0 => write!(f, "{{}}"),
            _ => write!(
                f,
                "{{ {} }}",
                self.prototype
                    .keys()
                    .iter()
                    .zip(self.values.iter())
                    .map(|(key, value)| format!("{}: {}", key, value))
                    .collect::<Vec<_>>()
                    .join(", "),
            ),
        }
    }
}
impl<T: Expression> SerializeJson for RecordTerm<T> {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        let fields = self
            .prototype()
            .keys()
            .iter()
            .zip(self.values().iter())
            .map(|(key, value)| {
                let key = key.to_json()?;
                let value = value.to_json()?;
                match key {
                    serde_json::Value::String(key) => Ok((key, value)),
                    _ => Err(format!("Invalid JSON object key: {}", key.to_string())),
                }
            })
            .collect::<Result<serde_json::Map<_, _>, _>>()?;
        Ok(serde_json::Value::Object(fields))
    }
}
