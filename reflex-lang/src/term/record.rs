// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::collections::HashSet;

use serde::{Deserialize, Serialize};

use reflex::core::{
    transform_expression_list, CompoundNode, DependencyList, DynamicState, Eagerness,
    EvaluationCache, Expression, ExpressionFactory, ExpressionListIter, ExpressionListType,
    GraphNode, HeapAllocator, Internable, RecordTermType, RefType, Rewritable, SerializeJson,
    StackOffset, StructPrototypeType, Substitutions,
};

#[derive(Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct RecordTerm<T: Expression> {
    prototype: T::StructPrototype<T>,
    values: T::ExpressionList<T>,
}

impl<T: Expression> std::hash::Hash for RecordTerm<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.prototype.keys().as_deref().id().hash(state);
        self.values.id().hash(state);
    }
}

impl<T: Expression> RecordTerm<T> {
    pub fn new(prototype: T::StructPrototype<T>, values: T::ExpressionList<T>) -> Self {
        Self { prototype, values }
    }
    pub fn entries(&self) -> impl IntoIterator<Item = (&T, &T)> {
        self.prototype
            .keys()
            .as_deref()
            .iter()
            .map(|item| item.as_deref())
            .zip(self.values.iter().map(|item| item.as_deref()))
    }
}
impl<T: Expression> RecordTermType<T> for RecordTerm<T> {
    fn prototype<'a>(&'a self) -> T::Ref<'a, T::StructPrototype<T>>
    where
        T::StructPrototype<T>: 'a,
        T: 'a,
    {
        (&self.prototype).into()
    }
    fn values<'a>(&'a self) -> T::Ref<'a, T::ExpressionList<T>>
    where
        T::ExpressionList<T>: 'a,
        T: 'a,
    {
        (&self.values).into()
    }
    fn get<'a>(&'a self, key: &T) -> Option<T::Ref<'a, T>>
    where
        T: 'a,
    {
        let index = self
            .prototype
            .keys()
            .as_deref()
            .iter()
            .map(|item| item.as_deref())
            .position(|field_name| field_name == key)?;
        self.values.get(index)
    }
}
impl<T: Expression> GraphNode for RecordTerm<T> {
    fn size(&self) -> usize {
        1 + self.values.size()
    }
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
impl<T: Expression> CompoundNode<T> for RecordTerm<T> {
    type Children<'a> = ExpressionListIter<'a, T>
        where
            T: 'a,
            Self: 'a;
    fn children<'a>(&'a self) -> Self::Children<'a> {
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
            factory.create_record_term(
                allocator.clone_struct_prototype((&self.prototype).into()),
                fields,
            )
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
                factory.create_record_term(
                    allocator.clone_struct_prototype((&self.prototype).into()),
                    fields,
                )
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
            factory.create_record_term(
                allocator.clone_struct_prototype((&self.prototype).into()),
                fields,
            )
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
            factory.create_record_term(
                allocator.clone_struct_prototype((&self.prototype).into()),
                fields,
            )
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
        match self.prototype.keys().as_deref().len() {
            0 => write!(f, "{{}}"),
            _ => write!(
                f,
                "{{ {} }}",
                self.prototype
                    .keys()
                    .as_deref()
                    .iter()
                    .map(|item| item.as_deref())
                    .zip(self.values.iter().map(|item| item.as_deref()))
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
            .prototype
            .keys()
            .as_deref()
            .iter()
            .map(|item| item.as_deref())
            .zip(self.values.iter().map(|item| item.as_deref()))
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
