// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{fmt, iter::once};

use crate::{
    cache::EvaluationCache,
    core::{
        capture_depth_multiple, dynamic_dependencies_multiple, normalize_multiple,
        substitute_dynamic_multiple, substitute_static_multiple, DependencyList, DynamicState,
        Expression, Rewritable, StackOffset, Substitutions, Term,
    },
};

use super::CollectionTerm;

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct VectorTerm {
    items: Vec<Expression>,
}
impl VectorTerm {
    pub fn new(items: impl IntoIterator<Item = Expression>) -> Self {
        Self {
            items: items.into_iter().collect(),
        }
    }
    pub fn get(&self, index: usize) -> Option<&Expression> {
        self.items.get(index)
    }
    pub fn len(&self) -> usize {
        self.items.len()
    }
    pub fn items(&self) -> &[Expression] {
        &self.items
    }
    pub fn push(&self, value: Expression) -> Expression {
        Expression::new(Term::Collection(CollectionTerm::Vector(Self::new(
            self.items.iter().map(Expression::clone).chain(once(value)),
        ))))
    }
    pub fn push_front(&self, value: Expression) -> Expression {
        Expression::new(Term::Collection(CollectionTerm::Vector(Self::new(
            once(value).chain(self.items.iter().map(Expression::clone)),
        ))))
    }
    pub fn iterate(&self) -> impl IntoIterator<Item = Expression> + ExactSizeIterator + '_ {
        self.items.iter().map(|item| Expression::clone(item))
    }
    pub fn collect(items: impl IntoIterator<Item = Expression>) -> Expression {
        Expression::new(Term::Collection(CollectionTerm::Vector(Self::new(items))))
    }
}
impl Rewritable for VectorTerm {
    fn capture_depth(&self) -> StackOffset {
        capture_depth_multiple(&self.items)
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        dynamic_dependencies_multiple(&self.items)
    }
    fn substitute_static(
        &self,
        substitutions: &Substitutions,
        cache: &mut impl EvaluationCache,
    ) -> Option<Expression> {
        substitute_static_multiple(&self.items, substitutions, cache).map(|items| {
            Expression::new(Term::Collection(CollectionTerm::Vector(Self::new(items))))
        })
    }
    fn substitute_dynamic(
        &self,
        state: &DynamicState,
        cache: &mut impl EvaluationCache,
    ) -> Option<Expression> {
        substitute_dynamic_multiple(&self.items, state, cache).map(|items| {
            Expression::new(Term::Collection(CollectionTerm::Vector(Self::new(items))))
        })
    }
    fn normalize(&self, cache: &mut impl EvaluationCache) -> Option<Expression> {
        normalize_multiple(&self.items, cache).map(|items| {
            Expression::new(Term::Collection(CollectionTerm::Vector(Self::new(items))))
        })
    }
}
impl fmt::Display for VectorTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let max_displayed_items = 100;
        let items = &self.items;
        let num_items = items.len();
        write!(
            f,
            "[{}]",
            if num_items <= max_displayed_items {
                items
                    .iter()
                    .map(|item| format!("{}", item))
                    .collect::<Vec<_>>()
                    .join(", ")
            } else {
                items
                    .iter()
                    .take(max_displayed_items - 1)
                    .map(|item| format!("{}", item))
                    .chain(once(format!(
                        "...{} more items",
                        num_items - (max_displayed_items - 1)
                    )))
                    .collect::<Vec<_>>()
                    .join(", ")
            }
        )
    }
}
