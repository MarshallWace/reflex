// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{collections::HashSet, iter::once};

use crate::{
    compiler::{Compile, Compiler, Instruction, Program},
    core::{
        DependencyList, DynamicState, EvaluationCache, Expression, ExpressionFactory, GraphNode,
        HeapAllocator, Reducible, Rewritable, ScopeOffset, SerializeJson, StackOffset,
        Substitutions, VarArgs,
    },
};

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct LetTerm<T: Expression> {
    initializer: T,
    body: T,
}
impl<T: Expression> LetTerm<T> {
    pub fn new(initializer: T, body: T) -> Self {
        Self { initializer, body }
    }
    pub fn initializer(&self) -> &T {
        &self.initializer
    }
    pub fn body(&self) -> &T {
        &self.body
    }
}
impl<T: Expression> GraphNode for LetTerm<T> {
    fn capture_depth(&self) -> StackOffset {
        self.initializer
            .capture_depth()
            .max(self.body.capture_depth().saturating_sub(1))
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        self.initializer
            .free_variables()
            .into_iter()
            .chain(self.body.free_variables().into_iter().filter_map(|offset| {
                if offset == 0 {
                    None
                } else {
                    Some(offset - 1)
                }
            }))
            .collect()
    }
    fn dynamic_dependencies(&self, deep: bool) -> DependencyList {
        self.initializer
            .dynamic_dependencies(deep)
            .union(self.body.dynamic_dependencies(deep))
    }
    fn has_dynamic_dependencies(&self, deep: bool) -> bool {
        self.initializer.has_dynamic_dependencies(deep) || self.body.has_dynamic_dependencies(deep)
    }
    fn is_static(&self) -> bool {
        false
    }
    fn is_atomic(&self) -> bool {
        false
    }
}
impl<T: Expression + Rewritable<T> + Reducible<T>> Rewritable<T> for LetTerm<T> {
    fn subexpressions(&self) -> Vec<&T> {
        once(&self.initializer)
            .chain(self.initializer.subexpressions())
            .chain(once(&self.body))
            .chain(self.body.subexpressions())
            .collect()
    }
    fn substitute_static(
        &self,
        substitutions: &Substitutions<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        let initializer =
            self.initializer
                .substitute_static(substitutions, factory, allocator, cache);
        let body = self
            .body
            .substitute_static(&substitutions.offset(1), factory, allocator, cache);
        if initializer.is_none() && body.is_none() {
            return None;
        } else {
            Some(factory.create_let_term(
                initializer.unwrap_or_else(|| self.initializer.clone()),
                body.unwrap_or_else(|| self.body.clone()),
            ))
        }
    }
    fn substitute_dynamic(
        &self,
        deep: bool,
        state: &impl DynamicState<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        let initializer = self
            .initializer
            .substitute_dynamic(deep, state, factory, allocator, cache);
        let body = self
            .body
            .substitute_dynamic(deep, state, factory, allocator, cache);
        if initializer.is_none() && body.is_none() {
            return None;
        } else {
            Some(factory.create_let_term(
                initializer.unwrap_or_else(|| self.initializer.clone()),
                body.unwrap_or_else(|| self.body.clone()),
            ))
        }
    }
    fn hoist_free_variables(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Option<T> {
        let initializer = self.initializer.hoist_free_variables(factory, allocator);
        let body = self.body.hoist_free_variables(factory, allocator);
        if initializer.is_none() && body.is_none() {
            return None;
        } else {
            Some(factory.create_let_term(
                initializer.unwrap_or_else(|| self.initializer.clone()),
                body.unwrap_or_else(|| self.body.clone()),
            ))
        }
    }
    fn normalize(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        let normalized_initializer = self.initializer.normalize(factory, allocator, cache);
        let normalized_body = self.body.normalize(factory, allocator, cache);
        let substituted_expression = match normalized_initializer {
            Some(initializer) => Some(factory.create_let_term(
                initializer,
                normalized_body.unwrap_or_else(|| self.body.clone()),
            )),
            None => {
                normalized_body.map(|body| factory.create_let_term(self.initializer.clone(), body))
            }
        };
        let reduced_expression = match substituted_expression {
            Some(expression) => expression
                .reduce(factory, allocator, cache)
                .or_else(|| Some(expression)),
            None => self.reduce(factory, allocator, cache),
        };
        reduced_expression.map(|expression| {
            expression
                .normalize(factory, allocator, cache)
                .unwrap_or_else(|| expression)
        })
    }
}
impl<T: Expression + Rewritable<T>> Reducible<T> for LetTerm<T> {
    fn is_reducible(&self) -> bool {
        true
    }
    fn reduce(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        if self.body.capture_depth() == 0 {
            Some(self.body.clone())
        } else {
            let substitutions = vec![(0, self.initializer.clone())];
            let substitutions = Substitutions::named(&substitutions, Some(ScopeOffset::Unwrap(1)));
            self.body
                .substitute_static(&substitutions, factory, allocator, cache)
        }
    }
}
impl<T: Expression> std::fmt::Display for LetTerm<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<let:{}>", self.initializer)
    }
}
impl<T: Expression + Rewritable<T> + Reducible<T> + Compile<T>> Compile<T> for LetTerm<T> {
    fn compile(
        &self,
        eager: VarArgs,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<Program, String> {
        let compiled_initializer =
            self.initializer
                .compile(eager, stack_offset, factory, allocator, compiler)?;
        let compiled_body = self.body.compile(eager, 0, factory, allocator, compiler)?;
        let mut program = compiled_initializer;
        program.extend(compiled_body);
        program.push(Instruction::Squash { depth: 1 });
        Ok(program)
    }
}

impl<T: Expression> SerializeJson for LetTerm<T> {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        Err(format!("Unable to serialize term: {}", self))
    }
}
