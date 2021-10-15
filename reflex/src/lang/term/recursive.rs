// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{collections::HashSet, iter::once};

use crate::{
    compiler::{Compile, Compiler, Instruction, NativeFunctionRegistry, Program},
    core::{
        DependencyList, DynamicState, EvaluationCache, Expression, ExpressionFactory, GraphNode,
        HeapAllocator, Reducible, Rewritable, StackOffset, Substitutions, VarArgs,
    },
};

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct RecursiveTerm<T: Expression> {
    factory: T,
}
impl<T: Expression> RecursiveTerm<T> {
    pub fn new(factory: T) -> Self {
        Self { factory }
    }
    pub fn factory(&self) -> &T {
        &self.factory
    }
}
impl<T: Expression> GraphNode for RecursiveTerm<T> {
    fn capture_depth(&self) -> StackOffset {
        self.factory.capture_depth()
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        self.factory.free_variables()
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        self.factory.dynamic_dependencies()
    }
    fn is_static(&self) -> bool {
        false
    }
    fn is_atomic(&self) -> bool {
        false
    }
}
impl<T: Expression + Rewritable<T>> Rewritable<T> for RecursiveTerm<T> {
    fn subexpressions(&self) -> Vec<&T> {
        once(&self.factory)
            .chain(self.factory.subexpressions())
            .collect()
    }
    fn substitute_static(
        &self,
        substitutions: &Substitutions<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        self.factory
            .substitute_static(substitutions, factory, allocator, cache)
            .map(|target| factory.create_recursive_term(target))
    }
    fn substitute_dynamic(
        &self,
        state: &impl DynamicState<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        self.factory
            .substitute_dynamic(state, factory, allocator, cache)
            .map(|target| factory.create_recursive_term(target))
    }
    fn hoist_free_variables(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Option<T> {
        self.factory
            .hoist_free_variables(factory, allocator)
            .map(|target| factory.create_recursive_term(target))
    }
    fn normalize(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        self.factory
            .normalize(factory, allocator, cache)
            .map(|target| factory.create_recursive_term(target))
    }
}
impl<T: Expression + Rewritable<T> + Reducible<T>> Reducible<T> for RecursiveTerm<T> {
    fn is_reducible(&self) -> bool {
        true
    }
    fn reduce(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        factory
            .create_application_term(
                self.factory.clone(),
                allocator.create_unit_list(factory.create_recursive_term(self.factory.clone())),
            )
            .reduce(factory, allocator, cache)
    }
}
impl<T: Expression + Compile<T>> Compile<T> for RecursiveTerm<T> {
    fn compile(
        &self,
        eager: VarArgs,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<(Program, NativeFunctionRegistry<T>), String> {
        let (compiled_factory, native_functions) =
            self.factory
                .compile(eager, stack_offset, factory, allocator, compiler)?;
        let mut result = compiled_factory;
        result.push(Instruction::PushStatic { offset: 0 });
        result.push(Instruction::Apply { num_args: 1 });
        Ok((result, native_functions))
    }
}
impl<T: Expression> std::fmt::Display for RecursiveTerm<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<recursive:{}>", format!("{}", self.factory))
    }
}
