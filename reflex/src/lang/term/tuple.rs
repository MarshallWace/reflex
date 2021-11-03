// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{collections::HashSet, iter::once};

use crate::{
    compiler::{compile_expressions, Compile, Compiler, Instruction, Program},
    core::{
        transform_expression_list, DependencyList, DynamicState, EvaluationCache, Expression,
        ExpressionFactory, ExpressionList, GraphNode, HeapAllocator, Rewritable, SerializeJson,
        StackOffset, StructFieldOffset, Substitutions, VarArgs,
    },
};

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct TupleTerm<T: Expression> {
    fields: ExpressionList<T>,
}
impl<T: Expression> TupleTerm<T> {
    pub fn new(fields: ExpressionList<T>) -> Self {
        Self { fields }
    }
    pub fn fields(&self) -> &ExpressionList<T> {
        &self.fields
    }
    pub fn get(&self, field_offset: StructFieldOffset) -> Option<&T> {
        self.fields.get(field_offset)
    }
    pub fn size(&self) -> StructFieldOffset {
        self.fields.len()
    }
}
impl<T: Expression> GraphNode for TupleTerm<T> {
    fn capture_depth(&self) -> StackOffset {
        self.fields.capture_depth()
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        self.fields.free_variables()
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        DependencyList::empty()
    }
    fn is_static(&self) -> bool {
        true
    }
    fn is_atomic(&self) -> bool {
        self.fields.is_atomic()
    }
}
impl<T: Expression + Rewritable<T>> Rewritable<T> for TupleTerm<T> {
    fn subexpressions(&self) -> Vec<&T> {
        self.fields
            .iter()
            .flat_map(|field| once(field).chain(field.subexpressions()))
            .collect()
    }
    fn substitute_static(
        &self,
        substitutions: &Substitutions<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        transform_expression_list(&self.fields, allocator, |expression| {
            expression.substitute_static(substitutions, factory, allocator, cache)
        })
        .map(|fields| factory.create_tuple_term(fields))
    }
    fn substitute_dynamic(
        &self,
        state: &impl DynamicState<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        transform_expression_list(&self.fields, allocator, |expression| {
            expression.substitute_dynamic(state, factory, allocator, cache)
        })
        .map(|fields| factory.create_tuple_term(fields))
    }
    fn hoist_free_variables(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Option<T> {
        transform_expression_list(&self.fields, allocator, |expression| {
            expression.hoist_free_variables(factory, allocator)
        })
        .map(|fields| factory.create_tuple_term(fields))
    }
    fn normalize(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        transform_expression_list(&self.fields, allocator, |expression| {
            expression.normalize(factory, allocator, cache)
        })
        .map(|fields| factory.create_tuple_term(fields))
    }
}
impl<T: Expression + Compile<T>> Compile<T> for TupleTerm<T> {
    fn compile(
        &self,
        _eager: VarArgs,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<Program, String> {
        compile_expressions(
            self.fields.iter(),
            VarArgs::Lazy,
            stack_offset,
            factory,
            allocator,
            compiler,
        )
        .map(|mut program| {
            program.push(Instruction::ConstructTuple {
                size: self.fields.len(),
            });
            program
        })
    }
}
impl<T: Expression> std::fmt::Display for TupleTerm<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({})",
            self.fields
                .iter()
                .map(|value| format!("{}", value))
                .collect::<Vec<_>>()
                .join(", "),
        )
    }
}

impl<T: Expression> SerializeJson for TupleTerm<T> {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        self.fields()
            .iter()
            .map(|key| key.to_json())
            .collect::<Result<Vec<_>, String>>()
            .map(|values| serde_json::Value::Array(values))
    }
}
