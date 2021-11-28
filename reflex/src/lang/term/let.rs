// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::iter::once;

use crate::cache::NoopCache;
use crate::core::CompoundNode;
use crate::{
    compiler::{Compile, Compiler, Instruction, Program},
    core::{
        DependencyList, DynamicState, EvaluationCache, Expression, ExpressionFactory, GraphNode,
        HeapAllocator, Reducible, Rewritable, ScopeOffset, SerializeJson, StackOffset,
        Substitutions, VarArgs,
    },
};

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
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
    fn count_variable_usages(&self, offset: StackOffset) -> usize {
        self.body.count_variable_usages(offset + 1)
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
    fn is_complex(&self) -> bool {
        true
    }
}
pub type LetTermChildren<'a, T> = std::iter::Chain<std::iter::Once<&'a T>, std::iter::Once<&'a T>>;
impl<'a, T: Expression + 'a> CompoundNode<'a, T> for LetTerm<T> {
    type Children = LetTermChildren<'a, T>;
    fn children(&'a self) -> Self::Children {
        once(&self.initializer).chain(once(&self.body))
    }
}
impl<T: Expression + Rewritable<T> + Reducible<T>> Rewritable<T> for LetTerm<T> {
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
        let initializer = normalized_initializer.as_ref().unwrap_or(&self.initializer);
        let body = normalized_body.as_ref().unwrap_or(&self.body);
        let can_inline_initializer =
            !initializer.is_complex() || body.count_variable_usages(0) <= 1;
        if can_inline_initializer {
            body.substitute_static(
                &Substitutions::named(
                    &vec![(0, initializer.clone())],
                    Some(ScopeOffset::Unwrap(1)),
                ),
                factory,
                allocator,
                cache,
            )
            .map(|result| {
                result
                    .normalize(factory, allocator, cache)
                    .unwrap_or(result)
            })
            .or_else(|| normalized_body.or_else(|| Some(self.body.clone())))
        } else if normalized_initializer.is_some() || normalized_body.is_some() {
            Some(factory.create_let_term(
                normalized_initializer.unwrap_or_else(|| self.initializer.clone()),
                normalized_body.unwrap_or_else(|| self.body.clone()),
            ))
        } else {
            None
        }
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
        write!(f, "<let:{}:{}>", self.initializer, self.body)
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
        let compiled_body = {
            // Expressions encountered as the (non-first) argument of a function application will have a stack_offset
            // greater than 0, indicating that any stack offsets within the expression will need to be shifted to
            // accommodate the previous arguments which will be occupying slots in the stack at the point of evaluation.
            // Usually this is just a case of relaying the stack offset to any child expressions, which will cause their
            // respective stack offsets to be shifted by the given argument offset, however let-expressions introduce
            // their own child scope that assumes stack offset 0 refers to the initializer and stack offset 1 refers to
            // the most recently-defined variable in the parent scope.
            // This creates problems when there are arguments occupying the slots in between the parent scope and the
            // let-expression inner scope, seeing as offsets 0 and 1 within the inner scope now occupy non-contiguous
            // slots in the interpreter stack.
            // One approach to address this is to manually shift all variables with offset > 0 within the let-expression
            // body and then compile it with a stack offset of 0 to reflect the fact that any variables have already
            // been offset by the correct amount (this is what we do here).
            // A more thorough approach might be to allow an arbitrary function for the stack_offset argument, which
            // would allow returning non-contiguous offsets for variables declared in parent scopes.
            let inner_stack_offset = 0;
            let shifted_body = if stack_offset > 0 {
                self.body.substitute_static(
                    &Substitutions::increase_scope_offset(stack_offset, 1),
                    factory,
                    allocator,
                    &mut NoopCache::default(),
                )
            } else {
                None
            };
            shifted_body.as_ref().unwrap_or(&self.body).compile(
                eager,
                inner_stack_offset,
                factory,
                allocator,
                compiler,
            )
        }?;
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

#[cfg(test)]
mod tests {
    use crate::{
        allocator::DefaultAllocator,
        compiler::{
            hash_program_root, Compiler, CompilerMode, CompilerOptions, InstructionPointer,
        },
        core::{DependencyList, EvaluationResult, StateCache},
        interpreter::{execute, DefaultInterpreterCache, InterpreterOptions},
        lang::*,
        stdlib::Stdlib,
    };

    use super::*;

    #[test]
    fn compiled_let_expression_scoping() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();

        let expression = factory.create_let_term(
            factory.create_value_term(ValueTerm::Int(3)),
            factory.create_application_term(
                factory.create_builtin_term(Stdlib::Abs),
                allocator.create_unit_list(factory.create_let_term(
                    factory.create_value_term(ValueTerm::Null),
                    factory.create_static_variable_term(1),
                )),
            ),
        );
        let program = Compiler::new(CompilerOptions::unoptimized(), None)
            .compile(&expression, CompilerMode::Function, &factory, &allocator)
            .unwrap();
        let state = StateCache::default();
        let entry_point = InstructionPointer::default();
        let cache_key = hash_program_root(&program, &entry_point);
        let (result, _) = execute(
            cache_key,
            &program,
            InstructionPointer::default(),
            &state,
            &factory,
            &allocator,
            &InterpreterOptions::default(),
            &mut cache,
        )
        .unwrap();
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Int(3)),
                DependencyList::empty(),
            ),
        );

        let expression = factory.create_let_term(
            factory.create_value_term(ValueTerm::Int(3)),
            factory.create_application_term(
                factory.create_builtin_term(Stdlib::Add),
                allocator.create_pair(
                    factory.create_value_term(ValueTerm::Int(4)),
                    factory.create_let_term(
                        factory.create_value_term(ValueTerm::Null),
                        factory.create_static_variable_term(1),
                    ),
                ),
            ),
        );
        let program = Compiler::new(CompilerOptions::unoptimized(), None)
            .compile(&expression, CompilerMode::Function, &factory, &allocator)
            .unwrap();
        let state = StateCache::default();
        let entry_point = InstructionPointer::default();
        let cache_key = hash_program_root(&program, &entry_point);
        let (result, _) = execute(
            cache_key,
            &program,
            InstructionPointer::default(),
            &state,
            &factory,
            &allocator,
            &InterpreterOptions::default(),
            &mut cache,
        )
        .unwrap();
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Int(3 + 4)),
                DependencyList::empty(),
            ),
        );

        let expression = factory.create_let_term(
            factory.create_value_term(ValueTerm::Int(3)),
            factory.create_application_term(
                factory.create_builtin_term(Stdlib::If),
                allocator.create_triple(
                    factory.create_value_term(ValueTerm::Boolean(false)),
                    factory.create_value_term(ValueTerm::Null),
                    factory.create_application_term(
                        factory.create_builtin_term(Stdlib::Add),
                        allocator.create_pair(
                            factory.create_value_term(ValueTerm::Int(4)),
                            factory.create_let_term(
                                factory.create_value_term(ValueTerm::Null),
                                factory.create_static_variable_term(1),
                            ),
                        ),
                    ),
                ),
            ),
        );
        let program = Compiler::new(CompilerOptions::unoptimized(), None)
            .compile(&expression, CompilerMode::Function, &factory, &allocator)
            .unwrap();
        let state = StateCache::default();
        let entry_point = InstructionPointer::default();
        let cache_key = hash_program_root(&program, &entry_point);
        let (result, _) = execute(
            cache_key,
            &program,
            InstructionPointer::default(),
            &state,
            &factory,
            &allocator,
            &InterpreterOptions::default(),
            &mut cache,
        )
        .unwrap();
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Int(3 + 4)),
                DependencyList::empty(),
            ),
        );

        let expression = factory.create_application_term(
            factory.create_let_term(
                factory.create_lambda_term(1, factory.create_static_variable_term(0)),
                factory.create_lambda_term(
                    1,
                    factory.create_application_term(
                        factory.create_builtin_term(Stdlib::Abs),
                        allocator.create_unit_list(factory.create_application_term(
                            factory.create_static_variable_term(1),
                            allocator.create_unit_list(factory.create_static_variable_term(0)),
                        )),
                    ),
                ),
            ),
            allocator.create_unit_list(factory.create_value_term(ValueTerm::Int(3))),
        );
        let program = Compiler::new(CompilerOptions::unoptimized(), None)
            .compile(&expression, CompilerMode::Function, &factory, &allocator)
            .unwrap();
        let state = StateCache::default();
        let entry_point = InstructionPointer::default();
        let cache_key = hash_program_root(&program, &entry_point);
        let (result, _) = execute(
            cache_key,
            &program,
            InstructionPointer::default(),
            &state,
            &factory,
            &allocator,
            &InterpreterOptions::default(),
            &mut cache,
        )
        .unwrap();
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Int(3)),
                DependencyList::empty(),
            ),
        );
    }
}
