// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{collections::HashSet, iter::once};

use crate::{
    compiler::{Compile, Compiler, Instruction, NativeFunctionRegistry, Program},
    core::{
        transform_expression_list, Applicable, Arity, DependencyList, DynamicState,
        EvaluationCache, Expression, ExpressionFactory, ExpressionList, GraphNode, HeapAllocator,
        Reducible, Rewritable, StackOffset, Substitutions, VarArgs,
    },
};

use super::application::{compile_args, with_eagerness};

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct PartialApplicationTerm<T: Expression> {
    target: T,
    args: ExpressionList<T>,
}
impl<T: Expression> PartialApplicationTerm<T> {
    pub fn new(target: T, args: ExpressionList<T>) -> Self {
        Self { target, args }
    }
    pub fn target(&self) -> &T {
        &self.target
    }
    pub fn args(&self) -> &ExpressionList<T> {
        &self.args
    }
}
impl<T: Expression + Applicable<T>> GraphNode for PartialApplicationTerm<T> {
    fn capture_depth(&self) -> StackOffset {
        let target_depth = self.target.capture_depth();
        let arg_depth = self.args.capture_depth();
        target_depth.max(arg_depth)
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        let mut target_free_variables = self.target.free_variables();
        let args_free_variables = self.args.free_variables();
        if target_free_variables.is_empty() {
            args_free_variables
        } else if args_free_variables.is_empty() {
            target_free_variables
        } else {
            target_free_variables.extend(args_free_variables);
            target_free_variables
        }
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        let target_dependencies = self.target.dynamic_dependencies();
        let eager_args = self.target.arity().map(|arity| {
            with_eagerness(self.args.iter(), &arity)
                .into_iter()
                .filter_map(|(arg, eager)| match eager {
                    VarArgs::Eager => Some(arg),
                    VarArgs::Lazy => None,
                })
        });
        match eager_args {
            None => target_dependencies,
            Some(args) => args.fold(target_dependencies, |acc, arg| {
                acc.union(arg.dynamic_dependencies())
            }),
        }
    }
    fn is_static(&self) -> bool {
        self.target.is_static()
    }
    fn is_atomic(&self) -> bool {
        false
    }
}
impl<T: Expression + Rewritable<T> + Reducible<T> + Applicable<T>> Rewritable<T>
    for PartialApplicationTerm<T>
{
    fn subexpressions(&self) -> Vec<&T> {
        once(&self.target)
            .chain(self.target.subexpressions())
            .into_iter()
            .chain(
                self.args
                    .iter()
                    .flat_map(|arg| once(arg).chain(arg.subexpressions())),
            )
            .collect()
    }
    fn substitute_static(
        &self,
        substitutions: &Substitutions<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        let target = self
            .target
            .substitute_static(substitutions, factory, allocator, cache);
        let args = transform_expression_list(&self.args, allocator, |arg| {
            arg.substitute_static(substitutions, factory, allocator, cache)
        });
        if target.is_none() && args.is_none() {
            return None;
        }
        let target = target.unwrap_or_else(|| self.target.clone());
        let args = args.unwrap_or_else(|| allocator.clone_list(&self.args));
        Some(factory.create_partial_application_term(target, args))
    }
    fn substitute_dynamic(
        &self,
        state: &impl DynamicState<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        let target = self
            .target
            .substitute_dynamic(state, factory, allocator, cache);
        let has_dynamic_args = self
            .args
            .iter()
            .any(|arg| !arg.dynamic_dependencies().is_empty());
        let args = if has_dynamic_args {
            self.target.arity().map(|arity| {
                allocator.create_list(with_eagerness(self.args.iter(), &arity).into_iter().map(
                    |(arg, eager)| {
                        match eager {
                            VarArgs::Eager => arg
                                .substitute_dynamic(state, factory, allocator, cache)
                                .unwrap_or(arg.clone()),
                            VarArgs::Lazy => arg.clone(),
                        }
                    },
                ))
            })
        } else {
            None
        };
        if target.is_none() && args.is_none() {
            return None;
        }
        let target = target.unwrap_or_else(|| self.target.clone());
        let args = args.unwrap_or_else(|| allocator.clone_list(&self.args));
        Some(factory.create_partial_application_term(target, args))
    }
    fn hoist_free_variables(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Option<T> {
        let hoisted_target = self.target.hoist_free_variables(factory, allocator);
        let hoisted_args = transform_expression_list(&self.args, allocator, |arg| {
            arg.hoist_free_variables(factory, allocator)
        });
        if hoisted_target.is_none() && hoisted_args.is_none() {
            None
        } else {
            Some(factory.create_partial_application_term(
                hoisted_target.unwrap_or_else(|| self.target.clone()),
                hoisted_args.unwrap_or_else(|| allocator.clone_list(&self.args)),
            ))
        }
    }
    fn normalize(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        let normalized_target = self.target.normalize(factory, allocator, cache);
        let normalized_args = transform_expression_list(&self.args, allocator, |arg| {
            arg.normalize(factory, allocator, cache)
        });
        if normalized_target.is_none() && normalized_args.is_none() {
            None
        } else {
            Some(factory.create_partial_application_term(
                normalized_target.unwrap_or_else(|| self.target.clone()),
                normalized_args.unwrap_or_else(|| allocator.clone_list(&self.args)),
            ))
        }
    }
}
impl<T: Expression + Rewritable<T> + Applicable<T>> Applicable<T> for PartialApplicationTerm<T> {
    fn arity(&self) -> Option<Arity> {
        self.target.arity().map(|arity| {
            let partial_args = self.args.len();
            let remaining_eager_args = arity.eager().saturating_sub(partial_args);
            let partial_args = partial_args.saturating_sub(arity.eager());
            let remaining_lazy_args = arity.lazy().saturating_sub(partial_args);
            Arity::from(remaining_eager_args, remaining_lazy_args, arity.variadic())
        })
    }
    fn apply(
        &self,
        args: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        self.target.apply(
            self.args
                .iter()
                .cloned()
                .chain(args.into_iter())
                .collect::<Vec<_>>(),
            factory,
            allocator,
            cache,
        )
    }
}
impl<T: Expression + Applicable<T> + Rewritable<T> + Reducible<T> + Compile<T>> Compile<T>
    for PartialApplicationTerm<T>
{
    fn compile(
        &self,
        eager: VarArgs,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<(Program, NativeFunctionRegistry<T>), String> {
        let target = &self.target;
        let args = &self.args;
        let num_args = args.len();
        let eager_arity = match eager {
            VarArgs::Lazy => None,
            VarArgs::Eager => target.arity(),
            // VarArgs::Eager => {
            //     let precompiled_target_instruction = factory
            //         .match_compiled_function_term(target)
            //         .and_then(|target| {
            //             compiler.retrieve_compiled_instruction(target.address(), None)
            //         });
            //     match precompiled_target_instruction {
            //         Some(Instruction::Function { arity, .. }) => Some(Arity::from(0, *arity, None)),
            //         _ => target.arity(),
            //     }
            // }
        };
        let (compiled_target, target_native_functions) =
            target.compile(eager, stack_offset + num_args, factory, allocator, compiler)?;
        let (compiled_args, arg_native_functions) = match eager_arity {
            Some(arity) => compile_args(
                with_eagerness(args.iter(), &arity),
                stack_offset,
                factory,
                allocator,
                compiler,
            ),
            None => compile_args(
                args.iter().map(|arg| (arg, VarArgs::Lazy)),
                stack_offset,
                factory,
                allocator,
                compiler,
            ),
        }?;
        let mut combined_native_functions = target_native_functions;
        combined_native_functions.extend(arg_native_functions);
        let mut result = compiled_args;
        result.extend(compiled_target);
        result.push(Instruction::ConstructPartialApplication { num_args });
        Ok((result, combined_native_functions))
    }
}
impl<T: Expression> std::fmt::Display for PartialApplicationTerm<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "<partial:{}:{}>",
            self.target,
            self.args
                .iter()
                .map(|arg| format!("{}", arg))
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}
