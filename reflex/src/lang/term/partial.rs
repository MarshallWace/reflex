// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use serde::{Deserialize, Serialize};
use std::{collections::HashSet, iter::once};

use crate::{
    compiler::{Compile, Compiler, Instruction, Program},
    core::{
        transform_expression_list, Applicable, ArgType, Arity, DependencyList, DynamicState,
        EvaluationCache, Expression, ExpressionFactory, ExpressionList, GraphNode, HeapAllocator,
        Reducible, Rewritable, SerializeJson, StackOffset, Substitutions, VarArgs,
    },
};

use super::application::compile_args;

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
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
    fn dynamic_dependencies(&self, deep: bool) -> DependencyList {
        let target_dependencies = self.target.dynamic_dependencies(deep);
        if deep {
            target_dependencies.union(self.args.dynamic_dependencies(deep))
        } else {
            let eager_args = self.target.arity().map(|arity| {
                self.args
                    .iter()
                    .zip(arity.iter())
                    .filter_map(|(arg, arg_type)| match arg_type {
                        ArgType::Strict | ArgType::Eager => Some(arg),
                        _ => None,
                    })
            });
            match eager_args {
                None => target_dependencies,
                Some(args) => args.fold(target_dependencies, |acc, arg| {
                    acc.union(arg.dynamic_dependencies(deep))
                }),
            }
        }
    }
    fn has_dynamic_dependencies(&self, deep: bool) -> bool {
        self.target.has_dynamic_dependencies(deep)
            || (if deep {
                self.args.has_dynamic_dependencies(deep)
            } else {
                let eager_args = self.target.arity().map(|arity| {
                    self.args.iter().zip(arity.iter()).filter_map(
                        |(arg, arg_type)| match arg_type {
                            ArgType::Strict | ArgType::Eager => Some(arg),
                            _ => None,
                        },
                    )
                });
                match eager_args {
                    None => false,
                    Some(mut args) => args.any(|arg| arg.has_dynamic_dependencies(deep)),
                }
            })
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
    fn children(&self) -> Vec<&T> {
        once(&self.target).chain(self.args.iter()).collect()
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
        deep: bool,
        state: &impl DynamicState<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        let target = self
            .target
            .substitute_dynamic(deep, state, factory, allocator, cache);
        let args = transform_expression_list(&self.args, allocator, |arg| {
            arg.substitute_dynamic(deep, state, factory, allocator, cache)
        });
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
        if self.args.is_empty() {
            return normalized_target.or_else(|| Some(self.target.clone()));
        }
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
        match self.target.arity() {
            None => None,
            Some(arity) => Some(arity.partial(self.args.len())),
        }
    }
    fn apply(
        &self,
        args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        self.target.apply(
            self.args
                .iter()
                .cloned()
                .chain(args.into_iter())
                .collect::<Vec<_>>() // Required to prevent infinite type recursion
                .into_iter(),
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
    ) -> Result<Program, String> {
        let target = &self.target;
        let args = &self.args;
        let num_args = args.len();
        let arity = match eager {
            VarArgs::Eager => target.arity(),
            VarArgs::Lazy => None,
        }
        .unwrap_or_else(|| Arity::lazy(num_args, 0, false));
        let compiled_target =
            target.compile(eager, stack_offset + num_args, factory, allocator, compiler)?;
        let compiled_args = compile_args(
            args.iter().zip(arity.iter()),
            stack_offset,
            factory,
            allocator,
            compiler,
        )?;
        let mut result = compiled_args;
        result.extend(compiled_target);
        result.push(Instruction::ConstructPartialApplication { num_args });
        Ok(result)
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

impl<T: Expression> SerializeJson for PartialApplicationTerm<T> {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        Err(format!("Unable to serialize term: {}", self))
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        allocator::DefaultAllocator,
        cache::SubstitutionCache,
        core::{ExpressionFactory, HeapAllocator, Rewritable},
        lang::{CachedSharedTerm, SharedTermFactory, ValueTerm},
        stdlib::Stdlib,
    };

    #[test]
    fn normalize_fully_applied_partial() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();

        let expression = factory.create_partial_application_term(
            factory.create_lambda_term(0, factory.create_value_term(ValueTerm::Int(3))),
            allocator.create_empty_list(),
        );
        let result = expression.normalize(&factory, &allocator, &mut SubstitutionCache::new());
        assert_eq!(
            result,
            Some(factory.create_lambda_term(0, factory.create_value_term(ValueTerm::Int(3))))
        );

        let expression = factory.create_partial_application_term(
            factory.create_lambda_term(1, factory.create_static_variable_term(0)),
            allocator.create_empty_list(),
        );
        let result = expression.normalize(&factory, &allocator, &mut SubstitutionCache::new());
        assert_eq!(
            result,
            Some(factory.create_lambda_term(1, factory.create_static_variable_term(0)))
        );

        let expression = factory.create_partial_application_term(
            factory.create_lambda_term(1, factory.create_static_variable_term(0)),
            allocator.create_unit_list(factory.create_static_variable_term(0)),
        );
        let result = expression.normalize(&factory, &allocator, &mut SubstitutionCache::new());
        assert_eq!(result, None);

        let expression = factory.create_application_term(
            factory.create_partial_application_term(
                factory.create_lambda_term(1, factory.create_static_variable_term(0)),
                allocator.create_unit_list(factory.create_value_term(ValueTerm::Int(3))),
            ),
            allocator.create_empty_list(),
        );
        let result = expression.normalize(&factory, &allocator, &mut SubstitutionCache::new());
        assert_eq!(result, Some(factory.create_value_term(ValueTerm::Int(3))));

        let expression = factory.create_application_term(
            factory.create_partial_application_term(
                factory.create_lambda_term(0, factory.create_value_term(ValueTerm::Int(3))),
                allocator.create_empty_list(),
            ),
            allocator.create_empty_list(),
        );
        let result = expression.normalize(&factory, &allocator, &mut SubstitutionCache::new());
        assert_eq!(result, Some(factory.create_value_term(ValueTerm::Int(3))));

        let expression = factory.create_application_term(
            factory.create_partial_application_term(
                factory.create_lambda_term(1, factory.create_static_variable_term(0)),
                allocator.create_empty_list(),
            ),
            allocator.create_unit_list(factory.create_value_term(ValueTerm::Int(3))),
        );
        let result = expression.normalize(&factory, &allocator, &mut SubstitutionCache::new());
        assert_eq!(result, Some(factory.create_value_term(ValueTerm::Int(3))));
    }
}
