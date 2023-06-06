// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{collections::HashSet, iter::once};

use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;

use reflex::core::{
    apply_function, create_error_expression, get_combined_short_circuit_signal,
    transform_expression_list, validate_function_application_arity, Applicable,
    ApplicationTermType, ArgType, Arity, CompoundNode, DependencyList, DynamicState, Eagerness,
    Evaluate, EvaluationCache, EvaluationResult, Expression, ExpressionFactory, ExpressionListIter,
    ExpressionListType, GraphNode, HeapAllocator, Internable, LambdaTermType,
    PartialApplicationTermType, Reducible, RefType, Rewritable, ScopeOffset, SerializeJson,
    ShortCircuitCount, StackOffset, StateCache, Substitutions,
};

#[derive(Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct ApplicationTerm<T: Expression> {
    pub target: T,
    pub args: T::ExpressionList,
}

impl<T: Expression> std::hash::Hash for ApplicationTerm<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.target.id().hash(state);
        self.args.id().hash(state);
    }
}

impl<T: Expression> ApplicationTerm<T> {
    pub fn new(target: T, args: T::ExpressionList) -> Self {
        Self { target, args }
    }
}
impl<T: Expression> ApplicationTermType<T> for ApplicationTerm<T> {
    fn target<'a>(&'a self) -> T::ExpressionRef<'a>
    where
        T: 'a,
    {
        (&self.target).into()
    }
    fn args<'a>(&'a self) -> T::ExpressionListRef<'a>
    where
        T::ExpressionList: 'a,
        T: 'a,
    {
        (&self.args).into()
    }
}
impl<T: Expression + Applicable<T>> GraphNode for ApplicationTerm<T> {
    fn size(&self) -> usize {
        1 + self.target.size() + self.args.size()
    }
    fn capture_depth(&self) -> StackOffset {
        let target_depth = self.target().as_deref().capture_depth();
        let arg_depth = self.args().as_deref().capture_depth();
        target_depth.max(arg_depth)
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        let target_free_variables = self.target().as_deref().free_variables();
        let args_free_variables = self.args().as_deref().free_variables();
        if target_free_variables.is_empty() {
            args_free_variables
        } else if args_free_variables.is_empty() {
            target_free_variables
        } else {
            let mut combined = target_free_variables;
            combined.extend(args_free_variables);
            combined
        }
    }
    fn count_variable_usages(&self, offset: StackOffset) -> usize {
        self.target().as_deref().count_variable_usages(offset)
            + self.args().as_deref().count_variable_usages(offset)
    }
    fn dynamic_dependencies(&self, deep: bool) -> DependencyList {
        let target_dependencies = self.target().as_deref().dynamic_dependencies(deep);
        if deep {
            target_dependencies.union(self.args().as_deref().dynamic_dependencies(deep))
        } else {
            let eager_args = self
                .target
                .arity()
                .map(|arity| get_eager_args(self.args.iter(), &arity));
            match eager_args {
                None => target_dependencies,
                Some(args) => args.into_iter().fold(target_dependencies, |acc, arg| {
                    acc.union(arg.dynamic_dependencies(deep))
                }),
            }
        }
    }
    fn has_dynamic_dependencies(&self, deep: bool) -> bool {
        self.target().as_deref().has_dynamic_dependencies(deep)
            || (if deep {
                self.args().as_deref().has_dynamic_dependencies(deep)
            } else {
                let arity = self.target().as_deref().arity();
                match arity {
                    None => false,
                    Some(arity) => {
                        let args = self.args();
                        let mut eager_args = args.as_deref().iter().zip(arity.iter()).filter_map(
                            |(arg, arg_type)| match arg_type {
                                ArgType::Strict | ArgType::Eager => Some(arg),
                                _ => None,
                            },
                        );
                        eager_args.any(|arg| arg.has_dynamic_dependencies(deep))
                    }
                }
            })
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
impl<T: Expression> CompoundNode<T> for ApplicationTerm<T> {
    type Children<'a> = std::iter::Chain<std::iter::Once<T>, ExpressionListIter<'a, T>>
        where
            T: 'a,
            Self: 'a;
    fn children<'a>(&'a self) -> Self::Children<'a>
    where
        T: 'a,
    {
        once(self.target.clone()).chain(self.args.iter())
    }
}
impl<T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Evaluate<T>> Rewritable<T>
    for ApplicationTerm<T>
{
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
        let args = args.unwrap_or_else(|| allocator.clone_list((&self.args).into()));
        Some(factory.create_application_term(target, args))
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
        let args = args.unwrap_or_else(|| allocator.clone_list((&self.args).into()));
        Some(factory.create_application_term(target, args))
    }
    fn hoist_free_variables(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Option<T> {
        let hoisted_target = self
            .target()
            .as_deref()
            .hoist_free_variables(factory, allocator);
        let hoisted_args = transform_expression_list(&self.args, allocator, |arg| {
            arg.hoist_free_variables(factory, allocator)
        });
        if hoisted_target.is_none() && hoisted_args.is_none() {
            None
        } else {
            Some(factory.create_application_term(
                hoisted_target.unwrap_or_else(|| self.target.clone()),
                hoisted_args.unwrap_or_else(|| allocator.clone_list((&self.args).into())),
            ))
        }
    }
    fn normalize(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        let normalized_target = self
            .target()
            .as_deref()
            .normalize(factory, allocator, cache);
        let normalized_args = transform_expression_list(&self.args, allocator, |arg| {
            arg.normalize(factory, allocator, cache)
        });
        let target = self.target();
        let args = self.args();
        let target = normalized_target.as_ref().unwrap_or(&target.as_deref());
        let args = normalized_args.as_ref().unwrap_or(&args.as_deref());
        if let Some(partial) = factory.match_partial_application_term(target) {
            factory
                .create_application_term(
                    partial.target().as_deref().clone(),
                    allocator.create_sized_list(
                        partial.args().as_deref().len() + args.len(),
                        partial.args().as_deref().iter().chain(args.iter()),
                    ),
                )
                .normalize(factory, allocator, cache)
        } else {
            normalize_function_application(target, args, factory, allocator, cache).or_else(|| {
                if normalized_target.is_some() || normalized_args.is_some() {
                    Some(factory.create_application_term(
                        normalized_target.unwrap_or_else(|| self.target().as_deref().clone()),
                        normalized_args.unwrap_or_else(|| self.args().as_deref().clone()),
                    ))
                } else {
                    None
                }
            })
        }
    }
}
impl<T: Expression + Reducible<T> + Applicable<T> + Evaluate<T>> Reducible<T>
    for ApplicationTerm<T>
{
    fn is_reducible(&self) -> bool {
        true
    }
    fn reduce(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        let state = Option::<&StateCache<T>>::None;
        evaluate_function_application(self, state, factory, allocator, cache).map(|result| {
            let (result, _) = result.into_parts();
            result
        })
    }
}
impl<T: Expression + Reducible<T> + Applicable<T> + Evaluate<T>> Evaluate<T>
    for ApplicationTerm<T>
{
    fn evaluate(
        &self,
        state: &impl DynamicState<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<EvaluationResult<T>> {
        evaluate_function_application(self, Some(state), factory, allocator, cache)
    }
}

impl<T: Expression> Internable for ApplicationTerm<T> {
    fn should_intern(&self, eager: Eagerness) -> bool {
        eager == Eagerness::Lazy
            && self.target.capture_depth() == 0
            && self.args.capture_depth() == 0
    }
}

impl<T: Expression> std::fmt::Display for ApplicationTerm<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "<apply:{}:{}>",
            self.target().as_deref(),
            self.args()
                .as_deref()
                .iter()
                .map(|arg| format!("{}", arg))
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}

impl<T: Expression> SerializeJson for ApplicationTerm<T> {
    fn to_json(&self) -> Result<JsonValue, String> {
        Err(format!("Unable to serialize term: {}", self))
    }

    fn patch(&self, target: &Self) -> Result<Option<JsonValue>, String> {
        Err(format!("Unable to patch terms: {}, {}", self, target))
    }
}

fn normalize_function_application<
    T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Evaluate<T>,
>(
    target: &T,
    args: &T::ExpressionList,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    cache: &mut impl EvaluationCache<T>,
) -> Option<T> {
    if let Some(_) = factory.match_compiled_function_term(target) {
        None
    } else if let Some(target) = factory.match_lambda_term(target) {
        normalize_lambda_application(target, args, factory, allocator, cache)
    } else if let Some(arity) = target.arity() {
        normalize_builtin_application(target, &arity, args, factory, allocator, cache)
    } else {
        None
    }
}

fn normalize_lambda_application<T: Expression + Rewritable<T>>(
    target: &T::LambdaTerm,
    args: &T::ExpressionList,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    cache: &mut impl EvaluationCache<T>,
) -> Option<T> {
    if args.len() == 0 {
        Some(target.body().as_deref().clone())
    } else {
        let num_args = target.num_args();
        let (inlined_args, remaining_args): (Vec<_>, Vec<_>) = args
            .iter()
            .take(num_args)
            .enumerate()
            .map(|(index, arg)| (num_args - index - 1, arg))
            .partition(|(offset, arg)| {
                !arg.is_complex() || target.body().as_deref().count_variable_usages(*offset) <= 1
            });
        if remaining_args.is_empty() {
            Some(
                target
                    .body()
                    .as_deref()
                    .substitute_static(
                        &Substitutions::named(&inlined_args, Some(ScopeOffset::Unwrap(num_args))),
                        factory,
                        allocator,
                        cache,
                    )
                    .map(|result| {
                        result
                            .normalize(factory, allocator, cache)
                            .unwrap_or(result)
                    })
                    .unwrap_or_else(|| target.body().as_deref().clone()),
            )
        } else if !inlined_args.is_empty() {
            Some(factory.create_application_term(
                factory.create_lambda_term(
                    remaining_args.len(),
                    inlined_args.into_iter().fold(
                        target.body().as_deref().clone(),
                        |body, (offset, arg)| {
                            let arg = arg
                                .substitute_static(
                                    &Substitutions::increase_scope_offset(remaining_args.len(), 0),
                                    factory,
                                    allocator,
                                    cache,
                                )
                                .unwrap_or(arg);
                            body.substitute_static(
                                &Substitutions::named(
                                    &vec![(offset, arg)],
                                    Some(ScopeOffset::Unwrap(1)),
                                ),
                                factory,
                                allocator,
                                cache,
                            )
                            .unwrap_or(body)
                        },
                    ),
                ),
                allocator.create_list(remaining_args.into_iter().map(|(_, arg)| arg)),
            ))
        } else {
            None
        }
    }
}

fn normalize_builtin_application<
    T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Evaluate<T>,
>(
    target: &T,
    arity: &Arity,
    args: &T::ExpressionList,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    cache: &mut impl EvaluationCache<T>,
) -> Option<T> {
    let state = Option::<&StateCache<T>>::None;
    match evaluate_function_args(target, arity, args, state, factory, allocator, cache) {
        (FunctionArgs::ShortCircuit(signal), _) => Some(signal),
        (FunctionArgs::Unresolved(_), _) => None,
        (FunctionArgs::Resolved(resolved_args), _) => {
            if args.capture_depth() > 0 {
                None
            } else {
                let result = apply_function(target, resolved_args, factory, allocator, cache);
                Some(
                    result
                        .normalize(factory, allocator, cache)
                        .unwrap_or(result),
                )
            }
        }
    }
}

fn get_eager_args<T: Expression>(
    args: impl IntoIterator<Item = T>,
    arity: &Arity,
) -> impl Iterator<Item = T> {
    arity
        .iter()
        .zip(args)
        .filter_map(|(arg_type, arg)| match arg_type {
            ArgType::Strict | ArgType::Eager => Some(arg),
            ArgType::Lazy => None,
        })
}

fn evaluate_function_application<
    T: Expression + Reducible<T> + Applicable<T> + Evaluate<T>,
    TState: DynamicState<T>,
>(
    expression: &ApplicationTerm<T>,
    state: Option<&TState>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    cache: &mut impl EvaluationCache<T>,
) -> Option<EvaluationResult<T>> {
    let reduced_target = resolve_eager_value(&expression.target, state, factory, allocator, cache);
    let (reduced_target, target_dependencies) = reduced_target
        .map(|target| {
            let (result, dependencies) = target.into_parts();
            (Some(result), dependencies)
        })
        .unwrap_or((None, DependencyList::empty()));
    let target = reduced_target.as_ref().unwrap_or(&expression.target);
    let short_circuit_target = match factory.match_signal_term(target) {
        Some(_) => true,
        _ => false,
    };
    if short_circuit_target {
        return reduced_target.map(|target| EvaluationResult::new(target, target_dependencies));
    }
    match target.arity() {
        None => None,
        Some(arity) => {
            let (args, arg_dependencies) = evaluate_function_args(
                target,
                &arity,
                &expression.args,
                state,
                factory,
                allocator,
                cache,
            );
            let args_result = match args {
                FunctionArgs::ShortCircuit(signal) => Ok(signal),
                FunctionArgs::Unresolved(args) => Err(args),
                FunctionArgs::Resolved(args) => {
                    if is_compiled_application_target(target, factory) {
                        Err(args)
                    } else {
                        Ok(apply_function(target, args, factory, allocator, cache))
                    }
                }
            };
            let combined_dependencies = target_dependencies.union(arg_dependencies);
            match args_result {
                Ok(result) => Some(
                    match resolve_eager_value(&result, state, factory, allocator, cache) {
                        Some(result) => result.with_dependencies(combined_dependencies),
                        None => EvaluationResult::new(result, combined_dependencies),
                    },
                ),
                Err(reduced_args) => {
                    if reduced_target.is_none()
                        && expression
                            .args
                            .iter()
                            .zip(reduced_args.iter())
                            .all(|(arg, evaluated_arg)| arg.id() == evaluated_arg.id())
                    {
                        None
                    } else {
                        let target = reduced_target.unwrap_or_else(|| expression.target.clone());
                        Some(EvaluationResult::new(
                            factory.create_application_term(
                                target,
                                allocator.create_list(reduced_args),
                            ),
                            combined_dependencies,
                        ))
                    }
                }
            }
        }
    }
}

fn resolve_eager_value<T: Expression + Reducible<T> + Evaluate<T>, TState: DynamicState<T>>(
    expression: &T,
    state: Option<&TState>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    cache: &mut impl EvaluationCache<T>,
) -> Option<EvaluationResult<T>> {
    if let Some(state) = state {
        expression.evaluate(state, factory, allocator, cache)
    } else {
        expression
            .reduce(factory, allocator, cache)
            .map(|result| EvaluationResult::new(result, DependencyList::empty()))
    }
}

enum FunctionArgs<T: Expression> {
    ShortCircuit(T),
    Unresolved(Vec<T>),
    Resolved(Vec<T>),
}
fn evaluate_function_args<T: Expression + Reducible<T> + Evaluate<T>, TState: DynamicState<T>>(
    target: &T,
    arity: &Arity,
    args: &T::ExpressionList,
    state: Option<&TState>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    cache: &mut impl EvaluationCache<T>,
) -> (FunctionArgs<T>, DependencyList) {
    match validate_function_application_arity(target, arity, args.len()) {
        Err(message) => (
            FunctionArgs::ShortCircuit(create_error_expression(
                factory.create_string_term(allocator.create_string(message)),
                factory,
                allocator,
            )),
            DependencyList::empty(),
        ),
        Ok(num_args) => {
            let mut has_unresolved_args = false;
            let mut num_short_circuit_signals = ShortCircuitCount::None;
            let (mut resolved_args, dependencies) = args
                .iter()
                .take(num_args)
                .zip(arity.iter())
                .map(|(arg, arg_type)| match arg_type {
                    ArgType::Strict | ArgType::Eager => {
                        let resolved_arg =
                            resolve_eager_value(&arg, state, factory, allocator, cache)
                                .unwrap_or_else(|| {
                                    EvaluationResult::new(arg, DependencyList::empty())
                                });
                        let value = resolved_arg.result();
                        if is_unresolved_arg(value) {
                            has_unresolved_args = true;
                        }
                        if factory.match_signal_term(value).is_some() {
                            if let ArgType::Strict = arg_type {
                                num_short_circuit_signals.increment();
                            }
                        }
                        resolved_arg
                    }
                    ArgType::Lazy => EvaluationResult::new(arg, DependencyList::empty()),
                })
                .fold(
                    (Vec::<T>::with_capacity(num_args), DependencyList::empty()),
                    |(mut args, dependencies), result| {
                        let (arg_value, arg_dependencies) = result.into_parts();
                        args.push(arg_value);
                        (args, dependencies.union(arg_dependencies))
                    },
                );
            if has_unresolved_args {
                (FunctionArgs::Unresolved(resolved_args), dependencies)
            } else if let Some(signal) = get_combined_short_circuit_signal(
                num_short_circuit_signals,
                resolved_args.iter().cloned(),
                arity,
                factory,
                allocator,
            ) {
                (FunctionArgs::ShortCircuit(signal), dependencies)
            } else {
                let num_provided_args = resolved_args.len();
                let num_required_args = arity.required().len();
                let num_optional_args = arity.optional().len();
                let num_positional_args = num_required_args + num_optional_args;
                let num_unspecified_optional_args =
                    num_positional_args.saturating_sub(num_provided_args);
                if num_unspecified_optional_args > 0 {
                    resolved_args.extend(
                        (0..num_unspecified_optional_args).map(|_| factory.create_nil_term()),
                    )
                }
                (FunctionArgs::Resolved(resolved_args), dependencies)
            }
        }
    }
}

fn is_unresolved_arg<T: Expression>(arg: &T) -> bool {
    arg.capture_depth() > 0 || arg.has_dynamic_dependencies(false) || !arg.is_static()
}

fn is_compiled_application_target<T: Expression>(
    target: &T,
    factory: &impl ExpressionFactory<T>,
) -> bool {
    if let Some(_) = factory.match_compiled_function_term(target) {
        true
    } else if let Some(target) = factory.match_partial_application_term(target) {
        is_compiled_application_target(target.target().as_deref(), factory)
    } else {
        false
    }
}

#[cfg(test)]
mod tests {

    use crate::{allocator::DefaultAllocator, CachedSharedTerm, SharedTermFactory};
    use reflex::{
        cache::SubstitutionCache,
        core::{ExpressionFactory, HeapAllocator, Rewritable},
    };
    use reflex_stdlib::{Add, Get, Stdlib, Subtract};

    #[test]
    fn normalize_zero_arg_applications() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
        let expression = factory.create_application_term(
            factory.create_lambda_term(0, factory.create_int_term(3)),
            allocator.create_empty_list(),
        );
        let result = expression.normalize(&factory, &allocator, &mut SubstitutionCache::new());
        assert_eq!(result, Some(factory.create_int_term(3)));
    }

    #[test]
    fn normalize_fully_specified_applications() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
        let expression = factory.create_application_term(
            factory.create_lambda_term(
                3,
                factory.create_application_term(
                    factory.create_builtin_term(Subtract),
                    allocator.create_pair(
                        factory.create_variable_term(2),
                        factory.create_application_term(
                            factory.create_builtin_term(Add),
                            allocator.create_pair(
                                factory.create_variable_term(1),
                                factory.create_variable_term(0),
                            ),
                        ),
                    ),
                ),
            ),
            allocator.create_triple(
                factory.create_int_term(3),
                factory.create_int_term(4),
                factory.create_int_term(5),
            ),
        );
        let result = expression.normalize(&factory, &allocator, &mut SubstitutionCache::new());
        assert_eq!(result, Some(factory.create_int_term(3 - (4 + 5))));
    }

    #[test]
    fn normalize_partially_specified_applications() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
        let expression = factory.create_application_term(
            factory.create_lambda_term(
                3,
                factory.create_application_term(
                    factory.create_builtin_term(Subtract),
                    allocator.create_pair(
                        factory.create_variable_term(2),
                        factory.create_application_term(
                            factory.create_builtin_term(Add),
                            allocator.create_pair(
                                factory.create_variable_term(1),
                                factory.create_variable_term(0),
                            ),
                        ),
                    ),
                ),
            ),
            allocator.create_triple(
                factory.create_variable_term(123),
                factory.create_int_term(4),
                factory.create_int_term(5),
            ),
        );
        let result = expression.normalize(&factory, &allocator, &mut SubstitutionCache::new());
        assert_eq!(
            result,
            Some(factory.create_application_term(
                factory.create_builtin_term(Subtract),
                allocator.create_pair(
                    factory.create_variable_term(123),
                    factory.create_int_term(4 + 5),
                ),
            ))
        );
    }

    #[test]
    fn normalize_complex_application_args() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
        let expression = factory.create_application_term(
            factory.create_lambda_term(
                3,
                factory.create_application_term(
                    factory.create_builtin_term(Add),
                    allocator.create_pair(
                        factory.create_application_term(
                            factory.create_builtin_term(Get),
                            allocator.create_pair(
                                factory.create_variable_term(2),
                                factory.create_variable_term(1),
                            ),
                        ),
                        factory.create_application_term(
                            factory.create_builtin_term(Get),
                            allocator.create_pair(
                                factory.create_variable_term(2),
                                factory.create_variable_term(0),
                            ),
                        ),
                    ),
                ),
            ),
            allocator.create_triple(
                factory.create_list_term(
                    allocator.create_pair(factory.create_int_term(3), factory.create_int_term(4)),
                ),
                factory.create_int_term(0),
                factory.create_variable_term(0),
            ),
        );
        let result = expression.normalize(&factory, &allocator, &mut SubstitutionCache::new());
        assert_eq!(
            result,
            Some(factory.create_application_term(
                factory.create_lambda_term(
                    1,
                    factory.create_application_term(
                        factory.create_builtin_term(Add),
                        allocator.create_pair(
                            factory.create_application_term(
                                factory.create_builtin_term(Get),
                                allocator.create_pair(
                                    factory.create_variable_term(0),
                                    factory.create_int_term(0),
                                ),
                            ),
                            factory.create_application_term(
                                factory.create_builtin_term(Get),
                                allocator.create_pair(
                                    factory.create_variable_term(0),
                                    factory.create_variable_term(1),
                                ),
                            ),
                        ),
                    ),
                ),
                allocator.create_unit_list(factory.create_list_term(
                    allocator.create_pair(factory.create_int_term(3), factory.create_int_term(4),)
                ),),
            ))
        );
    }

    #[test]
    fn normalize_builtin_application_args() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
        let expression =
            factory.create_application_term(
                factory.create_builtin_term(Get),
                allocator.create_pair(
                    factory.create_list_term(allocator.create_unit_list(
                        factory.create_application_term(
                            factory.create_builtin_term(Add),
                            allocator.create_pair(
                                factory.create_int_term(3),
                                factory.create_int_term(4),
                            ),
                        ),
                    )),
                    factory.create_int_term(0),
                ),
            );
        let result = expression.normalize(&factory, &allocator, &mut SubstitutionCache::new());
        assert_eq!(result, Some(factory.create_int_term(3 + 4)),);
    }
}
