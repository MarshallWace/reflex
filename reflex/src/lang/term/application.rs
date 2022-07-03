// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use serde::{Deserialize, Serialize};
use std::{
    collections::HashSet,
    iter::{empty, once},
};

use crate::{
    compiler::{Compile, Compiler, Instruction, InstructionPointer, Program},
    core::{
        transform_expression_list, Applicable, ArgType, Arity, CompoundNode, DependencyList,
        DynamicState, Evaluate, EvaluationCache, EvaluationResult, Expression, ExpressionFactory,
        ExpressionList, ExpressionListSlice, GraphNode, HeapAllocator, Reducible, Rewritable,
        ScopeOffset, SerializeJson, SignalType, StackOffset, StateCache, Substitutions, VarArgs,
    },
    hash::HashId,
    lang::SignalTerm,
};

use super::LambdaTerm;

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct ApplicationTerm<T: Expression> {
    target: T,
    args: ExpressionList<T>,
}
impl<T: Expression> ApplicationTerm<T> {
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
impl<T: Expression + Applicable<T>> GraphNode for ApplicationTerm<T> {
    fn capture_depth(&self) -> StackOffset {
        let target_depth = self.target.capture_depth();
        let arg_depth = self.args.capture_depth();
        target_depth.max(arg_depth)
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        let target_free_variables = self.target.free_variables();
        let args_free_variables = self.args.free_variables();
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
        self.target.count_variable_usages(offset) + self.args.count_variable_usages(offset)
    }
    fn dynamic_dependencies(&self, deep: bool) -> DependencyList {
        let target_dependencies = self.target.dynamic_dependencies(deep);
        if deep {
            target_dependencies.union(self.args.dynamic_dependencies(deep))
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
        false
    }
    fn is_atomic(&self) -> bool {
        false
    }
    fn is_complex(&self) -> bool {
        true
    }
}
pub type ApplicationTermChildren<'a, T> =
    std::iter::Chain<std::iter::Once<&'a T>, ExpressionListSlice<'a, T>>;
impl<'a, T: Expression + 'a> CompoundNode<'a, T> for ApplicationTerm<T> {
    type Children = ApplicationTermChildren<'a, T>;
    fn children(&'a self) -> Self::Children {
        once(&self.target).chain(self.args.iter())
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
        let args = args.unwrap_or_else(|| allocator.clone_list(&self.args));
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
        let args = args.unwrap_or_else(|| allocator.clone_list(&self.args));
        Some(factory.create_application_term(target, args))
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
            Some(factory.create_application_term(
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
        let target = normalized_target.as_ref().unwrap_or(&self.target);
        let args = normalized_args.as_ref().unwrap_or(&self.args);
        if let Some(partial) = factory.match_partial_application_term(target) {
            factory
                .create_application_term(
                    partial.target().clone(),
                    allocator.create_sized_list(
                        partial.args().len() + args.len(),
                        partial.args().iter().chain(args).cloned(),
                    ),
                )
                .normalize(factory, allocator, cache)
        } else {
            normalize_function_application(target, args, factory, allocator, cache).or_else(|| {
                if normalized_target.is_some() || normalized_args.is_some() {
                    Some(factory.create_application_term(
                        normalized_target.unwrap_or_else(|| self.target.clone()),
                        normalized_args.unwrap_or_else(|| self.args.clone()),
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
impl<T: Expression + Applicable<T> + Compile<T>> Compile<T> for ApplicationTerm<T> {
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
            args.iter().map(|arg| (arg, ArgType::Lazy)),
            stack_offset,
            factory,
            allocator,
            compiler,
        )?;
        match eager {
            VarArgs::Lazy => {
                let mut result = compiled_args;
                result.extend(compiled_target);
                result.push(Instruction::ConstructApplication { num_args });
                Ok(result)
            }
            VarArgs::Eager => {
                if num_args < arity.required().len() {
                    Err(format!(
                        "{}: expected {} {}, received {}",
                        target,
                        arity.required().len(),
                        if arity.optional().len() > 0 || arity.variadic().is_some() {
                            "or more arguments"
                        } else if arity.required().len() != 1 {
                            "arguments"
                        } else {
                            "argument"
                        },
                        num_args,
                    ))
                } else {
                    if let Some((target_address, target_hash)) =
                        match_compiled_function_result(&compiled_target)
                    {
                        let mut result = compiled_args;
                        // TODO: jump to target if in tail position
                        result.push(Instruction::Call {
                            target_address,
                            target_hash,
                            num_args,
                        });
                        Ok(result)
                    } else {
                        let mut result = compiled_args;
                        result.extend(compiled_target);
                        if !target.is_static() {
                            result.push(Instruction::Evaluate);
                        }
                        result.push(Instruction::Apply { num_args });
                        Ok(result)
                    }
                }
            }
        }
    }
}
impl<T: Expression> std::fmt::Display for ApplicationTerm<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "<apply:{}:{}>",
            self.target,
            self.args
                .iter()
                .map(|arg| format!("{}", arg))
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}
impl<T: Expression> SerializeJson for ApplicationTerm<T> {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        Err(format!("Unable to serialize term: {}", self))
    }
}

pub fn validate_function_application_arity<T: Expression>(
    target: &T,
    arity: &Arity,
    num_args: usize,
) -> Result<usize, String> {
    let num_required_args = arity.required().len();
    let num_optional_args = arity.optional().len();
    if num_args < num_required_args {
        Err(format!(
            "{}: Expected {} {}, received {}",
            target,
            num_required_args,
            if num_optional_args > 0 || arity.variadic().is_some() {
                "or more arguments"
            } else if num_required_args != 1 {
                "arguments"
            } else {
                "argument"
            },
            num_args,
        ))
    } else {
        let num_args = match arity.variadic() {
            Some(_) => num_args,
            None => num_args.min(num_required_args + num_optional_args),
        };
        Ok(num_args)
    }
}

fn apply_function<T: Expression + Applicable<T>>(
    target: &T,
    args: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    cache: &mut impl EvaluationCache<T>,
) -> T {
    target
        .apply(args.into_iter(), factory, allocator, cache)
        .unwrap_or_else(|error| {
            create_error_signal_term(
                factory.create_string_term(allocator.create_string(error)),
                factory,
                allocator,
            )
        })
}

fn normalize_function_application<
    T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Evaluate<T>,
>(
    target: &T,
    args: &ExpressionList<T>,
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
    target: &LambdaTerm<T>,
    args: &ExpressionList<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    cache: &mut impl EvaluationCache<T>,
) -> Option<T> {
    if args.is_empty() {
        Some(target.body().clone())
    } else {
        let num_args = target.num_args();
        let (inlined_args, remaining_args): (Vec<_>, Vec<_>) = args
            .iter()
            .take(num_args)
            .enumerate()
            .map(|(index, arg)| (num_args - index - 1, arg.clone()))
            .partition(|(offset, arg)| {
                !arg.is_complex() || target.body().count_variable_usages(*offset) <= 1
            });
        if remaining_args.is_empty() {
            Some(
                target
                    .body()
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
                    .unwrap_or_else(|| target.body().clone()),
            )
        } else if !inlined_args.is_empty() {
            Some(
                factory.create_application_term(
                    factory.create_lambda_term(
                        remaining_args.len(),
                        inlined_args.into_iter().fold(
                            target.body().clone(),
                            |body, (offset, arg)| {
                                let arg = arg
                                    .substitute_static(
                                        &Substitutions::increase_scope_offset(
                                            remaining_args.len(),
                                            0,
                                        ),
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
                ),
            )
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
    args: &ExpressionList<T>,
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

pub(crate) fn get_eager_args<'a, T: Expression + 'a>(
    args: impl IntoIterator<Item = &'a T>,
    arity: &Arity,
) -> impl Iterator<Item = &'a T> {
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
    args: &ExpressionList<T>,
    state: Option<&TState>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    cache: &mut impl EvaluationCache<T>,
) -> (FunctionArgs<T>, DependencyList) {
    match validate_function_application_arity(target, arity, args.len()) {
        Err(message) => (
            FunctionArgs::ShortCircuit(create_error_signal_term(
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
                            resolve_eager_value(arg, state, factory, allocator, cache)
                                .unwrap_or_else(|| {
                                    EvaluationResult::new(arg.clone(), DependencyList::empty())
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
                    ArgType::Lazy => EvaluationResult::new(arg.clone(), DependencyList::empty()),
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
                &resolved_args,
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
        is_compiled_application_target(target.target(), factory)
    } else {
        false
    }
}

pub(crate) fn get_short_circuit_signal<T: Expression>(
    args: &[T],
    arity: &Arity,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Option<T> {
    get_combined_short_circuit_signal(
        get_num_short_circuit_signals(args, arity, factory),
        args,
        arity,
        factory,
        allocator,
    )
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub(crate) enum ShortCircuitCount {
    None,
    Single,
    Multiple,
}
impl ShortCircuitCount {
    fn increment(&mut self) {
        *self = match self {
            Self::None => Self::Single,
            Self::Single => Self::Multiple,
            Self::Multiple => Self::Multiple,
        }
    }
}

pub(crate) fn get_num_short_circuit_signals<'a, T: Expression + 'a>(
    args: impl IntoIterator<Item = &'a T>,
    arity: &Arity,
    factory: &'a impl ExpressionFactory<T>,
) -> ShortCircuitCount {
    let short_circuit_args = args
        .into_iter()
        .zip(arity.iter())
        .filter(|(arg, arg_type)| match arg_type {
            ArgType::Strict => factory.match_signal_term(arg).is_some(),
            _ => false,
        });
    let mut num_short_circuit_args = ShortCircuitCount::None;
    for _ in short_circuit_args {
        if let ShortCircuitCount::None = num_short_circuit_args {
            num_short_circuit_args = ShortCircuitCount::Single;
        } else {
            return ShortCircuitCount::Multiple;
        }
    }
    num_short_circuit_args
}

pub(crate) fn get_combined_short_circuit_signal<'a, T: Expression + 'a>(
    num_short_circuit_args: ShortCircuitCount,
    args: impl IntoIterator<Item = &'a T>,
    arity: &Arity,
    factory: &'a impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Option<T> {
    match num_short_circuit_args.into() {
        ShortCircuitCount::None => None,
        ShortCircuitCount::Single => Some(
            filter_short_circuit_args(args.into_iter(), arity, factory)
                .into_iter()
                .map(|(arg, _)| arg)
                .cloned()
                .next()
                .unwrap(),
        ),
        ShortCircuitCount::Multiple => {
            let combined_signal = factory.create_signal_term(
                allocator.create_signal_list(
                    filter_short_circuit_args(args.into_iter(), arity, factory)
                        .into_iter()
                        .flat_map(|(_, signal)| {
                            signal
                                .signals()
                                .iter()
                                .map(|signal| allocator.clone_signal(signal))
                        }),
                ),
            );
            Some(combined_signal)
        }
    }
}

fn filter_short_circuit_args<'a, T: Expression + 'a>(
    args: impl IntoIterator<Item = &'a T>,
    arity: &Arity,
    matcher: &'a impl ExpressionFactory<T>,
) -> impl IntoIterator<Item = (&'a T, &'a SignalTerm<T>)> {
    args.into_iter()
        .zip(arity.iter())
        .filter_map(move |(arg, arg_type)| match arg_type {
            ArgType::Strict => matcher.match_signal_term(&arg).map(|signal| (arg, signal)),
            ArgType::Eager | ArgType::Lazy => None,
        })
}

fn create_error_signal_term<T: Expression>(
    data: T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_signal_term(allocator.create_signal_list(once(
        allocator.create_signal(SignalType::Error, allocator.create_unit_list(data)),
    )))
}

pub(crate) fn compile_args<'a, T: Expression + Compile<T> + 'a>(
    args: impl IntoIterator<Item = (&'a T, ArgType)>,
    stack_offset: StackOffset,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    compiler: &mut Compiler,
) -> Result<Program, String> {
    Ok(Program::new(args.into_iter().enumerate().fold(
        Ok(Program::new(empty())),
        |program, (index, (arg, arg_type))| {
            let mut program = program?;
            match arg.compile(
                match arg_type {
                    ArgType::Eager | ArgType::Strict => VarArgs::Eager,
                    ArgType::Lazy => VarArgs::Lazy,
                },
                stack_offset + index,
                factory,
                allocator,
                compiler,
            ) {
                Err(error) => Err(error),
                Ok(compiled_arg) => {
                    program.extend(compiled_arg);
                    Ok(program)
                }
            }
        },
    )?))
}

fn match_compiled_function_result(program: &Program) -> Option<(InstructionPointer, HashId)> {
    match program.instructions().first() {
        Some(&Instruction::PushFunction { target, hash }) if program.len() == 1 => {
            Some((target, hash))
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {

    use crate::{
        allocator::DefaultAllocator,
        cache::SubstitutionCache,
        core::{ExpressionFactory, HeapAllocator, Rewritable},
        lang::{CachedSharedTerm, SharedTermFactory},
        stdlib::Stdlib,
    };

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
                    factory.create_builtin_term(Stdlib::Subtract),
                    allocator.create_pair(
                        factory.create_variable_term(2),
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Add),
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
                    factory.create_builtin_term(Stdlib::Subtract),
                    allocator.create_pair(
                        factory.create_variable_term(2),
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Add),
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
                factory.create_builtin_term(Stdlib::Subtract),
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
                    factory.create_builtin_term(Stdlib::Add),
                    allocator.create_pair(
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Get),
                            allocator.create_pair(
                                factory.create_variable_term(2),
                                factory.create_variable_term(1),
                            ),
                        ),
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Get),
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
                        factory.create_builtin_term(Stdlib::Add),
                        allocator.create_pair(
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::Get),
                                allocator.create_pair(
                                    factory.create_variable_term(0),
                                    factory.create_int_term(0),
                                ),
                            ),
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::Get),
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
                factory.create_builtin_term(Stdlib::Get),
                allocator.create_pair(
                    factory.create_list_term(allocator.create_unit_list(
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Add),
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
