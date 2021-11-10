// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use itertools::Itertools;
use serde::{Deserialize, Serialize};
use std::{
    collections::HashSet,
    iter::{empty, once},
};

use crate::{
    compiler::{Compile, Compiler, Instruction, InstructionPointer, Program},
    core::{
        transform_expression_list, Applicable, ArgType, Arity, DependencyList, DynamicState,
        EvaluationCache, Expression, ExpressionFactory, ExpressionList, GraphNode, HeapAllocator,
        Reducible, Rewritable, ScopeOffset, SerializeJson, SignalType, StackOffset, Substitutions,
        VarArgs,
    },
    lang::{LambdaTerm, SignalTerm, ValueTerm},
};

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
        false
    }
    fn is_atomic(&self) -> bool {
        false
    }
}
impl<T: Expression + Rewritable<T> + Reducible<T> + Applicable<T>> Rewritable<T>
    for ApplicationTerm<T>
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
        if self.args.is_empty() {
            let target = normalized_target.as_ref().unwrap_or(&self.target);
            match factory.match_lambda_term(target) {
                Some(target) if target.num_args() == 0 => {
                    return Some(target.body().clone());
                }
                _ => {}
            }
        }
        let normalized_args = transform_expression_list(&self.args, allocator, |arg| {
            arg.normalize(factory, allocator, cache)
        });
        let target = normalized_target.as_ref().unwrap_or(&self.target);
        let args = allocator.clone_list(normalized_args.as_ref().unwrap_or(&self.args));
        if let Some(partial) = factory.match_partial_application_term(target) {
            return factory
                .create_application_term(
                    partial.target().clone(),
                    allocator.create_unsized_list(
                        partial
                            .args()
                            .iter()
                            .cloned()
                            .chain(self.args.iter().cloned()),
                    ),
                )
                .normalize(factory, allocator, cache);
        }
        let substituted_expression = match normalized_args {
            Some(args) => Some(factory.create_application_term(
                normalized_target.unwrap_or_else(|| self.target.clone()),
                args,
            )),
            None => normalized_target.map(|target| {
                factory.create_application_term(target, allocator.clone_list(&self.args))
            }),
        };
        let substituted_expression = match substituted_expression {
            Some(expression) => expression
                .reduce(factory, allocator, cache)
                .or(Some(expression)),
            None => self.reduce(factory, allocator, cache),
        };
        let substituted_expression = substituted_expression.map(|expression| {
            expression
                .normalize(factory, allocator, cache)
                .unwrap_or(expression)
        });
        let result = substituted_expression.map(|expression| {
            abstract_unused_application_args(
                &expression,
                args.as_slice(),
                factory,
                allocator,
                cache,
            )
            .unwrap_or(expression)
        });

        // FIXME: reinstate 'IIFE -> let' conversion
        // let result = result.map(|expression| {
        //     convert_immediately_invoked_lambda_to_let(&expression, factory).unwrap_or(expression)
        // });

        result
    }
}

fn abstract_unused_application_args<T: Expression + Rewritable<T>>(
    expression: &T,
    args: &[T],
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    cache: &mut impl EvaluationCache<T>,
) -> Option<T> {
    let abstracted_args = args
        .iter()
        .unique()
        .filter(|arg| {
            arg.is_complex() && expression.count_subexpression_usages(arg, factory, allocator) > 1
        })
        .cloned()
        .collect::<Vec<_>>();
    if abstracted_args.is_empty() {
        return None;
    }
    let substitutions = abstracted_args
        .iter()
        .enumerate()
        .map(|(index, arg)| {
            (
                arg.clone(),
                factory.create_static_variable_term(abstracted_args.len() - index - 1),
            )
        })
        .collect::<Vec<_>>();
    let substitutions = Substitutions::term_match(
        &substitutions,
        Some(ScopeOffset::Wrap(abstracted_args.len())),
    );
    Some(factory.create_application_term(
        factory.create_lambda_term(
            abstracted_args.len(),
            expression.substitute_static(&substitutions, factory, allocator, cache)?,
        ),
        allocator.create_list(abstracted_args),
    ))
}

fn convert_immediately_invoked_lambda_to_let<T: Expression>(
    target: &T,
    factory: &impl ExpressionFactory<T>,
) -> Option<T> {
    let application = factory.match_application_term(&target)?;
    let lambda = factory.match_lambda_term(application.target())?;
    Some(convert_lambda_application_to_let(
        lambda,
        application.args().as_slice(),
        factory,
    ))
}

fn convert_lambda_application_to_let<T: Expression>(
    lambda: &LambdaTerm<T>,
    args: &[T],
    factory: &impl ExpressionFactory<T>,
) -> T {
    if args.is_empty() {
        lambda.body().clone()
    } else {
        factory.create_let_term(
            args[0].clone(),
            convert_lambda_application_to_let(lambda, &args[1..], factory),
        )
    }
}

impl<T: Expression + Rewritable<T> + Reducible<T> + Applicable<T>> Reducible<T>
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
        let reduced_target = self.target.reduce(factory, allocator, cache);
        let target = reduced_target.as_ref().unwrap_or(&self.target);
        let short_circuit_target = match factory.match_signal_term(target) {
            Some(_) => true,
            _ => false,
        };
        if short_circuit_target {
            return reduced_target;
        }
        match target.arity() {
            None => None,
            Some(arity) => {
                let num_required_args = arity.required().len();
                let num_optional_args = arity.optional().len();
                match validate_function_args(target, &arity, self.args.iter()) {
                    Err(err) => Some(create_error_signal_term(
                        factory.create_value_term(ValueTerm::String(allocator.create_string(err))),
                        factory,
                        allocator,
                    )),
                    Ok(args) => {
                        let num_args = args.len();
                        let result =
                            match reduce_function_args(args, arity, factory, allocator, cache) {
                                Err(args) => Err(args),
                                Ok((mut resolved_args, short_circuit_signal)) => {
                                    let num_positional_args = num_required_args + num_optional_args;
                                    let num_unspecified_optional_args =
                                        num_positional_args.saturating_sub(num_args);
                                    if num_unspecified_optional_args > 0 {
                                        resolved_args.extend(
                                            (0..num_unspecified_optional_args).map(|_| {
                                                factory.create_value_term(ValueTerm::Null)
                                            }),
                                        )
                                    }
                                    if let Some(signal) = short_circuit_signal {
                                        Ok(signal)
                                    } else if is_compiled_application_target(target, factory) {
                                        Err(resolved_args)
                                    } else {
                                        target
                                            .apply(
                                                resolved_args.into_iter(),
                                                factory,
                                                allocator,
                                                cache,
                                            )
                                            .or_else(|error| {
                                                Ok(create_error_signal_term(
                                                    factory.create_value_term(ValueTerm::String(
                                                        allocator.create_string(error),
                                                    )),
                                                    factory,
                                                    allocator,
                                                ))
                                            })
                                    }
                                }
                            };
                        match result {
                            Ok(result) => {
                                Some(result.reduce(factory, allocator, cache).unwrap_or(result))
                            }
                            Err(args) => {
                                if reduced_target.is_none()
                                    && self
                                        .args
                                        .iter()
                                        .zip(args.iter())
                                        .all(|(arg, evaluated_arg)| arg.id() == evaluated_arg.id())
                                {
                                    None
                                } else {
                                    let target =
                                        reduced_target.unwrap_or_else(|| self.target.clone());
                                    Some(factory.create_application_term(
                                        target,
                                        allocator.create_list(args),
                                    ))
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
impl<T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>> Compile<T>
    for ApplicationTerm<T>
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
                    if let Some(target_address) = match_compiled_function_result(&compiled_target) {
                        let mut result = compiled_args;
                        // TODO: jump to target if in tail position
                        result.push(Instruction::Call {
                            target: target_address,
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

pub fn validate_function_args<T: Expression + Applicable<T>, V>(
    target: &T,
    arity: &Arity,
    args: impl ExactSizeIterator<Item = V>,
) -> Result<impl ExactSizeIterator<Item = V>, String> {
    let num_args = args.len();
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
        Ok(WithExactSizeIterator::new(num_args, args.take(num_args)))
    }
}

pub(crate) struct WithExactSizeIterator<T: Iterator> {
    remaining: usize,
    iter: T,
}
impl<T: Iterator> WithExactSizeIterator<T> {
    pub fn new(remaining: usize, iter: T) -> Self {
        Self { remaining, iter }
    }
}
impl<T: Iterator> Iterator for WithExactSizeIterator<T> {
    type Item = T::Item;
    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining == 0 {
            None
        } else {
            self.remaining -= 1;
            self.iter.next()
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.remaining, Some(self.remaining))
    }
    fn count(self) -> usize
    where
        Self: Sized,
    {
        self.remaining
    }
}
impl<T: Iterator> ExactSizeIterator for WithExactSizeIterator<T> {
    fn len(&self) -> usize {
        self.remaining
    }
}

fn reduce_function_args<'a, T: Expression + Rewritable<T> + Reducible<T> + 'a>(
    args: impl IntoIterator<Item = &'a T>,
    arity: Arity,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    cache: &mut impl EvaluationCache<T>,
) -> Result<(Vec<T>, Option<T>), Vec<T>> {
    let mut has_unresolved_args = false;
    let mut num_short_circuit_signals = ShortCircuitCount::None;
    let resolved_args = args
        .into_iter()
        .zip(arity.iter())
        .map(|(arg, arg_type)| match arg_type {
            ArgType::Strict | ArgType::Eager => {
                let resolved_arg = Reducible::reduce(arg, factory, allocator, cache)
                    .unwrap_or_else(|| arg.clone());
                if is_unresolved_arg(&resolved_arg) {
                    has_unresolved_args = true;
                }
                if factory.match_signal_term(&resolved_arg).is_some() {
                    if let ArgType::Strict = arg_type {
                        num_short_circuit_signals.increment();
                    }
                }
                resolved_arg
            }
            ArgType::Lazy => arg.clone(),
        })
        .collect::<Vec<_>>();
    if has_unresolved_args {
        Err(resolved_args)
    } else {
        let short_circuit_signal = get_combined_short_circuit_signal(
            num_short_circuit_signals,
            &resolved_args,
            &arity,
            factory,
            allocator,
        );
        Ok((resolved_args, short_circuit_signal))
    }
}

fn is_unresolved_arg<T: Expression + Rewritable<T> + Reducible<T>>(arg: &T) -> bool {
    arg.capture_depth() > 0 || !arg.dynamic_dependencies(false).is_empty() || arg.is_reducible()
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

fn match_compiled_function_result(program: &Program) -> Option<InstructionPointer> {
    match program.instructions().first() {
        Some(Instruction::PushFunction { target }) if program.len() == 1 => Some(*target),
        _ => None,
    }
}

#[cfg(test)]
mod tests {

    use crate::parser::sexpr::parse;
    use crate::{
        allocator::DefaultAllocator,
        cache::SubstitutionCache,
        core::{ExpressionFactory, HeapAllocator, Rewritable},
        lang::{create_struct, CachedSharedTerm, SharedTermFactory, ValueTerm},
        stdlib::Stdlib,
    };

    use super::convert_immediately_invoked_lambda_to_let;

    #[test]
    fn lambda_let_conversion() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();

        let expression = parse("((lambda () 3))", &factory, &allocator).unwrap();
        assert_eq!(
            convert_immediately_invoked_lambda_to_let(&expression, &factory),
            Some(factory.create_value_term(ValueTerm::Int(3)))
        );

        let expression = parse("((lambda (foo) foo) 3)", &factory, &allocator).unwrap();
        assert_eq!(
            convert_immediately_invoked_lambda_to_let(&expression, &factory),
            Some(factory.create_let_term(
                factory.create_value_term(ValueTerm::Int(3)),
                factory.create_static_variable_term(0)
            ))
        );

        let expression = parse(
            "((lambda (foo bar baz) (+ foo (+ bar baz))) 3 4 5)",
            &factory,
            &allocator,
        )
        .unwrap();
        assert_eq!(
            convert_immediately_invoked_lambda_to_let(&expression, &factory),
            Some(factory.create_let_term(
                factory.create_value_term(ValueTerm::Int(3)),
                factory.create_let_term(
                    factory.create_value_term(ValueTerm::Int(4)),
                    factory.create_let_term(
                        factory.create_value_term(ValueTerm::Int(5)),
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Add),
                            allocator.create_pair(
                                factory.create_static_variable_term(2),
                                factory.create_application_term(
                                    factory.create_builtin_term(Stdlib::Add),
                                    allocator.create_pair(
                                        factory.create_static_variable_term(1),
                                        factory.create_static_variable_term(0),
                                    )
                                )
                            )
                        )
                    )
                )
            ))
        );
    }

    #[test]
    fn variable_inlining_single_use_is_inlined() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
        let expression = factory.create_application_term(
            factory.create_lambda_term(
                1,
                create_struct(
                    [
                        (String::from("foo"), factory.create_static_variable_term(0)),
                        (
                            String::from("bar"),
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::Abs),
                                allocator
                                    .create_unit_list(factory.create_value_term(ValueTerm::Int(3))),
                            ),
                        ),
                    ],
                    &factory,
                    &allocator,
                ),
            ),
            allocator.create_unit_list(factory.create_struct_term(
                allocator.create_struct_prototype([String::from("x")]),
                allocator.create_unit_list(factory.create_value_term(ValueTerm::Boolean(true))),
            )),
        );
        let result = expression.normalize(&factory, &allocator, &mut SubstitutionCache::new());
        assert_eq!(
            result,
            Some(create_struct(
                [
                    (
                        String::from("foo"),
                        factory.create_struct_term(
                            allocator.create_struct_prototype([String::from("x")]),
                            allocator.create_unit_list(
                                factory.create_value_term(ValueTerm::Boolean(true))
                            ),
                        )
                    ),
                    (
                        String::from("bar"),
                        factory.create_value_term(ValueTerm::Int(3))
                    ),
                ],
                &factory,
                &allocator,
            ))
        );
    }

    #[test]
    fn variable_inlining_subexpressions_normalized_but_complex_variables_not_inlined_twice() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
        let expression = factory.create_application_term(
            factory.create_lambda_term(
                1,
                create_struct(
                    [
                        (String::from("foo"), factory.create_static_variable_term(0)),
                        (
                            String::from("bar"),
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::Abs),
                                allocator
                                    .create_unit_list(factory.create_value_term(ValueTerm::Int(3))),
                            ),
                        ),
                        (String::from("baz"), factory.create_static_variable_term(0)),
                    ],
                    &factory,
                    &allocator,
                ),
            ),
            allocator.create_unit_list(factory.create_struct_term(
                allocator.create_struct_prototype([String::from("x")]),
                allocator.create_unit_list(factory.create_value_term(ValueTerm::Boolean(true))),
            )),
        );
        let result = expression.normalize(&factory, &allocator, &mut SubstitutionCache::new());
        assert_eq!(
            result,
            // Some(factory.create_let_term(
            //     factory.create_struct_term(
            //         allocator.create_struct_prototype([String::from("x")]),
            //         allocator.create_unit_list(factory.create_value_term(ValueTerm::Boolean(true)))
            //     ),
            //     create_struct(
            //         [
            //             (String::from("foo"), factory.create_static_variable_term(0)),
            //             (
            //                 String::from("bar"),
            //                 factory.create_value_term(ValueTerm::Int(3)),
            //             ),
            //             (String::from("baz"), factory.create_static_variable_term(0)),
            //         ],
            //         &factory,
            //         &allocator,
            //     ),
            // )),
            Some(factory.create_application_term(
                factory.create_lambda_term(
                    1,
                    create_struct(
                        [
                            (String::from("foo"), factory.create_static_variable_term(0)),
                            (
                                String::from("bar"),
                                factory.create_value_term(ValueTerm::Int(3)),
                            ),
                            (String::from("baz"), factory.create_static_variable_term(0)),
                        ],
                        &factory,
                        &allocator,
                    ),
                ),
                allocator.create_unit_list(factory.create_struct_term(
                    allocator.create_struct_prototype([String::from("x")]),
                    allocator.create_unit_list(factory.create_value_term(ValueTerm::Boolean(true)))
                )),
            )),
        );
    }

    #[test]
    fn variable_inlining_primitive_args_inlined_and_subexpressions_normalized() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
        let expression = factory.create_application_term(
            factory.create_lambda_term(
                2,
                create_struct(
                    [
                        (String::from("foo"), factory.create_static_variable_term(1)),
                        (
                            String::from("bar"),
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::Add),
                                allocator.create_pair(
                                    factory.create_static_variable_term(0),
                                    factory.create_static_variable_term(0),
                                ),
                            ),
                        ),
                        (String::from("baz"), factory.create_static_variable_term(1)),
                    ],
                    &factory,
                    &allocator,
                ),
            ),
            allocator.create_pair(
                factory.create_struct_term(
                    allocator.create_struct_prototype([String::from("x")]),
                    allocator.create_unit_list(factory.create_value_term(ValueTerm::Boolean(true))),
                ),
                factory.create_value_term(ValueTerm::Int(3)),
            ),
        );
        let result = expression.normalize(&factory, &allocator, &mut SubstitutionCache::new());
        assert_eq!(
            result,
            // Some(factory.create_let_term(
            //     factory.create_struct_term(
            //         allocator.create_struct_prototype([String::from("x")]),
            //         allocator.create_unit_list(factory.create_value_term(ValueTerm::Boolean(true)))
            //     ),
            //     create_struct(
            //         [
            //             (String::from("foo"), factory.create_static_variable_term(0)),
            //             (
            //                 String::from("bar"),
            //                 factory.create_value_term(ValueTerm::Int(3 + 3)),
            //             ),
            //             (String::from("baz"), factory.create_static_variable_term(0)),
            //         ],
            //         &factory,
            //         &allocator,
            //     ),
            // ))
            Some(factory.create_application_term(
                factory.create_lambda_term(
                    1,
                    create_struct(
                        [
                            (String::from("foo"), factory.create_static_variable_term(0)),
                            (
                                String::from("bar"),
                                factory.create_value_term(ValueTerm::Int(3 + 3)),
                            ),
                            (String::from("baz"), factory.create_static_variable_term(0)),
                        ],
                        &factory,
                        &allocator,
                    ),
                ),
                allocator.create_unit_list(factory.create_struct_term(
                    allocator.create_struct_prototype([String::from("x")]),
                    allocator.create_unit_list(factory.create_value_term(ValueTerm::Boolean(true)))
                )),
            )),
        );
    }

    #[test]
    fn variable_inlining_complex_args_inlined_if_it_simplifies_overall() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
        let expression = factory.create_application_term(
            factory.create_lambda_term(
                3,
                create_struct(
                    [
                        (
                            // foo -> {"X": 1, "Y": 2}["X"]
                            String::from("foo"),
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::Get),
                                allocator.create_pair(
                                    factory.create_static_variable_term(0),
                                    factory.create_static_variable_term(1),
                                ),
                            ),
                        ),
                        (
                            // bar -> {"X": 1, "Y": 2}["Y"]
                            String::from("bar"),
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::Get),
                                allocator.create_pair(
                                    factory.create_static_variable_term(0),
                                    factory.create_static_variable_term(2),
                                ),
                            ),
                        ),
                        (String::from("baz"), factory.create_static_variable_term(0)),
                    ],
                    &factory,
                    &allocator,
                ),
            ),
            allocator.create_list(vec![
                factory.create_value_term(ValueTerm::String(String::from("Y"))),
                factory.create_value_term(ValueTerm::String(String::from("X"))),
                create_struct(
                    [
                        (
                            String::from("X"),
                            factory.create_value_term(ValueTerm::Int(1)),
                        ),
                        (
                            String::from("Y"),
                            factory.create_value_term(ValueTerm::Int(2)),
                        ),
                    ],
                    &factory,
                    &allocator,
                ),
            ]),
        );
        let result = expression.normalize(&factory, &allocator, &mut SubstitutionCache::new());
        assert_eq!(
            result,
            Some(create_struct(
                [
                    (
                        // foo -> {"X": 1, "Y": 2}["X"]
                        String::from("foo"),
                        factory.create_value_term(ValueTerm::Int(1))
                    ),
                    (
                        // bar -> {"X": 1, "Y": 2}["Y"]
                        String::from("bar"),
                        factory.create_value_term(ValueTerm::Int(2))
                    ),
                    (
                        String::from("baz"),
                        create_struct(
                            [
                                (
                                    String::from("X"),
                                    factory.create_value_term(ValueTerm::Int(1)),
                                ),
                                (
                                    String::from("Y"),
                                    factory.create_value_term(ValueTerm::Int(2)),
                                ),
                            ],
                            &factory,
                            &allocator,
                        )
                    ),
                ],
                &factory,
                &allocator,
            ))
        );
    }

    #[test]
    fn variable_inlining_pairs_of_args_inlined() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
        let expression = factory.create_application_term(
            factory.create_lambda_term(
                3,
                create_struct(
                    [
                        (String::from("foo"), factory.create_static_variable_term(2)),
                        (
                            String::from("bar"),
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::Add),
                                allocator.create_pair(
                                    factory.create_application_term(
                                        factory.create_builtin_term(Stdlib::Add),
                                        allocator.create_pair(
                                            factory.create_static_variable_term(0),
                                            factory.create_static_variable_term(1),
                                        ),
                                    ),
                                    factory.create_application_term(
                                        factory.create_builtin_term(Stdlib::Add),
                                        allocator.create_pair(
                                            factory.create_static_variable_term(0),
                                            factory.create_static_variable_term(1),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                        (String::from("baz"), factory.create_static_variable_term(2)),
                    ],
                    &factory,
                    &allocator,
                ),
            ),
            allocator.create_list([
                factory.create_struct_term(
                    allocator.create_struct_prototype([String::from("x")]),
                    allocator.create_unit_list(factory.create_value_term(ValueTerm::Boolean(true))),
                ),
                factory.create_value_term(ValueTerm::Int(3)),
                factory.create_value_term(ValueTerm::Int(4)),
            ]),
        );
        let result = expression.normalize(&factory, &allocator, &mut SubstitutionCache::new());
        assert_eq!(
            result,
            // Some(factory.create_let_term(
            //     factory.create_struct_term(
            //         allocator.create_struct_prototype([String::from("x")]),
            //         allocator.create_unit_list(factory.create_value_term(ValueTerm::Boolean(true)))
            //     ),
            //     create_struct(
            //         [
            //             (String::from("foo"), factory.create_static_variable_term(0)),
            //             (
            //                 String::from("bar"),
            //                 factory.create_value_term(ValueTerm::Int(3 + 4 + 3 + 4)),
            //             ),
            //             (String::from("baz"), factory.create_static_variable_term(0)),
            //         ],
            //         &factory,
            //         &allocator,
            //     ),
            // )),
            Some(factory.create_application_term(
                factory.create_lambda_term(
                    1,
                    create_struct(
                        [
                            (String::from("foo"), factory.create_static_variable_term(0)),
                            (
                                String::from("bar"),
                                factory.create_value_term(ValueTerm::Int(3 + 4 + 3 + 4)),
                            ),
                            (String::from("baz"), factory.create_static_variable_term(0)),
                        ],
                        &factory,
                        &allocator,
                    ),
                ),
                allocator.create_unit_list(factory.create_struct_term(
                    allocator.create_struct_prototype([String::from("x")]),
                    allocator.create_unit_list(factory.create_value_term(ValueTerm::Boolean(true)))
                )),
            )),
        );
    }

    #[test]
    fn variable_inlining_lambda_as_a_variable() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
        let expression = parse(
            "
            ((lambda (value) ((lambda (foo) foo) (* value 2))) 3)
        ",
            &factory,
            &allocator,
        )
        .unwrap();
        assert_eq!(
            expression.normalize(&factory, &allocator, &mut SubstitutionCache::new()),
            Some(factory.create_value_term(ValueTerm::Int(6)))
        );
    }

    #[test]
    fn normalize_zero_arg_applications() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();

        let expression = factory.create_application_term(
            factory.create_lambda_term(0, factory.create_value_term(ValueTerm::Int(3))),
            allocator.create_empty_list(),
        );
        let result = expression.normalize(&factory, &allocator, &mut SubstitutionCache::new());
        assert_eq!(result, Some(factory.create_value_term(ValueTerm::Int(3))));
    }
}
