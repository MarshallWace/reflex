// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{
    collections::HashSet,
    iter::{empty, once},
};

use crate::{
    compiler::{Compile, Compiler, Instruction, InstructionPointer, Program},
    core::{
        transform_expression_list, Applicable, ArgType, Arity, DependencyList, DynamicState,
        EvaluationCache, Expression, ExpressionFactory, ExpressionList, GraphNode, HeapAllocator,
        Reducible, Rewritable, SerializeJson, SignalType, StackOffset, Substitutions, VarArgs,
    },
    lang::{SignalTerm, ValueTerm},
};

#[derive(Hash, Eq, PartialEq, Debug)]
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
    fn dynamic_dependencies(&self) -> DependencyList {
        let target_dependencies = self.target.dynamic_dependencies();
        let eager_args = self.target.arity().map(|arity| {
            self.args
                .iter()
                .zip(arity.iter())
                .filter_map(|(arg, arg_type)| match arg_type {
                    ArgType::Strict | ArgType::Eager => Some(arg),
                    ArgType::Lazy => None,
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
        false
    }
    fn is_atomic(&self) -> bool {
        false
    }
}
impl<T: Expression + Rewritable<T> + Reducible<T> + Applicable<T>> Rewritable<T>
    for ApplicationTerm<T>
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
        Some(factory.create_application_term(target, args))
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
                allocator.create_sized_list(
                    self.args.len(),
                    self.args
                        .iter()
                        .zip(arity.iter())
                        .map(|(arg, arg_type)| match arg_type {
                            ArgType::Strict | ArgType::Eager => arg
                                .substitute_dynamic(state, factory, allocator, cache)
                                .unwrap_or(arg.clone()),
                            ArgType::Lazy => arg.clone(),
                        }),
                )
            })
        } else {
            None
        };
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
        let substituted_expression = match normalized_args {
            Some(args) => Some(factory.create_application_term(
                normalized_target.unwrap_or_else(|| self.target.clone()),
                args,
            )),
            None => normalized_target.map(|target| {
                factory.create_application_term(
                    target,
                    allocator.create_list(self.args.iter().map(|arg| arg.clone())),
                )
            }),
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
            "<call:{}:{}>",
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
    arg.capture_depth() > 0 || !arg.dynamic_dependencies().is_empty() || arg.is_reducible()
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
