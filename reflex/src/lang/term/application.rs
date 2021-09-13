// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::HashSet,
    iter::{empty, once},
};

use crate::{
    compiler::{
        Compile, Compiler, Instruction, InstructionPointer, NativeFunctionRegistry, Program,
    },
    core::{
        transform_expression_list, Applicable, Arity, DependencyList, DynamicState,
        EvaluationCache, Expression, ExpressionFactory, ExpressionList, GraphNode, HeapAllocator,
        Reducible, Rewritable, SignalType, StackOffset, Substitutions, VarArgs,
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
        state: &DynamicState<T>,
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
                if self.args.len() < arity.required() {
                    return Some(create_error_signal_term(
                        factory.create_value_term(ValueTerm::String(allocator.create_string(
                            format!(
                                "Expected {}, received {}",
                                match arity.required() {
                                    1 => String::from("1 argument"),
                                    arity => format!("{} arguments", arity),
                                },
                                self.args.len()
                            ),
                        ))),
                        factory,
                        allocator,
                    ));
                }
                let result = match reduce_function_args(
                    self.args.iter(),
                    arity,
                    factory,
                    allocator,
                    cache,
                ) {
                    Err(args) => Err(args),
                    Ok((args, signal)) => {
                        if is_compiled_application_target(target, factory) {
                            Err(args)
                        } else {
                            match signal {
                                None => target.apply(args.into_iter(), factory, allocator, cache),
                                Some(signal) => match factory.match_signal_transformer_term(target)
                                {
                                    Some(transformer) => {
                                        transformer.apply(once(signal), factory, allocator, cache)
                                    }
                                    _ => Ok(signal),
                                },
                            }
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
                    Ok(result) => Some(result.reduce(factory, allocator, cache).unwrap_or(result)),
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
                            let target = reduced_target.unwrap_or_else(|| self.target.clone());
                            Some(
                                factory
                                    .create_application_term(target, allocator.create_list(args)),
                            )
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
    ) -> Result<(Program, NativeFunctionRegistry<T>), String> {
        let target = &self.target;
        let args = &self.args;
        let num_args = args.len();
        let eager_arity = match eager {
            VarArgs::Lazy => None,
            VarArgs::Eager => target.arity(),
        };
        let (compiled_target, target_native_functions) =
            target.compile(eager, stack_offset + num_args, factory, allocator, compiler)?;
        let (compiled_args, arg_native_functions) = match eager_arity {
            None => compile_args(
                args.iter().map(|arg| (arg, VarArgs::Lazy)),
                stack_offset,
                factory,
                allocator,
                compiler,
            ),
            Some(arity) => compile_args(
                with_eagerness(args.iter(), &arity),
                stack_offset,
                factory,
                allocator,
                compiler,
            ),
        }?;
        let mut combined_native_functions = target_native_functions;
        combined_native_functions.extend(arg_native_functions);
        match eager_arity {
            None => {
                let mut result = compiled_args;
                result.extend(compiled_target);
                result.push(Instruction::ConstructApplication { num_args });
                Ok((result, combined_native_functions))
            }
            Some(arity) => {
                if num_args < arity.required() {
                    Err(format!(
                        "{}: expected {}{} arguments, received {}",
                        target,
                        arity.required(),
                        if arity.variadic().is_some() {
                            " or more"
                        } else {
                            ""
                        },
                        num_args,
                    ))
                } else {
                    let mut compiled_args = compiled_args;
                    if arity.variadic().is_some() {
                        compiled_args.push(Instruction::ConstructTuple {
                            size: num_args - arity.required(),
                        });
                    }
                    if let Some(target_address) = match_compiled_function_result(&compiled_target) {
                        let mut result = compiled_args;
                        // TODO: jump to target if in tail position
                        result.push(Instruction::Call {
                            target: target_address,
                        });
                        Ok((result, combined_native_functions))
                    } else {
                        let mut result = compiled_args;
                        result.extend(compiled_target);
                        if !target.is_static() {
                            result.push(Instruction::Evaluate);
                        }
                        result.push(Instruction::Apply { num_args });
                        Ok((result, combined_native_functions))
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
impl<T: Expression> serde::Serialize for ApplicationTerm<T> {
    fn serialize<S>(&self, _serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        Err(serde::ser::Error::custom(format!(
            "Unable to serialize term: {}",
            self
        )))
    }
}

fn reduce_function_args<'a, T: Expression + Rewritable<T> + Reducible<T> + 'a>(
    args: impl IntoIterator<
        Item = &'a T,
        IntoIter = (impl ExactSizeIterator<Item = &'a T> + DoubleEndedIterator<Item = &'a T>),
    >,
    arity: Arity,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    cache: &mut impl EvaluationCache<T>,
) -> Result<(Vec<T>, Option<T>), Vec<T>> {
    let args = args.into_iter();
    let args = with_eagerness(args, &arity)
        .into_iter()
        .map(|(arg, eager)| match eager {
            VarArgs::Eager => {
                Reducible::reduce(arg, factory, allocator, cache).unwrap_or_else(|| arg.clone())
            }
            VarArgs::Lazy => arg.clone(),
        })
        .collect::<Vec<_>>();
    let has_unresolved_args =
        with_eagerness(args.iter(), &arity)
            .into_iter()
            .any(|(arg, eager)| match eager {
                VarArgs::Eager => is_unresolved_arg(arg),
                VarArgs::Lazy => false,
            });
    if has_unresolved_args {
        return Err(args);
    }
    let short_circuit_signal = get_short_circuit_signal(&args, &arity, factory, allocator);
    Ok((args, short_circuit_signal))
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
    let num_short_circuit_args = filter_signal_args(args.iter(), arity, factory)
        .into_iter()
        .count();
    match num_short_circuit_args {
        0 => None,
        1 => Some(
            filter_signal_args(args.iter(), arity, factory)
                .into_iter()
                .map(|(arg, _)| arg)
                .cloned()
                .next()
                .unwrap(),
        ),
        _ => {
            let combined_signal = factory.create_signal_term(
                allocator.create_signal_list(
                    filter_signal_args(args.iter(), arity, factory)
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

fn filter_signal_args<'a, T: Expression + 'a>(
    args: impl IntoIterator<
            Item = &'a T,
            IntoIter = (impl ExactSizeIterator<Item = &'a T> + DoubleEndedIterator<Item = &'a T>),
        > + ExactSizeIterator,
    arity: &Arity,
    matcher: &'a impl ExpressionFactory<T>,
) -> impl IntoIterator<Item = (&'a T, &'a SignalTerm<T>)> {
    with_eagerness(args.into_iter(), arity)
        .into_iter()
        .filter_map(move |(arg, eager)| match eager {
            VarArgs::Eager => matcher.match_signal_term(&arg).map(|signal| (arg, signal)),
            VarArgs::Lazy => None,
        })
}

pub(crate) fn with_eagerness<T>(
    args: impl IntoIterator<
        Item = T,
        IntoIter = (impl ExactSizeIterator<Item = T> + DoubleEndedIterator<Item = T>),
    >,
    arity: &Arity,
) -> impl IntoIterator<
    Item = (T, VarArgs),
    IntoIter = (impl ExactSizeIterator<Item = (T, VarArgs)> + DoubleEndedIterator<Item = (T, VarArgs)>),
> {
    let required_arity = arity.required();
    let eager_arity = arity.eager();
    let eager_varargs = match arity.variadic() {
        Some(VarArgs::Eager) => true,
        _ => false,
    };
    args.into_iter().enumerate().map(move |(index, arg)| {
        let is_eager = index < eager_arity || (eager_varargs && index >= required_arity);
        (
            arg,
            match is_eager {
                true => VarArgs::Eager,
                false => VarArgs::Lazy,
            },
        )
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
    args: impl IntoIterator<
        Item = (&'a T, VarArgs),
        IntoIter = impl DoubleEndedIterator<Item = (&'a T, VarArgs)>,
    >,
    stack_offset: StackOffset,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    compiler: &mut Compiler,
) -> Result<(Program, NativeFunctionRegistry<T>), String> {
    args.into_iter().enumerate().fold(
        Ok((Program::new(empty()), NativeFunctionRegistry::default())),
        |result, (index, (arg, eager))| {
            let (mut program, mut native_functions) = result?;
            let (compiled_arg, arg_native_functions) =
                arg.compile(eager, stack_offset + index, factory, allocator, compiler)?;
            native_functions.extend(arg_native_functions);
            program.extend(compiled_arg);
            Ok((program, native_functions))
        },
    )
}

fn match_compiled_function_result(program: &Program) -> Option<InstructionPointer> {
    match program.instructions().first() {
        Some(Instruction::PushFunction { target }) if program.len() == 1 => Some(*target),
        _ => None,
    }
}
