// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::convert::TryInto;
use std::{collections::hash_map::DefaultHasher, hash::Hasher, iter::once};

use tracing::info_span;
use tracing::trace;

pub use cache::{
    CacheEntries, DefaultInterpreterCache, GcMetrics, InterpreterCache, InterpreterCacheEntry,
    InterpreterCacheKey,
};
pub use stack::{CallStack, VariableStack};

use crate::hash::hash_object;
use crate::{
    cache::NoopCache,
    compiler::{Instruction, InstructionPointer, Program},
    core::{
        Applicable, ArgType, Arity, DependencyList, DynamicState, EvaluationResult, Expression,
        ExpressionFactory, ExpressionList, HeapAllocator, Reducible, Rewritable,
    },
    hash::HashId,
    lang::{
        get_combined_short_circuit_signal, get_num_short_circuit_signals, get_short_circuit_signal,
        validate_function_application_arity, CompiledFunctionTerm, ValueTerm,
    },
};

mod cache;
mod stack;

#[derive(Debug)]
enum ExecutionResult<T: Expression> {
    Advance,
    Jump(InstructionPointer),
    CallFunction {
        target_address: InstructionPointer,
        num_args: usize,
        caller_address: InstructionPointer,
        resume_address: InstructionPointer,
    },
    Return,
    ResolveExpression,
    ResolveApplicationTarget {
        target: T,
        args: ExpressionList<T>,
    },
    ResolveApplicationArgs {
        target: T,
        args: Vec<T>,
        caller_address: InstructionPointer,
        resume_address: InstructionPointer,
    },
}

#[derive(Copy, Clone, Debug)]
pub struct InterpreterOptions {
    pub debug_instructions: bool,
    pub debug_stack: bool,
    pub call_stack_size: Option<usize>,
    pub variable_stack_size: Option<usize>,
}
impl Default for InterpreterOptions {
    fn default() -> Self {
        Self {
            debug_instructions: false,
            debug_stack: false,
            call_stack_size: Some(1024),
            variable_stack_size: Some(4096),
        }
    }
}
impl InterpreterOptions {
    pub fn debug() -> Self {
        Self {
            debug_instructions: true,
            debug_stack: true,
            ..Self::default()
        }
    }
}

pub fn execute<'a, T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + 'a>(
    cache_key: HashId,
    program: &Program,
    entry_point: InstructionPointer,
    state: &impl DynamicState<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    options: &InterpreterOptions,
    cache: &impl InterpreterCache<T>,
) -> Result<(EvaluationResult<T>, CacheEntries<T>), String> {
    match program.get(entry_point) {
        Some(&Instruction::Function { required_args, .. }) if required_args == 0 => Ok(()),
        Some(instruction) => Err(format!(
            "Invalid entry point function: {:x} {:?}",
            entry_point, instruction
        )),
        _ => Err(format!("Invalid entry point address: {:x}", entry_point)),
    }?;
    let execution_span = info_span!("interpreter::execute");
    let execution_span = execution_span.enter();
    trace!("Starting execution");
    let mut stack = VariableStack::new(options.variable_stack_size);
    let mut call_stack = CallStack::new(program, entry_point, options.call_stack_size);
    let mut cache_entries = CacheEntries::default();
    let result = evaluate_program_loop(
        state,
        &mut stack,
        &mut call_stack,
        factory,
        allocator,
        cache,
        &mut cache_entries,
        options.debug_instructions,
        options.debug_stack,
    );
    std::mem::drop(execution_span);
    match result {
        Err(error) => Err(error),
        Ok(value) => {
            assert!(
                call_stack.call_stack_depth() == 0,
                "{} frames remaining on call stack",
                call_stack.call_stack_depth(),
            );
            assert!(
                stack.len() == 0,
                "{} values remaining on variable stack",
                stack.len()
            );
            let (dependencies, subexpressions) = call_stack.drain();
            let result = EvaluationResult::new(value, dependencies);
            cache_entries.insert(
                cache_key,
                InterpreterCacheEntry::new(result.clone(), state),
                subexpressions,
            );
            Ok((result, cache_entries))
        }
    }
}

fn evaluate_program_loop<T: Expression + Rewritable<T> + Reducible<T> + Applicable<T>>(
    state: &impl DynamicState<T>,
    stack: &mut VariableStack<T>,
    call_stack: &mut CallStack<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    cache: &impl InterpreterCache<T>,
    cache_entries: &mut CacheEntries<T>,
    debug_instructions: bool,
    debug_stack: bool,
) -> Result<T, String> {
    loop {
        let result = match call_stack.lookup_instruction(call_stack.program_counter()) {
            None => Err(format!(
                "Invalid program instruction offset: {:x}",
                call_stack.program_counter(),
            )),
            Some(instruction) => {
                if debug_instructions {
                    eprintln!("{}", format_current_instruction(call_stack));
                }
                if debug_stack {
                    eprintln!("{}", format_current_stack(stack))
                }
                evaluate_instruction(
                    instruction,
                    state,
                    stack,
                    call_stack,
                    factory,
                    allocator,
                    cache,
                    cache_entries,
                )
            }
        };
        match result {
            Err(error) => return Err(format!("{:x}: {}", call_stack.program_counter(), error)),
            Ok((result, dependencies)) => {
                if !dependencies.is_empty() {
                    call_stack.add_state_dependencies(dependencies);
                }
                match result {
                    ExecutionResult::ResolveExpression => {
                        let expression = stack.peek().unwrap();
                        if !expression.is_static() {
                            call_stack.enter_expression(expression.id());
                        }
                        continue;
                    }
                    ExecutionResult::ResolveApplicationTarget { target, args } => {
                        if target.is_static() {
                            stack.push(factory.create_application_term(target, args));
                            continue;
                        } else {
                            let target_hash = target.id();
                            stack.push(target);
                            call_stack.enter_application_target(args);
                            call_stack.enter_expression(target_hash);
                            continue;
                        }
                    }
                    ExecutionResult::ResolveApplicationArgs {
                        target,
                        args,
                        caller_address,
                        resume_address,
                    } => {
                        let arity = get_function_arity(&target)?;
                        let results =
                            fill_resolved_args(&args, &arity, Vec::with_capacity(args.len()));
                        let next_arg = args.get(results.len()).unwrap().clone();
                        call_stack
                            .update_program_counter(call_stack.evaluate_arg_program_counter());
                        call_stack.enter_application_arg_list(
                            target,
                            arity,
                            caller_address,
                            resume_address,
                            args,
                            results,
                        );
                        let expression_hash = next_arg.id();
                        stack.push(next_arg);
                        call_stack.enter_expression(expression_hash);
                    }
                    ExecutionResult::Advance => {
                        let previous_address = call_stack.program_counter();
                        let is_leaving_evaluation = call_stack
                            .lookup_instruction(previous_address)
                            .map(|instruction| match instruction {
                                Instruction::Evaluate => true,
                                _ => false,
                            })
                            .unwrap_or(false);
                        let next_address = if is_leaving_evaluation {
                            let value = stack.peek().unwrap();
                            while let Some((hash, dependencies, subexpressions)) =
                                call_stack.pop_expression_stack(previous_address)
                            {
                                call_stack.add_state_dependencies(dependencies.iter());
                                cache_entries.insert(
                                    hash,
                                    InterpreterCacheEntry::new(
                                        EvaluationResult::new(value.clone(), dependencies),
                                        state,
                                    ),
                                    subexpressions,
                                );
                            }
                            if let Some((args, dependencies, subexpressions)) =
                                call_stack.pop_application_target_stack()
                            {
                                call_stack.add_state_dependencies(dependencies);
                                call_stack.add_subexpressions(subexpressions);
                                let resolved_target = stack.pop().unwrap();
                                stack.push(factory.create_application_term(
                                    resolved_target,
                                    allocator.create_list(args),
                                ));
                                continue;
                            } else if let Some((
                                target,
                                arity,
                                caller_address,
                                resume_address,
                                args,
                                results,
                                dependencies,
                                subexpressions,
                            )) = call_stack.pop_application_arg_list_stack()
                            {
                                call_stack.add_state_dependencies(dependencies);
                                call_stack.add_subexpressions(subexpressions);
                                let resolved_item = stack.pop().unwrap();
                                let mut results = results;
                                results.push(resolved_item);
                                let results = fill_resolved_args(&args, &arity, results);
                                if results.len() < args.len() {
                                    let next_arg = args.get(results.len()).unwrap().clone();
                                    call_stack.enter_application_arg_list(
                                        target,
                                        arity,
                                        caller_address,
                                        resume_address,
                                        args,
                                        results,
                                    );
                                    let expression_hash = next_arg.id();
                                    stack.push(next_arg);
                                    call_stack.enter_expression(expression_hash);
                                    continue;
                                } else if let Some(signal) =
                                    get_short_circuit_signal(&results, &arity, factory, allocator)
                                {
                                    stack.push(signal);
                                    resume_address
                                } else {
                                    let result = apply_function(
                                        &target,
                                        results.into_iter(),
                                        factory,
                                        allocator,
                                    )?;
                                    stack.push(result);
                                    resume_address
                                }
                            } else {
                                call_stack.next_program_counter()
                            }
                        } else {
                            call_stack.next_program_counter()
                        };
                        call_stack.update_program_counter(next_address);
                        let is_entering_evaluation = call_stack
                            .current_instruction()
                            .map(is_evaluate_instruction)
                            .unwrap_or(false);
                        if is_entering_evaluation {
                            let expression = stack.peek().unwrap();
                            if !expression.is_static() {
                                call_stack.enter_expression(expression.id());
                            }
                        }
                        continue;
                    }
                    ExecutionResult::Jump(next_address) => {
                        call_stack.update_program_counter(next_address);
                        continue;
                    }
                    ExecutionResult::CallFunction {
                        num_args,
                        target_address,
                        caller_address,
                        resume_address,
                    } => {
                        if let Some((target_hash, num_required_args, num_optional_args)) =
                            call_stack
                                .lookup_instruction(target_address)
                                .and_then(|target| match target {
                                    &Instruction::Function {
                                        hash,
                                        required_args,
                                        optional_args,
                                    } => Some((hash, required_args, optional_args)),
                                    _ => None,
                                })
                        {
                            let hash =
                                generate_function_call_hash(&target_hash, stack.slice(num_args));
                            if let Some((cached_result, updated_cache_entry)) =
                                retrieve_cached_result(&hash, state, cache, cache_entries)
                            {
                                if let Some(cache_entry) = updated_cache_entry {
                                    cache_entries.insert(hash, cache_entry, Vec::new())
                                }
                                let (result, dependencies) = cached_result.into_parts();
                                call_stack.add_subexpression(hash);
                                call_stack.add_state_dependencies(dependencies.iter());
                                stack.pop_multiple(num_args);
                                stack.push(result);

                                let is_resuming_arg_evaluation = resume_address == caller_address
                                    && call_stack.peek_application_arg_list_stack().is_some();
                                if is_resuming_arg_evaluation {
                                    process_next_list_item(stack, call_stack, factory, allocator)?;
                                    continue;
                                } else {
                                    call_stack.update_program_counter(resume_address);
                                    continue;
                                }
                            } else {
                                let num_positional_args = num_required_args + num_optional_args;
                                let num_unspecified_optional_args =
                                    num_positional_args.saturating_sub(num_args);
                                stack.push_multiple(
                                    (0..num_unspecified_optional_args)
                                        .map(|_| factory.create_value_term(ValueTerm::Null)),
                                );
                                call_stack.enter_function(
                                    hash,
                                    target_address,
                                    caller_address,
                                    resume_address,
                                );
                                continue;
                            }
                        } else {
                            return Err(format!(
                                "Invalid function call target address: {:x}",
                                target_address
                            ));
                        }
                    }
                    ExecutionResult::Return => match call_stack.pop_function_stack() {
                        Some((
                            hash,
                            dependencies,
                            subexpressions,
                            caller_address,
                            resume_address,
                        )) => {
                            let value = stack.peek().unwrap();
                            call_stack.add_state_dependencies(dependencies.iter());
                            cache_entries.insert(
                                hash,
                                InterpreterCacheEntry::new(
                                    EvaluationResult::new(value.clone(), dependencies),
                                    state,
                                ),
                                subexpressions,
                            );

                            let is_entering_evaluation = call_stack
                                .lookup_instruction(resume_address)
                                .map(is_evaluate_instruction)
                                .unwrap_or(false);
                            if is_entering_evaluation {
                                let expression = value;
                                if expression.is_static() {
                                    call_stack.update_program_counter(resume_address);
                                    continue;
                                } else {
                                    call_stack.update_program_counter(resume_address);
                                    call_stack.enter_expression(expression.id());
                                    continue;
                                }
                            }

                            let is_resuming_arg_evaluation = resume_address == caller_address
                                && call_stack.peek_application_arg_list_stack().is_some();
                            if is_resuming_arg_evaluation {
                                process_next_list_item(stack, call_stack, factory, allocator)?;
                                continue;
                            }

                            call_stack.update_program_counter(resume_address);
                            continue;
                        }
                        None => match stack.pop() {
                            Some(value) => return Ok(value),
                            None => return Err(String::from("Program did not return a result")),
                        },
                    },
                }
            }
        }
    }
}

fn evaluate_instruction<T: Expression + Rewritable<T> + Reducible<T> + Applicable<T>>(
    instruction: &Instruction,
    state: &impl DynamicState<T>,
    stack: &mut VariableStack<T>,
    call_stack: &CallStack<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    cache: &impl InterpreterCache<T>,
    cache_entries: &mut CacheEntries<T>,
) -> Result<(ExecutionResult<T>, DependencyList), String> {
    match instruction {
        Instruction::PushStatic { offset } => {
            trace!(instruction = "Instruction::PushStatic");
            let value = stack.get(*offset).cloned();
            match value {
                None => Err(format!("Invalid stack offset: {}", offset)),
                Some(value) => {
                    stack.push(value);
                    Ok((ExecutionResult::Advance, DependencyList::empty()))
                }
            }
        }
        Instruction::PushDynamic { state_token } => {
            trace!(instruction = "Instruction::PushDynamic");
            let fallback = stack.pop();
            match fallback {
                None => Err(format!(
                    "Missing dynamic variable fallback: {}",
                    state_token
                )),
                Some(fallback) => {
                    let value = state.get(state_token).cloned().unwrap_or(fallback);
                    stack.push(value);
                    Ok((ExecutionResult::Advance, DependencyList::of(*state_token)))
                }
            }
        }
        Instruction::PushNull => {
            trace!(instruction = "Instruction::PushNull");
            stack.push(factory.create_value_term(ValueTerm::Null));
            Ok((ExecutionResult::Advance, DependencyList::empty()))
        }
        Instruction::PushBoolean { value } => {
            trace!(instruction = "Instruction::PushBoolean");
            stack.push(factory.create_value_term(ValueTerm::Boolean(*value)));
            Ok((ExecutionResult::Advance, DependencyList::empty()))
        }
        Instruction::PushInt { value } => {
            trace!(instruction = "Instruction::PushInt");
            stack.push(factory.create_value_term(ValueTerm::Int(*value)));
            Ok((ExecutionResult::Advance, DependencyList::empty()))
        }
        Instruction::PushFloat { value } => {
            trace!(instruction = "Instruction::PushFloat");
            stack.push(factory.create_value_term(ValueTerm::Float(*value)));
            Ok((ExecutionResult::Advance, DependencyList::empty()))
        }
        Instruction::PushString { value } => {
            trace!(instruction = "Instruction::PushString");
            stack.push(
                factory
                    .create_value_term(ValueTerm::String(allocator.create_string(value.as_str()))),
            );
            Ok((ExecutionResult::Advance, DependencyList::empty()))
        }
        Instruction::PushSymbol { value } => {
            trace!(instruction = "Instruction::PushSymbol");
            stack.push(factory.create_value_term(ValueTerm::Symbol(*value)));
            Ok((ExecutionResult::Advance, DependencyList::empty()))
        }
        Instruction::PushHash { value } => {
            trace!(instruction = "Instruction::PushHash");
            stack.push(factory.create_value_term(ValueTerm::Hash(*value)));
            Ok((ExecutionResult::Advance, DependencyList::empty()))
        }
        Instruction::PushFunction { target } => {
            trace!(instruction = "Instruction::PushFunction");
            let target_address = *target;
            match call_stack.lookup_instruction(target_address) {
                Some(&Instruction::Function {
                    hash,
                    required_args,
                    optional_args,
                }) => {
                    stack.push(factory.create_compiled_function_term(
                        target_address,
                        hash,
                        required_args,
                        optional_args,
                    ));
                    Ok((ExecutionResult::Advance, DependencyList::empty()))
                }
                _ => Err(format!(
                    "Target address {:x} is not a function",
                    target_address
                )),
            }
        }
        Instruction::PushBuiltin { target } => {
            trace!(instruction = "Instruction::PushBuiltin", builtin_target = %target);
            let uid = *target;
            let builtin: Result<T::Builtin, ()> = uid.try_into();
            match builtin {
                Ok(target) => {
                    stack.push(factory.create_builtin_term(target));
                    Ok((ExecutionResult::Advance, DependencyList::empty()))
                }
                Err(_) => Err(format!("Invalid builtin function id: {}", uid)),
            }
        }
        Instruction::PushSignal {
            signal_type,
            num_args,
        } => {
            trace!(instruction = "Instruction::PushSignal");
            let num_args = *num_args;
            if stack.len() < num_args {
                Err(format!(
                    "Unable to create {} signal: insufficient arguments on stack",
                    signal_type,
                ))
            } else {
                let args = stack.pop_multiple(num_args).into_iter();
                let signal = factory.create_signal_term(allocator.create_signal_list(once(
                    allocator.create_signal(signal_type.clone(), allocator.create_list(args)),
                )));
                stack.push(signal);
                Ok((ExecutionResult::Advance, DependencyList::empty()))
            }
        }
        Instruction::Pop { count } => {
            trace!(instruction = "Instruction::Pop");
            let count = *count;
            if stack.len() < count {
                Err(format!(
                    "Unable to pop {} stack items: insufficient arguments on stack",
                    count,
                ))
            } else {
                stack.pop_multiple(count);
                Ok((ExecutionResult::Advance, DependencyList::empty()))
            }
        }
        Instruction::Squash { depth } => {
            trace!(instruction = "Instruction::Squash");
            let depth = *depth;
            if stack.len() < depth + 1 {
                Err(String::from(
                    "Unable to squash stack: insufficient arguments on stack",
                ))
            } else {
                let value = stack.pop().unwrap();
                stack.pop_multiple(depth);
                stack.push(value);
                Ok((ExecutionResult::Advance, DependencyList::empty()))
            }
        }
        Instruction::Move { offset } => {
            trace!(instruction = "Instruction::Move");
            let offset = *offset;
            if stack.len() < offset + 1 {
                Err(String::from(
                    "Unable to move stack item: insufficient arguments on stack",
                ))
            } else {
                if offset > 0 {
                    let value = stack.pop().unwrap();
                    stack.update(offset - 1, value);
                }
                Ok((ExecutionResult::Advance, DependencyList::empty()))
            }
        }
        Instruction::Jump { target } => {
            trace!(instruction = "Instruction::Jump");
            let target_address = *target;
            Ok((
                ExecutionResult::Jump(target_address),
                DependencyList::empty(),
            ))
        }
        Instruction::Call { target, num_args } => {
            trace!(instruction = "Instruction::Call");
            let target_address = *target;
            let num_args = *num_args;
            let caller_address = call_stack.program_counter();
            let resume_address = call_stack.next_program_counter();
            Ok((
                ExecutionResult::CallFunction {
                    target_address,
                    num_args,
                    caller_address,
                    resume_address,
                },
                DependencyList::empty(),
            ))
        }
        Instruction::Apply { num_args } => {
            let num_args = *num_args;
            if stack.len() < num_args {
                Err(String::from(
                    "Unable to evaluate function application: insufficient arguments on stack",
                ))
            } else {
                let target = stack.pop().unwrap();
                if let Some((compiled_target, partial_args)) =
                    match_compiled_application_target(&target, factory)
                {
                    trace!(
                        instruction = "Instruction::Apply",
                        apply_metadata = %(if partial_args.is_empty() {
                            format!("{}", compiled_target)
                        } else {
                            format!("<partial:{}:[{};{}]>", compiled_target, partial_args.len(), hash_object(&partial_args))
                        }),
                    );
                    let target_address = compiled_target.address();
                    let caller_address = call_stack.program_counter();
                    let resume_address = call_stack.next_program_counter();
                    let num_combined_args = num_args + partial_args.len();
                    if !partial_args.is_empty() {
                        stack.splice(num_args, partial_args);
                    }
                    Ok((
                        ExecutionResult::CallFunction {
                            target_address,
                            num_args: num_combined_args,
                            caller_address,
                            resume_address,
                        },
                        DependencyList::empty(),
                    ))
                } else {
                    trace!(
                        instruction = "Instruction::Apply",
                        apply_metadata = %target,
                    );
                    let arity = get_function_arity(&target)?;
                    if has_unresolved_args(&arity, stack.slice(num_args)) {
                        let args = stack.pop_multiple(num_args);
                        Ok((
                            ExecutionResult::ResolveApplicationArgs {
                                target,
                                args: args.into_iter().collect(),
                                caller_address: call_stack.program_counter(),
                                resume_address: call_stack.next_program_counter(),
                            },
                            DependencyList::empty(),
                        ))
                    } else if let Some(signal) =
                        get_short_circuit_signal(stack.slice(num_args), &arity, factory, allocator)
                    {
                        stack.pop_multiple(num_args);
                        stack.push(signal);
                        Ok((ExecutionResult::Advance, DependencyList::empty()))
                    } else {
                        let args = stack.pop_multiple(num_args);
                        let result = apply_function(&target, args.into_iter(), factory, allocator)?;
                        stack.push(result);
                        Ok((ExecutionResult::Advance, DependencyList::empty()))
                    }
                }
            }
        }
        Instruction::Function { .. } => {
            trace!(instruction = "Instruction::Function");
            Ok((ExecutionResult::Advance, DependencyList::empty()))
        }
        Instruction::Return => {
            trace!(instruction = "Instruction::Return");
            Ok((ExecutionResult::Return, DependencyList::empty()))
        }
        Instruction::Evaluate => match stack.peek().map(|target| target.is_static()) {
            None => Err(String::from("Missing evaluation target")),
            Some(true) => {
                trace!(
                    instruction = "Instruction::Evaluate",
                    evaluate_metadata = "Static"
                );
                Ok((ExecutionResult::Advance, DependencyList::empty()))
            }
            Some(false) => {
                let expression = stack.pop().unwrap();
                trace!(
                    instruction = "Instruction::Evaluate",
                    evaluate_metadata = "Expression"
                );
                evaluate_expression(
                    &expression,
                    state,
                    stack,
                    call_stack,
                    factory,
                    allocator,
                    cache,
                    cache_entries,
                )
            }
        },
        Instruction::ConstructApplication { num_args } => {
            trace!(instruction = "Instruction::ConstructApplication");
            let num_args = *num_args;
            if stack.len() < (1 + num_args) {
                Err(String::from(
                    "Unable to create function application: insufficient arguments on stack",
                ))
            } else {
                let target = stack.pop().unwrap();
                let args = allocator.create_list(stack.pop_multiple(num_args));
                stack.push(factory.create_application_term(target, args));
                Ok((ExecutionResult::Advance, DependencyList::empty()))
            }
        }
        Instruction::ConstructPartialApplication { num_args } => {
            trace!(instruction = "Instruction::ConstructPartialApplication");
            let num_args = *num_args;
            if stack.len() < (1 + num_args) {
                Err(String::from(
                    "Unable to create partial application: insufficient arguments on stack",
                ))
            } else {
                let target = stack.pop().unwrap();
                let args = allocator.create_list(stack.pop_multiple(num_args));
                let expression =
                    factory.create_partial_application_term(target, allocator.create_list(args));
                stack.push(expression);
                Ok((ExecutionResult::Advance, DependencyList::empty()))
            }
        }
        Instruction::ConstructTuple { size } => {
            trace!(instruction = "Instruction::ConstructTuple");
            let size = *size;
            if stack.len() < size {
                Err(format!(
                    "Unable to create {}-item tuple: insufficient arguments on stack",
                    size,
                ))
            } else {
                let fields = allocator.create_list(stack.pop_multiple(size));
                stack.push(factory.create_tuple_term(fields));
                Ok((ExecutionResult::Advance, DependencyList::empty()))
            }
        }
        Instruction::PushConstructor { prototype } => {
            trace!(instruction = "Instruction::PushConstructor");
            stack.push(factory.create_constructor_term(prototype.clone()));
            Ok((ExecutionResult::Advance, DependencyList::empty()))
        }
        Instruction::ConstructVector { size } => {
            trace!(instruction = "Instruction::ConstructVector");
            let size = *size;
            if stack.len() < size {
                Err(format!(
                    "Unable to create {}-item vector: insufficient arguments on stack",
                    size,
                ))
            } else {
                let items = allocator.create_list(stack.pop_multiple(size));
                stack.push(factory.create_vector_term(items));
                Ok((ExecutionResult::Advance, DependencyList::empty()))
            }
        }
        Instruction::ConstructHashMap { size } => {
            trace!(instruction = "Instruction::ConstructHashMap");
            let size = *size;
            if stack.len() < size * 2 {
                Err(format!(
                    "Unable to create {}-item hashmap: insufficient arguments on stack",
                    size,
                ))
            } else {
                let values = allocator.create_list(stack.pop_multiple(size));
                let keys = allocator.create_list(stack.pop_multiple(size));
                stack.push(factory.create_hashmap_term(keys, values));
                Ok((ExecutionResult::Advance, DependencyList::empty()))
            }
        }
        Instruction::ConstructHashSet { size } => {
            trace!(instruction = "Instruction::ConstructHashSet");
            let size = *size;
            if stack.len() < size {
                Err(format!(
                    "Unable to create {}-item hashset: insufficient arguments on stack",
                    size,
                ))
            } else {
                let values = allocator.create_list(stack.pop_multiple(size));
                stack.push(factory.create_hashset_term(values));
                Ok((ExecutionResult::Advance, DependencyList::empty()))
            }
        }
        Instruction::CombineSignals { count } => {
            trace!(instruction = "Instruction::CombineSignals");
            let count = *count;
            if stack.len() < count {
                Err(format!(
                    "Unable to combine {} signals: insufficient arguments on stack",
                    count,
                ))
            } else {
                let signals = stack
                    .pop_multiple(count)
                    .into_iter()
                    .map(|arg| match factory.match_signal_term(&arg) {
                        Some(signal) => Ok(signal
                            .signals()
                            .iter()
                            .map(|signal| allocator.clone_signal(signal))
                            .collect::<Vec<_>>()),
                        None => Err(format!(
                            "Invalid combined signal: Expected <signal>, received {}",
                            arg
                        )),
                    })
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .flatten();
                stack.push(factory.create_signal_term(allocator.create_signal_list(signals)));
                Ok((ExecutionResult::Advance, DependencyList::empty()))
            }
        }
    }
}

fn evaluate_expression<T: Expression + Applicable<T>>(
    expression: &T,
    state: &impl DynamicState<T>,
    stack: &mut VariableStack<T>,
    call_stack: &CallStack<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    cache: &impl InterpreterCache<T>,
    cache_entries: &mut CacheEntries<T>,
) -> Result<(ExecutionResult<T>, DependencyList), String> {
    if expression.is_static() {
        stack.push(expression.clone());
        Ok((ExecutionResult::Advance, DependencyList::empty()))
    } else {
        let hash = expression.id();
        if let Some((cached_result, updated_cache_entry)) =
            retrieve_cached_result(&hash, state, cache, cache_entries)
        {
            if let Some(cache_entry) = updated_cache_entry {
                cache_entries.insert(hash, cache_entry, Vec::new())
            }
            let (result, dependencies) = cached_result.into_parts();
            stack.push(result);
            Ok((ExecutionResult::Advance, dependencies))
        } else if let Some(application) = factory.match_application_term(expression) {
            let target = application.target();
            let args = application.args();
            let (target, partial_args) = extract_partial_args(target, factory);
            let combined_args = combine_slices(partial_args.as_slice(), args.as_slice());
            if !target.is_static() {
                Ok((
                    ExecutionResult::ResolveApplicationTarget {
                        target: target.clone(),
                        args: if partial_args.len() > 0 {
                            allocator.create_list(combined_args.into_iter().cloned())
                        } else {
                            allocator.clone_list(args)
                        },
                    },
                    DependencyList::empty(),
                ))
            } else if let Some(_) = factory.match_signal_term(target) {
                stack.push(target.clone());
                Ok((ExecutionResult::Advance, DependencyList::empty()))
            } else if let Some(compiled_function) = factory.match_compiled_function_term(target) {
                let target_address = compiled_function.address();
                let caller_address = call_stack.program_counter();
                let resume_address = call_stack.program_counter();
                let num_args = combined_args.len();
                stack.push_multiple(combined_args.into_iter().cloned());
                Ok((
                    ExecutionResult::CallFunction {
                        target_address,
                        num_args,
                        caller_address,
                        resume_address,
                    },
                    DependencyList::empty(),
                ))
            } else {
                let arity = get_function_arity(target)?;
                if has_unresolved_args(&arity, combined_args.iter()) {
                    Ok((
                        ExecutionResult::ResolveApplicationArgs {
                            target: target.clone(),
                            args: combined_args.into_iter().cloned().collect(),
                            caller_address: call_stack.program_counter(),
                            resume_address: call_stack.program_counter(),
                        },
                        DependencyList::empty(),
                    ))
                } else {
                    if let Some(signal) = get_combined_short_circuit_signal(
                        get_num_short_circuit_signals(combined_args.iter(), &arity, factory),
                        combined_args.iter(),
                        &arity,
                        factory,
                        allocator,
                    ) {
                        stack.push(signal);
                        Ok((ExecutionResult::Advance, DependencyList::empty()))
                    } else {
                        let result = apply_function(
                            target,
                            combined_args.into_iter().cloned(),
                            factory,
                            allocator,
                        )?;
                        stack.push(result);
                        Ok((ExecutionResult::ResolveExpression, DependencyList::empty()))
                    }
                }
            }
        } else if let Some(expression) = factory.match_static_variable_term(expression) {
            match stack.get(expression.offset()).cloned() {
                None => Err(format!("Invalid stack offset: {}", expression.offset())),
                Some(value) => {
                    stack.push(value);
                    Ok((ExecutionResult::ResolveExpression, DependencyList::empty()))
                }
            }
        } else if let Some(expression) = factory.match_dynamic_variable_term(expression) {
            stack.push(
                state
                    .get(&expression.state_token())
                    .unwrap_or(expression.fallback())
                    .clone(),
            );
            Ok((
                ExecutionResult::ResolveExpression,
                DependencyList::of(expression.state_token()),
            ))
        } else if let Some(term) = factory.match_recursive_term(expression) {
            stack.push(factory.create_application_term(
                term.factory().clone(),
                allocator.create_unit_list(expression.clone()),
            ));
            Ok((ExecutionResult::ResolveExpression, DependencyList::empty()))
        } else {
            Err(format!("Unexpected evaluation target: {}", expression))
        }
    }
}

fn get_function_arity<T: Expression + Applicable<T>>(target: &T) -> Result<Arity, String> {
    target
        .arity()
        .ok_or_else(|| format!("Invalid function application target: {}", target))
}

fn has_unresolved_args<'a, T: Expression + 'a>(
    arity: &Arity,
    args: impl IntoIterator<Item = &'a T>,
) -> bool {
    args.into_iter()
        .zip(arity.iter())
        .any(|(arg, arg_type)| match arg_type {
            ArgType::Lazy => false,
            ArgType::Strict | ArgType::Eager => !arg.is_static(),
        })
}

fn apply_function<T: Expression + Applicable<T>>(
    target: &T,
    args: impl ExactSizeIterator<Item = T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, String> {
    let arity = get_function_arity(target)?;
    let num_args = validate_function_application_arity(target, &arity, args.len())?;
    let num_required_args = arity.required().len();
    let num_optional_args = arity.optional().len();
    let num_positional_args = num_required_args + num_optional_args;
    let num_unspecified_optional_args = num_positional_args.saturating_sub(num_args);
    let args = WithExactSizeIterator::new(
        num_args + num_unspecified_optional_args,
        args.take(num_args).chain(
            (0..num_unspecified_optional_args).map(|_| factory.create_value_term(ValueTerm::Null)),
        ),
    );
    // TODO: distinguish between type errors vs runtime errors, and wrap runtime errors as signals
    target.apply(args, factory, allocator, &mut NoopCache::default())
}

fn process_next_list_item<T: Expression + Applicable<T>>(
    stack: &mut VariableStack<T>,
    call_stack: &mut CallStack<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<(), String> {
    let (
        target,
        arity,
        caller_address,
        resume_address,
        args,
        results,
        dependencies,
        subexpressions,
    ) = call_stack.pop_application_arg_list_stack().unwrap();
    call_stack.add_state_dependencies(dependencies);
    call_stack.add_subexpressions(subexpressions);
    let resolved_item = stack.pop().unwrap();
    if !resolved_item.is_static() {
        let next_arg = resolved_item;
        call_stack.enter_application_arg_list(
            target,
            arity,
            caller_address,
            resume_address,
            args,
            results,
        );
        let expression_hash = next_arg.id();
        stack.push(next_arg);
        call_stack.enter_expression(expression_hash);
    } else {
        let mut results = results;
        results.push(resolved_item);
        let results = fill_resolved_args(&args, &arity, results);
        if results.len() < args.len() {
            let next_arg = args.get(results.len()).unwrap().clone();
            call_stack.enter_application_arg_list(
                target,
                arity,
                caller_address,
                resume_address,
                args,
                results,
            );
            let expression_hash = next_arg.id();
            stack.push(next_arg);
            call_stack.enter_expression(expression_hash);
        } else if let Some(signal) = get_short_circuit_signal(&results, &arity, factory, allocator)
        {
            stack.push(signal);
            call_stack.update_program_counter(resume_address);
        } else {
            let result = apply_function(&target, results.into_iter(), factory, allocator)?;
            stack.push(result);
            call_stack.update_program_counter(resume_address);
        }
    }
    Ok(())
}

fn fill_resolved_args<T: Expression>(args: &[T], arity: &Arity, results: Vec<T>) -> Vec<T> {
    let num_resolved_args = results.len();
    let mut results = results;
    for (arg, arg_type) in args[num_resolved_args..]
        .iter()
        .zip(arity.partial(num_resolved_args).iter())
    {
        let is_resolved = match arg_type {
            ArgType::Lazy => true,
            ArgType::Strict | ArgType::Eager => arg.is_static(),
        };
        if !is_resolved {
            break;
        }
        results.push(arg.clone());
    }
    results
}

fn retrieve_cached_result<T: Expression>(
    hash: &HashId,
    state: &impl DynamicState<T>,
    cache: &impl InterpreterCache<T>,
    cache_entries: &CacheEntries<T>,
) -> Option<(EvaluationResult<T>, Option<InterpreterCacheEntry<T>>)> {
    cache_entries
        .get(hash)
        .map(|(entry, _subexpressions)| (entry.result().clone(), None))
        .or_else(|| cache.retrieve_result(hash, state))
}

fn is_evaluate_instruction(instruction: &Instruction) -> bool {
    match instruction {
        Instruction::Evaluate => true,
        _ => false,
    }
}

fn match_compiled_application_target<'a, T: Expression>(
    target: &'a T,
    factory: &impl ExpressionFactory<T>,
) -> Option<(&'a CompiledFunctionTerm, Vec<T>)> {
    let (target, partial_args) = extract_partial_args(target, factory);
    match factory.match_compiled_function_term(target) {
        Some(target) => Some((target, partial_args)),
        None => None,
    }
}

fn extract_partial_args<'a, T: Expression>(
    target: &'a T,
    factory: &impl ExpressionFactory<T>,
) -> (&'a T, Vec<T>) {
    let mut partial_args = Vec::new();
    let mut current = target;
    while let Some(partial) = factory.match_partial_application_term(current) {
        partial_args.extend(partial.args().iter().cloned());
        current = partial.target();
    }
    (current, partial_args)
}

fn generate_function_call_hash<'a, T: Expression + 'a>(
    target: &impl std::hash::Hash,
    args: impl IntoIterator<Item = &'a T>,
) -> HashId {
    let mut hasher = DefaultHasher::default();
    target.hash(&mut hasher);
    for arg in args {
        arg.hash(&mut hasher);
    }
    hasher.finish()
}

fn format_current_instruction<T: Expression>(call_stack: &CallStack<T>) -> String {
    format!(
        "> {:x} {}",
        call_stack.program_counter(),
        call_stack
            .current_instruction()
            .map(|instruction| format!("{:?}", instruction))
            .unwrap_or_else(|| String::from("---"))
    )
}

fn format_current_stack<T: Expression>(stack: &VariableStack<T>) -> String {
    format!(
        "{}",
        stack
            .values()
            .iter()
            .rev()
            .enumerate()
            .map(|(offset, value)| format!("  {}: {}", offset, value))
            .collect::<Vec<_>>()
            .join("\n")
    )
}

fn combine_slices<'a, T>(left: &'a [T], right: &'a [T]) -> CombinedSlice<'a, T> {
    CombinedSlice { left, right }
}
struct CombinedSlice<'a, T> {
    left: &'a [T],
    right: &'a [T],
}
impl<'a, T> CombinedSlice<'a, T> {
    fn len(&self) -> usize {
        self.left.len() + self.right.len()
    }
    fn iter(&self) -> CombinedSliceIterator<'a, T> {
        CombinedSliceIterator {
            left: self.left,
            right: self.right,
            index: 0,
        }
    }
}
impl<'a, T> IntoIterator for CombinedSlice<'a, T> {
    type Item = &'a T;
    type IntoIter = CombinedSliceIterator<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}
struct CombinedSliceIterator<'a, T> {
    left: &'a [T],
    right: &'a [T],
    index: usize,
}
impl<'a, T> Iterator for CombinedSliceIterator<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        let left_len = self.left.len();
        if self.index < left_len {
            let result = self.left.get(self.index);
            self.index += 1;
            result
        } else if self.index - left_len < self.right.len() {
            let result = self.right.get(self.index - left_len);
            self.index += 1;
            result
        } else {
            None
        }
    }
}
impl<'a, T> ExactSizeIterator for CombinedSliceIterator<'a, T> {
    fn len(&self) -> usize {
        self.left.len() + self.right.len()
    }
}

struct WithExactSizeIterator<T: Iterator> {
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
        let len = ExactSizeIterator::len(self);
        (len, Some(len))
    }
    fn count(self) -> usize
    where
        Self: Sized,
    {
        let len = ExactSizeIterator::len(&self);
        len
    }
}
impl<T: Iterator> ExactSizeIterator for WithExactSizeIterator<T> {
    fn len(&self) -> usize {
        self.remaining
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        allocator::DefaultAllocator,
        compiler::{hash_program_root, Compiler, CompilerMode, CompilerOptions},
        core::{DependencyList, SignalType, StateCache, Uid},
        lang::*,
        parser::sexpr::parse,
        stdlib::Stdlib,
    };

    use super::*;

    #[test]
    fn match_compiled_functions() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let compiled_function = CompiledFunctionTerm::new(InstructionPointer::default(), 0, 0, 0);
        let expression = factory.create_partial_application_term(
            factory.create_partial_application_term(
                factory.create_partial_application_term(
                    factory.create_compiled_function_term(
                        compiled_function.address(),
                        compiled_function.hash(),
                        compiled_function.required_args(),
                        compiled_function.optional_args(),
                    ),
                    allocator.create_unit_list(factory.create_value_term(ValueTerm::Int(5))),
                ),
                allocator.create_unit_list(factory.create_value_term(ValueTerm::Int(4))),
            ),
            allocator.create_unit_list(factory.create_value_term(ValueTerm::Int(3))),
        );
        assert_eq!(
            match_compiled_application_target(&expression, &factory),
            Some((
                &compiled_function,
                vec![
                    factory.create_value_term(ValueTerm::Int(3)),
                    factory.create_value_term(ValueTerm::Int(4)),
                    factory.create_value_term(ValueTerm::Int(5))
                ],
            ))
        )
    }

    #[test]
    fn compiled_functions() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let target = parse("(lambda (foo bar) (+ foo bar))", &factory, &allocator).unwrap();
        let state_token = 345;
        let expression = factory.create_application_term(
            target,
            allocator.create_pair(
                factory.create_value_term(ValueTerm::Int(3)),
                factory.create_dynamic_variable_term(
                    state_token,
                    factory.create_signal_term(allocator.create_signal_list(once(
                        allocator.create_signal(SignalType::Pending, allocator.create_empty_list()),
                    ))),
                ),
            ),
        );
        let compiler = Compiler::new(CompilerOptions::unoptimized(), None);
        let program = compiler
            .compile(&expression, CompilerMode::Function, &factory, &allocator)
            .unwrap();
        let mut state = StateCache::default();
        let entry_point = InstructionPointer::default();
        let cache_key = hash_program_root(&program, &entry_point);
        let (result, _) = execute(
            cache_key,
            &program,
            entry_point,
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
                factory.create_signal_term(allocator.create_signal_list(once(
                    allocator.create_signal(SignalType::Pending, allocator.create_empty_list()),
                ))),
                DependencyList::of(state_token),
            ),
        );

        state.set(state_token, factory.create_value_term(ValueTerm::Int(4)));
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
                DependencyList::of(state_token),
            ),
        );
    }

    #[test]
    fn compiled_tail_calls() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let expression = parse(
            "((lambda (foo bar) ((lambda (baz) (+ foo baz)) bar)) 3 4)",
            &factory,
            &allocator,
        )
        .unwrap();
        let compiler = Compiler::new(CompilerOptions::unoptimized(), None);
        let program = compiler
            .compile(&expression, CompilerMode::Function, &factory, &allocator)
            .unwrap();
        let entry_point = InstructionPointer::default();
        let cache_key = hash_program_root(&program, &entry_point);
        let (result, _) = execute(
            cache_key,
            &program,
            entry_point,
            &mut StateCache::default(),
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
    }

    #[test]
    fn compiled_closures() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let expression = parse(
            "(((lambda (foo) (lambda (bar) (+ foo bar))) 3) 4)",
            &factory,
            &allocator,
        )
        .unwrap();
        let compiler = Compiler::new(CompilerOptions::unoptimized(), None);
        let program = compiler
            .compile(&expression, CompilerMode::Function, &factory, &allocator)
            .unwrap();
        let entry_point = InstructionPointer::default();
        let cache_key = hash_program_root(&program, &entry_point);
        let (result, _) = execute(
            cache_key,
            &program,
            entry_point,
            &mut StateCache::default(),
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
    }

    #[test]
    fn basic_operation() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let program = Program::new(vec![
            Instruction::Function {
                hash: 0,
                required_args: 0,
                optional_args: 0,
            },
            Instruction::PushInt { value: 3 },
            Instruction::Evaluate,
            Instruction::Return,
        ]);
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

    #[test]
    fn nested_expressions() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let program = Program::new(vec![
            Instruction::Function {
                hash: 0,
                required_args: 0,
                optional_args: 0,
            },
            Instruction::PushInt { value: 5 },
            Instruction::PushInt { value: 4 },
            Instruction::PushInt { value: -3 },
            Instruction::PushBuiltin {
                target: Uid::uid(&Stdlib::Abs {}),
            },
            Instruction::Apply { num_args: 1 },
            Instruction::Evaluate,
            Instruction::PushBuiltin {
                target: Uid::uid(&Stdlib::Add {}),
            },
            Instruction::Apply { num_args: 2 },
            Instruction::Evaluate,
            Instruction::PushBuiltin {
                target: Uid::uid(&Stdlib::Add {}),
            },
            Instruction::Apply { num_args: 2 },
            Instruction::Evaluate,
            Instruction::Return,
        ]);
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
                factory.create_value_term(ValueTerm::Int(3 + 4 + 5)),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn builtin_function_applications() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let expression = factory.create_application_term(
            factory.create_builtin_term(Stdlib::Add),
            allocator.create_pair(
                factory.create_value_term(ValueTerm::Int(3)),
                factory.create_value_term(ValueTerm::Int(4)),
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

        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let expression = factory.create_application_term(
            factory.create_builtin_term(Stdlib::If),
            allocator.create_triple(
                factory.create_value_term(ValueTerm::Boolean(true)),
                factory.create_value_term(ValueTerm::Int(3)),
                factory.create_value_term(ValueTerm::Int(4)),
            ),
        );
        let program = Compiler::new(CompilerOptions::unoptimized(), None)
            .compile(&expression, CompilerMode::Function, &factory, &allocator)
            .unwrap();
        let state = StateCache::default();
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

        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let expression = factory.create_application_term(
            factory.create_builtin_term(Stdlib::If),
            allocator.create_triple(
                factory.create_value_term(ValueTerm::Boolean(false)),
                factory.create_value_term(ValueTerm::Int(3)),
                factory.create_value_term(ValueTerm::Int(4)),
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
                factory.create_value_term(ValueTerm::Int(4)),
                DependencyList::empty(),
            ),
        );

        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let expression = factory.create_application_term(
            factory.create_builtin_term(Stdlib::If),
            allocator.create_triple(
                factory.create_application_term(
                    factory.create_builtin_term(Stdlib::And),
                    allocator.create_pair(
                        factory.create_value_term(ValueTerm::Boolean(true)),
                        factory.create_value_term(ValueTerm::Boolean(true)),
                    ),
                ),
                factory.create_value_term(ValueTerm::Int(3)),
                factory.create_value_term(ValueTerm::Int(4)),
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

        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let expression = factory.create_application_term(
            factory.create_builtin_term(Stdlib::If),
            allocator.create_triple(
                factory.create_application_term(
                    factory.create_builtin_term(Stdlib::And),
                    allocator.create_pair(
                        factory.create_value_term(ValueTerm::Boolean(true)),
                        factory.create_value_term(ValueTerm::Boolean(false)),
                    ),
                ),
                factory.create_value_term(ValueTerm::Int(3)),
                factory.create_value_term(ValueTerm::Int(4)),
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
                factory.create_value_term(ValueTerm::Int(4)),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn chained_functions() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let expression = factory.create_application_term(
            factory.create_builtin_term(Stdlib::Add),
            allocator.create_pair(
                factory.create_application_term(
                    factory.create_builtin_term(Stdlib::Add),
                    allocator.create_pair(
                        factory.create_value_term(ValueTerm::Int(3)),
                        factory.create_value_term(ValueTerm::Int(4)),
                    ),
                ),
                factory.create_value_term(ValueTerm::Int(5)),
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
                factory.create_value_term(ValueTerm::Int(3 + 4 + 5)),
                DependencyList::empty(),
            ),
        );

        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let expression = factory.create_application_term(
            factory.create_builtin_term(Stdlib::If),
            allocator.create_triple(
                factory.create_application_term(
                    factory.create_builtin_term(Stdlib::And),
                    allocator.create_pair(
                        factory.create_value_term(ValueTerm::Boolean(true)),
                        factory.create_value_term(ValueTerm::Boolean(false)),
                    ),
                ),
                factory.create_value_term(ValueTerm::Int(3)),
                factory.create_value_term(ValueTerm::Int(4)),
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
                factory.create_value_term(ValueTerm::Int(4)),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn variadic_functions() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let expression = factory.create_application_term(
            factory.create_builtin_term(Stdlib::Append),
            allocator.create_triple(
                factory.create_vector_term(allocator.create_list(vec![
                    factory.create_value_term(ValueTerm::Int(1)),
                    factory.create_value_term(ValueTerm::Int(2)),
                    factory.create_value_term(ValueTerm::Int(3)),
                ])),
                factory.create_vector_term(allocator.create_list(vec![
                    factory.create_value_term(ValueTerm::Int(4)),
                    factory.create_value_term(ValueTerm::Int(5)),
                    factory.create_value_term(ValueTerm::Int(6)),
                ])),
                factory.create_vector_term(allocator.create_list(vec![
                    factory.create_value_term(ValueTerm::Int(7)),
                    factory.create_value_term(ValueTerm::Int(8)),
                    factory.create_value_term(ValueTerm::Int(9)),
                ])),
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
                factory.create_vector_term(allocator.create_list(vec![
                    factory.create_value_term(ValueTerm::Int(1)),
                    factory.create_value_term(ValueTerm::Int(2)),
                    factory.create_value_term(ValueTerm::Int(3)),
                    factory.create_value_term(ValueTerm::Int(4)),
                    factory.create_value_term(ValueTerm::Int(5)),
                    factory.create_value_term(ValueTerm::Int(6)),
                    factory.create_value_term(ValueTerm::Int(7)),
                    factory.create_value_term(ValueTerm::Int(8)),
                    factory.create_value_term(ValueTerm::Int(9)),
                ])),
                DependencyList::empty(),
            ),
        );

        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let expression = factory.create_application_term(
            factory.create_builtin_term(Stdlib::Append),
            allocator.create_triple(
                factory.create_application_term(
                    factory.create_lambda_term(
                        0,
                        factory.create_vector_term(allocator.create_list(vec![
                            factory.create_value_term(ValueTerm::Int(1)),
                            factory.create_value_term(ValueTerm::Int(2)),
                            factory.create_value_term(ValueTerm::Int(3)),
                        ])),
                    ),
                    allocator.create_empty_list(),
                ),
                factory.create_application_term(
                    factory.create_lambda_term(
                        0,
                        factory.create_vector_term(allocator.create_list(vec![
                            factory.create_value_term(ValueTerm::Int(4)),
                            factory.create_value_term(ValueTerm::Int(5)),
                            factory.create_value_term(ValueTerm::Int(6)),
                        ])),
                    ),
                    allocator.create_empty_list(),
                ),
                factory.create_application_term(
                    factory.create_lambda_term(
                        0,
                        factory.create_vector_term(allocator.create_list(vec![
                            factory.create_value_term(ValueTerm::Int(7)),
                            factory.create_value_term(ValueTerm::Int(8)),
                            factory.create_value_term(ValueTerm::Int(9)),
                        ])),
                    ),
                    allocator.create_empty_list(),
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
                factory.create_vector_term(allocator.create_list(vec![
                    factory.create_value_term(ValueTerm::Int(1)),
                    factory.create_value_term(ValueTerm::Int(2)),
                    factory.create_value_term(ValueTerm::Int(3)),
                    factory.create_value_term(ValueTerm::Int(4)),
                    factory.create_value_term(ValueTerm::Int(5)),
                    factory.create_value_term(ValueTerm::Int(6)),
                    factory.create_value_term(ValueTerm::Int(7)),
                    factory.create_value_term(ValueTerm::Int(8)),
                    factory.create_value_term(ValueTerm::Int(9)),
                ])),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn let_expression_scoping() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
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
