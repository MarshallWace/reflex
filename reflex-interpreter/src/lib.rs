// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::convert::TryInto;
use std::{hash::Hasher, iter::once};

use rayon::prelude::*;
use tracing::info_span;
use tracing::trace;

use interpreter::cache::MultithreadedCacheEntries;
pub use interpreter::cache::{
    DefaultInterpreterCache, GcMetrics, InterpreterCache, InterpreterCacheEntry, LocalCacheEntries,
    MutableInterpreterCache,
};
pub use interpreter::stack::{CallStack, VariableStack};

use reflex::core::{
    ApplicationTermType, CompiledFunctionTermType, ConditionListType, ConditionType,
    EffectTermType, InstructionPointer, PartialApplicationTermType, RecursiveTermType, RefType,
    SignalTermType, StateCache, VariableTermType,
};
use reflex::hash::{hash_iter, FnvHasher, IntSet};
use reflex::{
    cache::NoopCache,
    core::{
        get_combined_short_circuit_signal, get_num_short_circuit_signals, get_short_circuit_signal,
        validate_function_application_arity, Applicable, ArgType, Arity, DependencyList,
        DynamicState, EvaluationResult, Expression, ExpressionFactory, ExpressionListType,
        HeapAllocator, Reducible, Rewritable,
    },
    hash::HashId,
};
use serde::{Deserialize, Serialize};

use crate::compiler::{CompiledProgram, Instruction, Program};

pub mod compiler;
mod interpreter {
    pub(crate) mod cache;
    pub(crate) mod stack;
}
pub(crate) mod term;

#[derive(Debug)]
enum ExecutionResult<T: Expression> {
    Advance,
    Jump(InstructionPointer),
    CallFunction {
        target_address: InstructionPointer,
        target_hash: HashId,
        num_args: usize,
        caller_address: InstructionPointer,
        resume_address: InstructionPointer,
    },
    Return,
    ResolveExpression,
    ResolveApplicationTarget {
        target: T,
        args: T::ExpressionList,
    },
    ResolveApplicationArgs {
        target: T,
        args: Vec<T>,
        caller_address: InstructionPointer,
        resume_address: InstructionPointer,
    },
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
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

fn evaluate_data_section<'a, T: Expression + Rewritable<T> + Reducible<T> + Applicable<T>>(
    value_factories: impl IntoIterator<Item = (HashId, &'a Program)>,
    function_lookup: &Program,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    options: &InterpreterOptions,
) -> Result<Vec<T>, String> {
    value_factories
        .into_iter()
        .try_fold(vec![], |mut static_data, (_, value_factory)| {
            let value = evaluate_static_program(
                value_factory,
                Default::default(),
                &static_data,
                Some(function_lookup),
                factory,
                allocator,
                options,
            )?;
            static_data.push(value);
            Ok(static_data)
        })
}

fn evaluate_static_program<T: Expression + Rewritable<T> + Reducible<T> + Applicable<T>>(
    program: &Program,
    entry_point: InstructionPointer,
    static_data: &[T],
    function_lookup: Option<&Program>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    options: &InterpreterOptions,
) -> Result<T, String> {
    let mut stack = VariableStack::new(options.variable_stack_size);
    let mut call_stack = CallStack::new(program, entry_point, options.call_stack_size);
    evaluate_program_loop(
        Default::default(),
        &StateCache::default(),
        &mut stack,
        &mut call_stack,
        static_data,
        function_lookup,
        factory,
        allocator,
        &mut DefaultInterpreterCache::default(),
        &mut MultithreadedCacheEntries::default(),
        options,
    )
}

pub fn execute<T: Expression + Rewritable<T> + Reducible<T> + Applicable<T>>(
    cache_key: HashId,
    program: &CompiledProgram,
    entry_point: InstructionPointer,
    state_id: usize,
    state: &impl DynamicState<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    options: &InterpreterOptions,
    cache: &impl InterpreterCache<T>,
) -> Result<(EvaluationResult<T>, LocalCacheEntries<T>), String> {
    let CompiledProgram {
        instructions,
        data_section,
    } = program;

    let execution_span = info_span!("interpreter::data");
    let execution_span = execution_span.enter();

    let static_data = evaluate_data_section(
        data_section
            .iter()
            .map(|(hash_id, instructions)| (*hash_id, instructions)),
        instructions,
        factory,
        allocator,
        options,
    )?;

    std::mem::drop(execution_span);

    match instructions.get(entry_point) {
        Some(&Instruction::Function { required_args, .. }) if required_args == 0 => Ok(()),
        Some(instruction) => Err(format!(
            "Invalid entry point function: {:x} {:?}",
            entry_point, instruction
        )),
        _ => Err(format!("Invalid entry point address: {:x}", entry_point)),
    }?;
    let execution_span = info_span!("interpreter::execute");
    let execution_span = execution_span.enter();
    let mut stack = VariableStack::new(options.variable_stack_size);
    let mut call_stack = CallStack::new(instructions, entry_point, options.call_stack_size);
    let mut evaluation_cache_entries = MultithreadedCacheEntries::new(state);
    let result = evaluate_program_loop(
        state_id,
        state,
        &mut stack,
        &mut call_stack,
        &static_data,
        None,
        factory,
        allocator,
        cache,
        &mut evaluation_cache_entries,
        options,
    );

    std::mem::drop(execution_span);
    let mut cache_entries = evaluation_cache_entries.into_entries();
    match result {
        Err(error) => Err(error),
        Ok(value) => {
            // FIXME: Unwind call frame / variable stack on short-circuit signals
            // debug_assert!(
            //     call_stack.call_stack_depth() == 0,
            //     "{} frames remaining on call stack",
            //     call_stack.call_stack_depth(),
            // );
            // debug_assert!(
            //     stack.len() == 0,
            //     "{} values remaining on variable stack",
            //     stack.len()
            // );
            let (dependencies, subexpressions) = call_stack.drain();
            let result = EvaluationResult::new(value, dependencies);
            cache_entries.insert(InterpreterCacheEntry::new(
                cache_key,
                result.clone(),
                state_id,
                state,
                subexpressions,
            ));
            Ok((result, cache_entries))
        }
    }
}

fn evaluate_program_loop<'a, T: Expression + Rewritable<T> + Reducible<T> + Applicable<T>>(
    state_id: usize,
    state: &impl DynamicState<T>,
    stack: &mut VariableStack<T>,
    call_stack: &mut CallStack<T>,
    static_data: &[T],
    function_lookup: Option<&Program>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    cache: &impl InterpreterCache<T>,
    cache_entries: &mut MultithreadedCacheEntries<'a, T>,
    options: &InterpreterOptions,
) -> Result<T, String> {
    loop {
        let result = match call_stack.lookup_instruction(call_stack.program_counter()) {
            None => Err(format!(
                "Invalid program instruction offset: {:x}",
                call_stack.program_counter(),
            )),
            Some(instruction) => {
                if options.debug_instructions {
                    eprintln!("{}", format_current_instruction(call_stack));
                }
                if options.debug_stack {
                    eprintln!("{}", format_current_stack(stack))
                }
                evaluate_instruction(
                    instruction,
                    state,
                    stack,
                    call_stack,
                    static_data,
                    function_lookup,
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
                        caller_address,
                        args,
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
                                cache_entries.insert(InterpreterCacheEntry::new(
                                    hash,
                                    EvaluationResult::new(value.clone(), dependencies),
                                    state_id,
                                    state,
                                    subexpressions,
                                ));
                            }
                            if let Some((args, dependencies, subexpressions)) =
                                call_stack.pop_application_target_stack()
                            {
                                call_stack.add_state_dependencies(dependencies);
                                call_stack.add_subexpressions(subexpressions);
                                let resolved_target = stack.pop().unwrap();
                                stack.push(factory.create_application_term(resolved_target, args));
                                continue;
                            } else if let Some((dependencies, subexpressions)) =
                                call_stack.pop_application_arg_stack()
                            {
                                call_stack.add_state_dependencies(dependencies);
                                call_stack.add_subexpressions(subexpressions);
                                return Ok(stack.pop().unwrap());
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
                        target_hash,
                        caller_address,
                        resume_address,
                    } => {
                        if let Some((num_required_args, num_optional_args)) = call_stack
                            .lookup_instruction(target_address)
                            .and_then(|target| match target {
                                &Instruction::Function {
                                    required_args,
                                    optional_args,
                                    ..
                                } => Some((required_args, optional_args)),
                                _ => None,
                            })
                        {
                            let hash =
                                generate_function_call_hash(&target_hash, stack.slice(num_args));
                            if let Some(result) =
                                retrieve_cached_result(hash, state, cache, cache_entries)
                            {
                                let (result, dependencies) = result.into_parts();
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
                                        .map(|_| factory.create_nil_term()),
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
                            cache_entries.insert(InterpreterCacheEntry::new(
                                hash,
                                EvaluationResult::new(value.clone(), dependencies),
                                state_id,
                                state,
                                subexpressions,
                            ));
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

fn evaluate_instruction<'a, T: Expression + Rewritable<T> + Reducible<T> + Applicable<T>>(
    instruction: &Instruction,
    state: &impl DynamicState<T>,
    stack: &mut VariableStack<T>,
    call_stack: &CallStack<T>,
    static_data: &[T],
    function_lookup: Option<&Program>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    cache: &impl InterpreterCache<T>,
    cache_entries: &mut MultithreadedCacheEntries<'a, T>,
) -> Result<(ExecutionResult<T>, DependencyList), String> {
    match instruction {
        Instruction::LoadStaticData { offset } => {
            trace!(instruction = "Instruction::LoadStaticData");
            let value = static_data.get(*offset).cloned();
            match value {
                None => Err(format!("Invalid stack offset: {}", offset)),
                Some(value) => {
                    stack.push(value);
                    Ok((ExecutionResult::Advance, DependencyList::empty()))
                }
            }
        }
        Instruction::PushLocal { offset } => {
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
        Instruction::PushNil => {
            trace!(instruction = "Instruction::PushNil");
            stack.push(factory.create_nil_term());
            Ok((ExecutionResult::Advance, DependencyList::empty()))
        }
        Instruction::PushBoolean { value } => {
            trace!(instruction = "Instruction::PushBoolean");
            stack.push(factory.create_boolean_term(*value));
            Ok((ExecutionResult::Advance, DependencyList::empty()))
        }
        Instruction::PushInt { value } => {
            trace!(instruction = "Instruction::PushInt");
            stack.push(factory.create_int_term(*value));
            Ok((ExecutionResult::Advance, DependencyList::empty()))
        }
        Instruction::PushFloat { value } => {
            trace!(instruction = "Instruction::PushFloat");
            stack.push(factory.create_float_term(f64::from(*value)));
            Ok((ExecutionResult::Advance, DependencyList::empty()))
        }
        Instruction::PushString { value } => {
            trace!(instruction = "Instruction::PushString");
            stack.push(factory.create_string_term(allocator.create_string(value.as_str())));
            Ok((ExecutionResult::Advance, DependencyList::empty()))
        }
        Instruction::PushSymbol { id: value } => {
            trace!(instruction = "Instruction::PushSymbol");
            stack.push(factory.create_symbol_term(*value));
            Ok((ExecutionResult::Advance, DependencyList::empty()))
        }
        Instruction::PushFunction { target, hash } => {
            trace!(instruction = "Instruction::PushFunction");
            let target_address = *target;
            let hash = *hash;
            match function_lookup
                .unwrap_or(call_stack.program())
                .get(target_address)
            {
                Some(Instruction::Function {
                    required_args,
                    optional_args,
                    ..
                }) => {
                    stack.push(factory.create_compiled_function_term(
                        target_address,
                        hash,
                        *required_args,
                        *optional_args,
                    ));
                    Ok((ExecutionResult::Advance, DependencyList::empty()))
                }
                _ => Err(format!(
                    "Invalid compiled function address: {:x}",
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
        Instruction::LoadEffect => {
            trace!(instruction = "Instruction::LoadEffect");
            let effect = stack.pop();
            match effect {
                None => Err(String::from("Missing effect condition")),
                Some(effect) => {
                    let condition = factory
                        .match_signal_term(&effect)
                        .filter(|effect| effect.signals().as_deref().len() == 1)
                        .and_then(|effect| {
                            effect
                                .signals()
                                .as_deref()
                                .iter()
                                .map(|item| item.as_deref())
                                .next()
                        });
                    match condition {
                        None => Err(format!("Invalid effect condition: {}", effect)),
                        Some(condition) => {
                            let value = match state.get(&condition.id()) {
                                Some(value) => value.clone(),
                                None => factory.create_signal_term(
                                    allocator.create_signal_list(once(condition.clone())),
                                ),
                            };
                            stack.push(value);
                            Ok((ExecutionResult::Advance, DependencyList::of(condition.id())))
                        }
                    }
                }
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
        Instruction::Call {
            target_address,
            target_hash,
            num_args,
        } => {
            trace!(instruction = "Instruction::Call");
            let target_address = *target_address;
            let target_hash = *target_hash;
            let num_args = *num_args;
            let caller_address = call_stack.program_counter();
            let resume_address = call_stack.next_program_counter();
            Ok((
                ExecutionResult::CallFunction {
                    target_address,
                    target_hash,
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
                if let Some(_) = factory.match_signal_term(&target) {
                    stack.pop_multiple(num_args);
                    stack.push(target);
                    Ok((ExecutionResult::Advance, DependencyList::empty()))
                } else if let Some((compiled_target, partial_args)) =
                    match_compiled_application_target(&target, factory)
                {
                    trace!(
                        instruction = "Instruction::Apply",
                        apply_metadata = %(if partial_args.is_empty() {
                            format!("{:x}", compiled_target.address())
                        } else {
                            format!("<partial:{:x}:[{};{}]>", compiled_target.address(), partial_args.len(), hash_iter(partial_args.iter().map(|arg| arg.id())))
                        }),
                    );
                    let target_address = compiled_target.address();
                    let target_hash = compiled_target.hash();
                    let caller_address = call_stack.program_counter();
                    let resume_address = call_stack.next_program_counter();
                    let num_combined_args = num_args + partial_args.len();
                    if !partial_args.is_empty() {
                        stack.splice(num_args, partial_args);
                    }
                    Ok((
                        ExecutionResult::CallFunction {
                            target_address,
                            target_hash,
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
                let expression = factory.create_partial_application_term(target, args);
                stack.push(expression);
                Ok((ExecutionResult::Advance, DependencyList::empty()))
            }
        }
        Instruction::ConstructConstructor { num_fields } => {
            trace!(instruction = "Instruction::ConstructConstructor");
            let num_fields = *num_fields;
            if stack.len() < num_fields {
                Err(String::from(
                    "Unable to create constructor: insufficient arguments on stack",
                ))
            } else {
                let keys = allocator.create_list(stack.pop_multiple(num_fields));
                stack
                    .push(factory.create_constructor_term(allocator.create_struct_prototype(keys)));
                Ok((ExecutionResult::Advance, DependencyList::empty()))
            }
        }
        Instruction::ConstructList { size } => {
            trace!(instruction = "Instruction::ConstructList");
            let size = *size;
            if stack.len() < size {
                Err(format!(
                    "Unable to create {}-item list: insufficient arguments on stack",
                    size,
                ))
            } else {
                let items = allocator.create_list(stack.pop_multiple(size));
                stack.push(factory.create_list_term(items));
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
                let values = stack.pop_multiple(size).into_iter().collect::<Vec<_>>();
                let keys = stack.pop_multiple(size).into_iter();
                let entries = keys.into_iter().zip(values);
                let instance = factory.create_hashmap_term(entries);
                stack.push(instance);
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
                let values = stack.pop_multiple(size);
                let instance = factory.create_hashset_term(values);
                stack.push(instance);
                Ok((ExecutionResult::Advance, DependencyList::empty()))
            }
        }
        Instruction::ConstructCondition { signal_type } => {
            trace!(instruction = "Instruction::ConstructCondition");
            match stack
                .pop()
                .and_then(|payload| stack.pop().map(|token| (payload, token)))
            {
                None => Err(format!(
                    "Unable to construct {} condition: insufficient arguments on stack",
                    signal_type,
                )),
                Some((payload, token)) => {
                    let signal = factory.create_signal_term(allocator.create_signal_list(once(
                        allocator.create_signal(signal_type.clone(), payload, token),
                    )));
                    stack.push(signal);
                    Ok((ExecutionResult::Advance, DependencyList::empty()))
                }
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
                            .as_deref()
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
    cache_entries: &mut impl MutableInterpreterCache<T>,
) -> Result<(ExecutionResult<T>, DependencyList), String> {
    if expression.is_static() {
        stack.push(expression.clone());
        Ok((ExecutionResult::Advance, DependencyList::empty()))
    } else {
        let hash = expression.id();
        if let Some(cached_result) = retrieve_cached_result(hash, state, cache, cache_entries) {
            let (result, dependencies) = cached_result.into_parts();
            stack.push(result);
            Ok((ExecutionResult::Advance, dependencies))
        } else if let Some(application) = factory.match_application_term(expression) {
            let target = application.target().as_deref();
            let args = application.args();
            let (target, partial_args) = extract_partial_args(target, factory);
            let num_combined_args = partial_args.len() + args.as_deref().len();
            let combined_args = partial_args
                .iter()
                .chain(args.as_deref().iter().map(|item| item.as_deref()))
                .cloned();
            if !target.is_static() {
                Ok((
                    ExecutionResult::ResolveApplicationTarget {
                        target: target.clone(),
                        args: if partial_args.len() > 0 {
                            allocator.create_sized_list(num_combined_args, combined_args)
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
                stack.push_multiple(combined_args);
                Ok((
                    ExecutionResult::CallFunction {
                        target_address,
                        target_hash: hash,
                        num_args: num_combined_args,
                        caller_address,
                        resume_address,
                    },
                    DependencyList::empty(),
                ))
            } else {
                let arity = get_function_arity(target)?;
                if has_unresolved_args(
                    &arity,
                    partial_args
                        .iter()
                        .chain(args.as_deref().iter().map(|item| item.as_deref())),
                ) {
                    Ok((
                        ExecutionResult::ResolveApplicationArgs {
                            target: target.clone(),
                            args: combined_args.collect(),
                            caller_address: call_stack.program_counter(),
                            resume_address: call_stack.program_counter(),
                        },
                        DependencyList::empty(),
                    ))
                } else {
                    if let Some(signal) = get_combined_short_circuit_signal(
                        get_num_short_circuit_signals(
                            partial_args
                                .iter()
                                .chain(args.as_deref().iter().map(|item| item.as_deref())),
                            &arity,
                            factory,
                        ),
                        partial_args
                            .iter()
                            .chain(args.as_deref().iter().map(|item| item.as_deref())),
                        &arity,
                        factory,
                        allocator,
                    ) {
                        stack.push(signal);
                        Ok((ExecutionResult::Advance, DependencyList::empty()))
                    } else {
                        let result = apply_function(
                            target,
                            WithExactSizeIterator::new(num_combined_args, combined_args),
                            factory,
                            allocator,
                        );
                        let result = result?;
                        stack.push(result);
                        Ok((ExecutionResult::ResolveExpression, DependencyList::empty()))
                    }
                }
            }
        } else if let Some(expression) = factory.match_variable_term(expression) {
            match stack.get(expression.offset()).cloned() {
                None => Err(format!("Invalid stack offset: {}", expression.offset())),
                Some(value) => {
                    stack.push(value);
                    Ok((ExecutionResult::ResolveExpression, DependencyList::empty()))
                }
            }
        } else if let Some(expression) = factory.match_effect_term(expression) {
            let condition = expression.condition().as_deref();
            let value = match state.get(&condition.id()) {
                Some(value) => value.clone(),
                None => factory
                    .create_signal_term(allocator.create_signal_list(once(condition.clone()))),
            };
            stack.push(value);
            Ok((
                ExecutionResult::ResolveExpression,
                DependencyList::of(condition.id()),
            ))
        } else if let Some(term) = factory.match_recursive_term(expression) {
            stack.push(factory.create_application_term(
                term.factory().as_deref().clone(),
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
        args.take(num_args)
            .chain((0..num_unspecified_optional_args).map(|_| factory.create_nil_term())),
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
    hash: HashId,
    state: &impl DynamicState<T>,
    cache: &impl InterpreterCache<T>,
    cache_entries: &mut impl MutableInterpreterCache<T>,
) -> Option<EvaluationResult<T>> {
    cache_entries
        .retrieve_result(hash, state)
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
) -> Option<(&'a T::CompiledFunctionTerm, Vec<T>)> {
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
        partial_args.extend(
            partial
                .args()
                .as_deref()
                .iter()
                .map(|item| item.as_deref())
                .cloned(),
        );
        current = partial.target().as_deref();
    }
    (current, partial_args)
}

fn generate_function_call_hash<'a, T: Expression + 'a>(
    target: &impl std::hash::Hash,
    args: impl IntoIterator<Item = &'a T>,
) -> HashId {
    let mut hasher = FnvHasher::default();
    target.hash(&mut hasher);
    for arg in args {
        std::hash::Hash::hash(&arg.id(), &mut hasher);
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

#[allow(dead_code)]
// Parallel interpreter execution deactivated indefinitely
//
// See commit bcfc57 for original implementation
//
// Usage:
//
// ```
// match result {
//     // ...
//     ExecutionResult::ResolveApplicationArgs {
//         target,
//         caller_address,
//         args,
//         resume_address,
//     } => {
//         if target.should_parallelize(&args) {
//             let resolved_args = resolve_parallel_args(
//                 state_id,
//                 state,
//                 call_stack,
//                 arity,
//                 args,
//                 stack,
//                 factory,
//                 allocator,
//                 debug_instructions,
//                 debug_stack,
//                 cache,
//                 cache_entries,
//             )?;
//             stack.push(
//                 if let Some(signal) = get_short_circuit_signal(
//                     &resolved_args,
//                     &arity,
//                     factory,
//                     allocator,
//                 ) {
//                     signal
//                 } else {
//                     apply_function(
//                         &target,
//                         resolved_args.into_iter(),
//                         factory,
//                         allocator,
//                     )?
//                 },
//             );
//             call_stack.update_program_counter(resume_address);
//         } else {
//             // ...
//         }
//     }
//     // ...
// }
// ```
fn resolve_parallel_args<
    'a,
    T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Send + Sync,
>(
    state_id: usize,
    state: &(impl DynamicState<T> + Sync),
    call_stack: &mut CallStack<T>,
    arity: Arity,
    args: Vec<T>,
    stack: &mut VariableStack<T>,
    static_data: &[T],
    function_lookup: Option<&Program>,
    factory: &(impl ExpressionFactory<T> + Sync),
    allocator: &(impl HeapAllocator<T> + Sync),
    cache: &(impl InterpreterCache<T> + Sync),
    cache_entries: &mut MultithreadedCacheEntries<'a, T>,
    options: &InterpreterOptions,
) -> Result<Vec<T>, String>
where
    T::ExpressionList: Sync,
{
    {
        // this inner block does the calculation for a single entry which warms up the cache
        let first_unresolved_eager_arg = args
            .iter()
            .zip(arity.iter())
            .find(|(arg, arg_type)| match arg_type {
                ArgType::Lazy => false,
                ArgType::Strict | ArgType::Eager => !arg.is_static(),
            })
            .map(|(arg, _)| arg);
        if let Some(arg) = first_unresolved_eager_arg {
            let _ = resolve_arg_within_new_stack(
                arg.clone(),
                state_id,
                state,
                call_stack,
                stack,
                static_data,
                function_lookup,
                factory,
                allocator,
                cache,
                cache_entries,
                options,
            );
        } else {
            return Ok(args);
        }
    }

    // TODO: only parallelize argument evaluation for non-static arguments
    let (results, dependencies, subexpressions, added_cache_entries) = args
        .into_iter()
        .zip(arity.iter())
        .collect::<Vec<_>>()
        .into_par_iter()
        .fold(
            || {
                (
                    Vec::new(),
                    DependencyList::empty(),
                    IntSet::default(),
                    LocalCacheEntries::new(state),
                )
            },
            |mut results, (arg, arg_type)| {
                let is_already_resolved = match arg_type {
                    ArgType::Lazy => true,
                    ArgType::Strict | ArgType::Eager => arg.is_static(),
                };
                if is_already_resolved {
                    let (combined_results, _, _, _) = &mut results;
                    combined_results.push(Ok(arg));
                    results
                } else {
                    let (
                        mut combined_results,
                        mut combined_dependencies,
                        mut combined_subexpressions,
                        combined_cache_entries,
                    ) = results;
                    let (result, dependencies, subexpressions, combined_cache_entries) = {
                        let mut local_cache_entries =
                            cache_entries.create_child(combined_cache_entries);
                        let (result, dependencies, subexpressions) = resolve_arg_within_new_stack(
                            arg,
                            state_id,
                            state,
                            call_stack,
                            stack,
                            static_data,
                            function_lookup,
                            factory,
                            allocator,
                            cache,
                            &mut local_cache_entries,
                            options,
                        );
                        (
                            result,
                            dependencies,
                            subexpressions,
                            local_cache_entries.into_entries(),
                        )
                    };
                    combined_results.push(result);
                    combined_dependencies.extend(dependencies);
                    combined_subexpressions.extend(subexpressions);
                    (
                        combined_results,
                        combined_dependencies,
                        combined_subexpressions,
                        combined_cache_entries,
                    )
                }
            },
        )
        .reduce(
            || {
                (
                    Vec::new(),
                    DependencyList::empty(),
                    IntSet::default(),
                    LocalCacheEntries::new(state),
                )
            },
            |mut results, subtask_output| {
                let (
                    combined_results,
                    combined_dependencies,
                    combined_subexpressions,
                    combined_cache_entries,
                ) = &mut results;
                let (
                    subtask_results,
                    subtask_dependencies,
                    subtask_subexpressions,
                    subtask_cache_entries,
                ) = subtask_output;
                combined_results.extend(subtask_results);
                combined_dependencies.extend(subtask_dependencies);
                combined_subexpressions.extend(subtask_subexpressions);
                combined_cache_entries.extend(subtask_cache_entries);
                results
            },
        );
    call_stack.add_state_dependencies(dependencies);
    call_stack.add_subexpressions(subexpressions);
    cache_entries.extend(added_cache_entries);
    results.into_iter().collect::<Result<Vec<_>, _>>()
}

fn resolve_arg_within_new_stack<
    'a,
    T: Expression + Rewritable<T> + Reducible<T> + Applicable<T>,
>(
    arg: T,
    state_id: usize,
    state: &impl DynamicState<T>,
    call_stack: &CallStack<T>,
    stack: &VariableStack<T>,
    static_data: &[T],
    function_lookup: Option<&Program>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    cache: &impl InterpreterCache<T>,
    cache_entries: &mut MultithreadedCacheEntries<'a, T>,
    options: &InterpreterOptions,
) -> (Result<T, String>, DependencyList, Vec<HashId>) {
    let capture_depth = arg.capture_depth();
    let mut new_stack = stack.clone_shallow(capture_depth);
    let mut new_call_stack = CallStack::new(
        call_stack.program(),
        call_stack.evaluate_arg_program_counter(),
        None,
    );
    new_call_stack.enter_application_arg();
    new_call_stack.enter_expression(arg.id());
    new_stack.push(arg);
    let result = evaluate_program_loop(
        state_id,
        state,
        &mut new_stack,
        &mut new_call_stack,
        static_data,
        function_lookup,
        factory,
        allocator,
        cache,
        cache_entries,
        options,
    );
    // FIXME: Unwind call frame / variable stack on short-circuit signals
    // debug_assert_eq!(
    //     new_call_stack.call_stack_depth(),
    //     0,
    //     "{} frames remaining on call stack",
    //     new_call_stack.call_stack_depth()
    // );
    // debug_assert_eq!(
    //     new_stack.len(),
    //     capture_depth,
    //     "{} values remaining on variable stack",
    //     new_stack.len()
    // );
    let (dependencies, subexpressions) = new_call_stack.drain();
    return (result, dependencies, subexpressions);
}

#[cfg(test)]
mod tests {
    use reflex::core::{DependencyList, SignalType, StateCache, Uid};
    use reflex_lang::{allocator::DefaultAllocator, term::*, SharedTermFactory};
    use reflex_lisp::parse;
    use reflex_stdlib::{Add, And, Append, If, Stdlib};

    use crate::compiler::{hash_compiled_program, Compiler, CompilerMode, CompilerOptions};

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
                    allocator.create_unit_list(factory.create_int_term(5)),
                ),
                allocator.create_unit_list(factory.create_int_term(4)),
            ),
            allocator.create_unit_list(factory.create_int_term(3)),
        );
        assert_eq!(
            match_compiled_application_target(&expression, &factory),
            Some((
                &compiled_function,
                vec![
                    factory.create_int_term(3),
                    factory.create_int_term(4),
                    factory.create_int_term(5)
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
        let condition = allocator.create_signal(
            SignalType::Custom(String::from("foo")),
            factory.create_string_term(allocator.create_string("bar")),
            factory.create_symbol_term(123),
        );
        let expression = factory.create_application_term(
            target,
            allocator.create_pair(
                factory.create_int_term(3),
                factory.create_effect_term(condition.clone()),
            ),
        );
        let compiler = Compiler::new(
            CompilerOptions {
                debug: true,
                ..CompilerOptions::unoptimized()
            },
            None,
        );
        let program = compiler
            .compile(&expression, CompilerMode::Function, &factory, &allocator)
            .unwrap();

        let mut state = StateCache::default();
        let state_id = 0;
        let entry_point = InstructionPointer::default();
        let cache_key = hash_compiled_program(&program, &entry_point);
        let (result, _) = execute(
            cache_key,
            &program,
            entry_point,
            state_id,
            &state,
            &factory,
            &allocator,
            &InterpreterOptions::debug(),
            &mut cache,
        )
        .unwrap();
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_signal_term(allocator.create_signal_list(once(condition.clone()))),
                DependencyList::of(condition.id()),
            ),
        );

        state.set(condition.id(), factory.create_int_term(4));
        let state_id = 1;
        let (result, _) = execute(
            cache_key,
            &program,
            InstructionPointer::default(),
            state_id,
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
                factory.create_int_term(3 + 4),
                DependencyList::of(condition.id()),
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
        let cache_key = hash_compiled_program(&program, &entry_point);
        let state_id = 0;
        let (result, _) = execute(
            cache_key,
            &program,
            entry_point,
            state_id,
            &mut StateCache::default(),
            &factory,
            &allocator,
            &InterpreterOptions::default(),
            &mut cache,
        )
        .unwrap();
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3 + 4), DependencyList::empty(),),
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
        let cache_key = hash_compiled_program(&program, &entry_point);
        let state_id = 0;
        let (result, _) = execute(
            cache_key,
            &program,
            entry_point,
            state_id,
            &mut StateCache::default(),
            &factory,
            &allocator,
            &InterpreterOptions::default(),
            &mut cache,
        )
        .unwrap();
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3 + 4), DependencyList::empty(),),
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
        let program = CompiledProgram {
            instructions: program,
            data_section: Default::default(),
        };
        let state = StateCache::default();
        let entry_point = InstructionPointer::default();
        let cache_key = hash_compiled_program(&program, &entry_point);
        let state_id = 0;
        let (result, _) = execute(
            cache_key,
            &program,
            InstructionPointer::default(),
            state_id,
            &state,
            &factory,
            &allocator,
            &InterpreterOptions::default(),
            &mut cache,
        )
        .unwrap();
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3), DependencyList::empty(),),
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

        let program = CompiledProgram {
            instructions: program,
            data_section: Default::default(),
        };
        let state = StateCache::default();
        let entry_point = InstructionPointer::default();
        let cache_key = hash_compiled_program(&program, &entry_point);
        let state_id = 0;
        let (result, _) = execute(
            cache_key,
            &program,
            InstructionPointer::default(),
            state_id,
            &state,
            &factory,
            &allocator,
            &InterpreterOptions::default(),
            &mut cache,
        )
        .unwrap();
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3 + 4 + 5), DependencyList::empty(),),
        );
    }

    #[test]
    fn builtin_function_applications() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let expression = factory.create_application_term(
            factory.create_builtin_term(Add),
            allocator.create_pair(factory.create_int_term(3), factory.create_int_term(4)),
        );
        let program = Compiler::new(CompilerOptions::unoptimized(), None)
            .compile(&expression, CompilerMode::Function, &factory, &allocator)
            .unwrap();
        let state = StateCache::default();
        let entry_point = InstructionPointer::default();
        let cache_key = hash_compiled_program(&program, &entry_point);
        let state_id = 0;
        let (result, _) = execute(
            cache_key,
            &program,
            InstructionPointer::default(),
            state_id,
            &state,
            &factory,
            &allocator,
            &InterpreterOptions::default(),
            &mut cache,
        )
        .unwrap();
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3 + 4), DependencyList::empty(),),
        );

        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let expression = factory.create_application_term(
            factory.create_builtin_term(If),
            allocator.create_triple(
                factory.create_boolean_term(true),
                factory.create_int_term(3),
                factory.create_int_term(4),
            ),
        );
        let program = Compiler::new(CompilerOptions::unoptimized(), None)
            .compile(&expression, CompilerMode::Function, &factory, &allocator)
            .unwrap();
        let state = StateCache::default();
        let state_id = 0;
        let (result, _) = execute(
            cache_key,
            &program,
            InstructionPointer::default(),
            state_id,
            &state,
            &factory,
            &allocator,
            &InterpreterOptions::default(),
            &mut cache,
        )
        .unwrap();
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3), DependencyList::empty(),),
        );

        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let expression = factory.create_application_term(
            factory.create_builtin_term(If),
            allocator.create_triple(
                factory.create_boolean_term(false),
                factory.create_int_term(3),
                factory.create_int_term(4),
            ),
        );
        let program = Compiler::new(CompilerOptions::unoptimized(), None)
            .compile(&expression, CompilerMode::Function, &factory, &allocator)
            .unwrap();
        let entry_point = InstructionPointer::default();
        let cache_key = hash_compiled_program(&program, &entry_point);
        let state = StateCache::default();
        let state_id = 0;
        let (result, _) = execute(
            cache_key,
            &program,
            InstructionPointer::default(),
            state_id,
            &state,
            &factory,
            &allocator,
            &InterpreterOptions::default(),
            &mut cache,
        )
        .unwrap();
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(4), DependencyList::empty(),),
        );

        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let expression = factory.create_application_term(
            factory.create_builtin_term(If),
            allocator.create_triple(
                factory.create_application_term(
                    factory.create_builtin_term(And),
                    allocator.create_pair(
                        factory.create_boolean_term(true),
                        factory.create_boolean_term(true),
                    ),
                ),
                factory.create_int_term(3),
                factory.create_int_term(4),
            ),
        );
        let program = Compiler::new(CompilerOptions::unoptimized(), None)
            .compile(&expression, CompilerMode::Function, &factory, &allocator)
            .unwrap();
        let entry_point = InstructionPointer::default();
        let cache_key = hash_compiled_program(&program, &entry_point);
        let state = StateCache::default();
        let state_id = 0;
        let (result, _) = execute(
            cache_key,
            &program,
            InstructionPointer::default(),
            state_id,
            &state,
            &factory,
            &allocator,
            &InterpreterOptions::default(),
            &mut cache,
        )
        .unwrap();
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3), DependencyList::empty(),),
        );

        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let expression = factory.create_application_term(
            factory.create_builtin_term(If),
            allocator.create_triple(
                factory.create_application_term(
                    factory.create_builtin_term(And),
                    allocator.create_pair(
                        factory.create_boolean_term(true),
                        factory.create_boolean_term(false),
                    ),
                ),
                factory.create_int_term(3),
                factory.create_int_term(4),
            ),
        );
        let program = Compiler::new(CompilerOptions::unoptimized(), None)
            .compile(&expression, CompilerMode::Function, &factory, &allocator)
            .unwrap();
        let entry_point = InstructionPointer::default();
        let cache_key = hash_compiled_program(&program, &entry_point);
        let state = StateCache::default();
        let state_id = 0;
        let (result, _) = execute(
            cache_key,
            &program,
            InstructionPointer::default(),
            state_id,
            &state,
            &factory,
            &allocator,
            &InterpreterOptions::default(),
            &mut cache,
        )
        .unwrap();
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(4), DependencyList::empty(),),
        );
    }

    #[test]
    fn chained_functions() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let expression = factory.create_application_term(
            factory.create_builtin_term(Add),
            allocator.create_pair(
                factory.create_application_term(
                    factory.create_builtin_term(Add),
                    allocator.create_pair(factory.create_int_term(3), factory.create_int_term(4)),
                ),
                factory.create_int_term(5),
            ),
        );
        let program = Compiler::new(CompilerOptions::unoptimized(), None)
            .compile(&expression, CompilerMode::Function, &factory, &allocator)
            .unwrap();
        let entry_point = InstructionPointer::default();
        let cache_key = hash_compiled_program(&program, &entry_point);
        let state = StateCache::default();
        let state_id = 0;
        let (result, _) = execute(
            cache_key,
            &program,
            InstructionPointer::default(),
            state_id,
            &state,
            &factory,
            &allocator,
            &InterpreterOptions::default(),
            &mut cache,
        )
        .unwrap();
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3 + 4 + 5), DependencyList::empty(),),
        );

        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let expression = factory.create_application_term(
            factory.create_builtin_term(If),
            allocator.create_triple(
                factory.create_application_term(
                    factory.create_builtin_term(And),
                    allocator.create_pair(
                        factory.create_boolean_term(true),
                        factory.create_boolean_term(false),
                    ),
                ),
                factory.create_int_term(3),
                factory.create_int_term(4),
            ),
        );
        let program = Compiler::new(CompilerOptions::unoptimized(), None)
            .compile(&expression, CompilerMode::Function, &factory, &allocator)
            .unwrap();
        let entry_point = InstructionPointer::default();
        let cache_key = hash_compiled_program(&program, &entry_point);
        let state = StateCache::default();
        let state_id = 0;
        let (result, _) = execute(
            cache_key,
            &program,
            InstructionPointer::default(),
            state_id,
            &state,
            &factory,
            &allocator,
            &InterpreterOptions::default(),
            &mut cache,
        )
        .unwrap();
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(4), DependencyList::empty(),),
        );
    }

    #[test]
    fn variadic_functions() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let expression = factory.create_application_term(
            factory.create_builtin_term(Append),
            allocator.create_triple(
                factory.create_list_term(allocator.create_list(vec![
                    factory.create_int_term(1),
                    factory.create_int_term(2),
                    factory.create_int_term(3),
                ])),
                factory.create_list_term(allocator.create_list(vec![
                    factory.create_int_term(4),
                    factory.create_int_term(5),
                    factory.create_int_term(6),
                ])),
                factory.create_list_term(allocator.create_list(vec![
                    factory.create_int_term(7),
                    factory.create_int_term(8),
                    factory.create_int_term(9),
                ])),
            ),
        );
        let program = Compiler::new(CompilerOptions::unoptimized(), None)
            .compile(&expression, CompilerMode::Function, &factory, &allocator)
            .unwrap();
        let entry_point = InstructionPointer::default();
        let cache_key = hash_compiled_program(&program, &entry_point);
        let state = StateCache::default();
        let state_id = 0;
        let (result, _) = execute(
            cache_key,
            &program,
            InstructionPointer::default(),
            state_id,
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
                factory.create_list_term(allocator.create_list(vec![
                    factory.create_int_term(1),
                    factory.create_int_term(2),
                    factory.create_int_term(3),
                    factory.create_int_term(4),
                    factory.create_int_term(5),
                    factory.create_int_term(6),
                    factory.create_int_term(7),
                    factory.create_int_term(8),
                    factory.create_int_term(9),
                ])),
                DependencyList::empty(),
            ),
        );

        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let expression = factory.create_application_term(
            factory.create_builtin_term(Append),
            allocator.create_triple(
                factory.create_application_term(
                    factory.create_lambda_term(
                        0,
                        factory.create_list_term(allocator.create_list(vec![
                            factory.create_int_term(1),
                            factory.create_int_term(2),
                            factory.create_int_term(3),
                        ])),
                    ),
                    allocator.create_empty_list(),
                ),
                factory.create_application_term(
                    factory.create_lambda_term(
                        0,
                        factory.create_list_term(allocator.create_list(vec![
                            factory.create_int_term(4),
                            factory.create_int_term(5),
                            factory.create_int_term(6),
                        ])),
                    ),
                    allocator.create_empty_list(),
                ),
                factory.create_application_term(
                    factory.create_lambda_term(
                        0,
                        factory.create_list_term(allocator.create_list(vec![
                            factory.create_int_term(7),
                            factory.create_int_term(8),
                            factory.create_int_term(9),
                        ])),
                    ),
                    allocator.create_empty_list(),
                ),
            ),
        );
        let program = Compiler::new(CompilerOptions::unoptimized(), None)
            .compile(&expression, CompilerMode::Function, &factory, &allocator)
            .unwrap();
        let entry_point = InstructionPointer::default();
        let cache_key = hash_compiled_program(&program, &entry_point);
        let state = StateCache::default();
        let state_id = 0;
        let (result, _) = execute(
            cache_key,
            &program,
            InstructionPointer::default(),
            state_id,
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
                factory.create_list_term(allocator.create_list(vec![
                    factory.create_int_term(1),
                    factory.create_int_term(2),
                    factory.create_int_term(3),
                    factory.create_int_term(4),
                    factory.create_int_term(5),
                    factory.create_int_term(6),
                    factory.create_int_term(7),
                    factory.create_int_term(8),
                    factory.create_int_term(9),
                ])),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn intern_static_test() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let expression = factory.create_int_term(3);
        let program = Compiler::new(CompilerOptions::debug(), None)
            .compile(&expression, CompilerMode::Function, &factory, &allocator)
            .unwrap();

        let entry_point = InstructionPointer::default();
        let cache_key = hash_compiled_program(&program, &entry_point);
        let state = StateCache::default();
        let state_id = 0;
        let mut cache = DefaultInterpreterCache::default();

        let (result, _) = execute(
            cache_key,
            &program,
            InstructionPointer::default(),
            state_id,
            &state,
            &factory,
            &allocator,
            &InterpreterOptions::default(),
            &mut cache,
        )
        .unwrap();
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3), DependencyList::empty(),),
        );
    }
}
