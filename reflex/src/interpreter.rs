// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    iter::once,
};

mod cache;
pub use cache::{DefaultInterpreterCache, InterpreterCache};
mod stack;
pub use stack::{CallStack, VariableStack};

use crate::{
    cache::NoopCache,
    compiler::{Instruction, InstructionPointer, NativeFunctionRegistry, Program},
    core::{
        Applicable, DependencyList, DynamicState, EvaluationResult, Expression, ExpressionFactory,
        ExpressionList, HeapAllocator, Reducible, Rewritable, SignalType,
    },
    hash::HashId,
    lang::{
        get_short_circuit_signal, BuiltinTerm, CompiledFunctionTerm, ValueTerm,
        WithCompiledBuiltins,
    },
};

#[derive(Debug)]
enum ExecutionResult<T: Expression> {
    Advance,
    Jump(InstructionPointer),
    CallFunction {
        target_address: InstructionPointer,
        caller_address: InstructionPointer,
        resume_address: InstructionPointer,
    },
    Return,
    ResolveExpression,
    ResolveApplicationTarget {
        target: T,
        args: Vec<T>,
    },
    ResolveList(ExpressionList<T>),
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

pub fn execute<T: Expression + Rewritable<T> + Reducible<T> + Applicable<T>>(
    program: &Program,
    entry_point: InstructionPointer,
    state: &DynamicState<T>,
    factory: &(impl ExpressionFactory<T> + WithCompiledBuiltins),
    allocator: &impl HeapAllocator<T>,
    builtins: &[(BuiltinTerm, InstructionPointer)],
    plugins: &NativeFunctionRegistry<T>,
    options: &InterpreterOptions,
    cache: &mut impl InterpreterCache<T>,
) -> Result<(EvaluationResult<T>, HashId), String> {
    let cache_key = hash_program_root(program, &entry_point);
    let mut stack = VariableStack::new(options.variable_stack_size);
    let mut call_stack = CallStack::new(program, entry_point, options.call_stack_size);
    let result = evaluate_program_loop(
        state,
        &mut stack,
        &mut call_stack,
        factory,
        &factory.with_compiled_builtins(
            builtins,
            &plugins
                .iter()
                .map(|(target, address)| (target.uid(), target.arity(), *address))
                .collect::<Vec<_>>(),
        ),
        allocator,
        plugins,
        cache,
        options.debug_instructions,
        options.debug_stack,
    );
    match result {
        Err(error) => Err(error),
        Ok(value) => {
            assert_eq!(call_stack.call_stack_depth(), 0);
            let (dependencies, subexpressions) = call_stack.drain();
            let result = EvaluationResult::new(value, dependencies);
            cache.store_result(cache_key, state, result.clone(), subexpressions);
            Ok((result, cache_key))
        }
    }
}

fn hash_program_root(program: &Program, entry_point: &InstructionPointer) -> HashId {
    let mut hasher = DefaultHasher::new();
    program.hash(&mut hasher);
    entry_point.hash(&mut hasher);
    hasher.finish()
}

fn evaluate_program_loop<T: Expression + Rewritable<T> + Reducible<T> + Applicable<T>>(
    state: &DynamicState<T>,
    stack: &mut VariableStack<T>,
    call_stack: &mut CallStack<T>,
    default_factory: &impl ExpressionFactory<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    plugins: &NativeFunctionRegistry<T>,
    cache: &mut impl InterpreterCache<T>,
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
                    println!("> {:x} {:?}", call_stack.program_counter(), instruction);
                }
                if debug_stack {
                    println!(
                        "{}",
                        stack
                            .values()
                            .iter()
                            .rev()
                            .enumerate()
                            .map(|(offset, value)| format!("  {}: {}", offset, value))
                            .collect::<Vec<_>>()
                            .join("\n")
                    );
                }
                evaluate_instruction(
                    instruction,
                    state,
                    stack,
                    call_stack,
                    default_factory,
                    factory,
                    allocator,
                    plugins,
                    cache,
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
                            stack.push(factory.create_application_term(
                                target.clone(),
                                allocator.create_list(args),
                            ));
                            continue;
                        } else {
                            let target_hash = target.id();
                            stack.push(target);
                            call_stack.enter_application_target(args);
                            call_stack.enter_expression(target_hash);
                            continue;
                        }
                    }
                    ExecutionResult::ResolveList(items) => {
                        call_stack.enter_list(items.into_values(), None);
                        continue;
                    }
                    ExecutionResult::Advance => {
                        let previous_address = call_stack.program_counter();
                        let next_address = call_stack.next_program_counter();
                        let is_leaving_evaluation = call_stack
                            .lookup_instruction(previous_address)
                            .map(|instruction| match instruction {
                                Instruction::Evaluate | Instruction::EvaluateArgList => true,
                                _ => false,
                            })
                            .unwrap_or(false);
                        if is_leaving_evaluation {
                            let value = stack.peek().unwrap();
                            while let Some((hash, dependencies, subexpressions)) =
                                call_stack.pop_expression_stack(previous_address)
                            {
                                call_stack.add_state_dependencies(dependencies.iter());
                                cache.store_result(
                                    hash,
                                    state,
                                    EvaluationResult::new(value.clone(), dependencies),
                                    subexpressions,
                                );
                            }
                            if let Some((args, dependencies, subexpressions)) =
                                call_stack.pop_application_target_stack()
                            {
                                // FIXME: remove
                                if let Some(_) = call_stack.peek_list_stack(previous_address) {
                                    panic!("!!! EVALUATION NESTED WITHIN LIST");
                                }
                                call_stack.add_state_dependencies(dependencies);
                                call_stack.add_subexpressions(subexpressions);
                                let resolved_target = stack.pop().unwrap();
                                stack.push(factory.create_application_term(
                                    resolved_target,
                                    allocator.create_list(args),
                                ));
                                continue;
                            } else if let Some((items, results, dependencies, subexpressions)) =
                                call_stack.pop_list_stack(previous_address)
                            {
                                call_stack.add_state_dependencies(dependencies);
                                call_stack.add_subexpressions(subexpressions);
                                let resolved_item = stack.pop().unwrap();
                                let mut results = results;
                                results.push(resolved_item);
                                if results.len() < items.len() {
                                    call_stack.enter_list(items, Some(results));
                                    continue;
                                } else {
                                    let resolved_list =
                                        factory.create_tuple_term(allocator.create_list(results));
                                    stack.push(resolved_list);
                                }
                            }
                        }
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
                        target_address,
                        caller_address,
                        resume_address,
                    } => {
                        if let Some((target_hash, num_args, variadic)) = call_stack
                            .lookup_instruction(target_address)
                            .and_then(|target| match target {
                                Instruction::Function {
                                    hash,
                                    arity,
                                    variadic,
                                } => Some((hash, *arity, *variadic)),
                                _ => None,
                            })
                        {
                            let variadic_offset = if variadic { 1 } else { 0 };
                            let hash = generate_function_call_hash(
                                target_hash,
                                stack.slice(num_args + variadic_offset),
                            );
                            if let Some(cached_result) = cache.retrieve_result(hash, state) {
                                call_stack.add_subexpression(hash);
                                call_stack
                                    .add_state_dependencies(cached_result.dependencies().iter());
                                stack.pop_multiple(num_args + variadic_offset);
                                stack.push(cached_result.result().clone());

                                let is_resuming_list_evaluation = resume_address == caller_address
                                    && call_stack
                                        .lookup_instruction(resume_address)
                                        .map(is_evaluate_list_instruction)
                                        .unwrap_or(false);
                                if is_resuming_list_evaluation {
                                    process_next_list_item(
                                        stack,
                                        call_stack,
                                        resume_address,
                                        factory,
                                        allocator,
                                    );
                                    continue;
                                } else {
                                    call_stack.update_program_counter(resume_address);
                                    continue;
                                }
                            } else {
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
                            cache.store_result(
                                hash,
                                state,
                                EvaluationResult::new(value.clone(), dependencies),
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

                            let is_resuming_list_evaluation = resume_address == caller_address
                                && call_stack
                                    .lookup_instruction(resume_address)
                                    .map(is_evaluate_list_instruction)
                                    .unwrap_or(false);
                            if is_resuming_list_evaluation {
                                process_next_list_item(
                                    stack,
                                    call_stack,
                                    resume_address,
                                    factory,
                                    allocator,
                                );
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

fn process_next_list_item<T: Expression>(
    stack: &mut VariableStack<T>,
    call_stack: &mut CallStack<T>,
    resume_address: InstructionPointer,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) {
    let (items, results, dependencies, subexpressions) =
        call_stack.pop_list_stack(resume_address).unwrap();
    call_stack.add_state_dependencies(dependencies);
    call_stack.add_subexpressions(subexpressions);
    let resolved_item = stack.pop().unwrap();
    if !resolved_item.is_static() {
        let current_index = results.len();
        let mut items = items;
        items[current_index] = resolved_item;
        call_stack.update_program_counter(resume_address);
        call_stack.enter_list(items, Some(results));
    } else {
        let mut results = results;
        results.push(resolved_item);
        if results.len() < items.len() {
            call_stack.update_program_counter(resume_address);
            call_stack.enter_list(items, Some(results));
        } else {
            let resolved_list = factory.create_tuple_term(allocator.create_list(results));
            stack.push(resolved_list);
            call_stack.update_program_counter(resume_address.advance());
        }
    }
}

fn evaluate_instruction<T: Expression + Rewritable<T> + Reducible<T> + Applicable<T>>(
    instruction: &Instruction,
    state: &DynamicState<T>,
    stack: &mut VariableStack<T>,
    call_stack: &CallStack<T>,
    default_factory: &impl ExpressionFactory<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    plugins: &NativeFunctionRegistry<T>,
    cache: &mut impl InterpreterCache<T>,
) -> Result<(ExecutionResult<T>, DependencyList), String> {
    match instruction {
        Instruction::PushStatic { offset } => {
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
            let state_token = *state_token;
            let fallback = stack.pop();
            match fallback {
                None => Err(format!(
                    "Missing dynamic variable fallback: {}",
                    state_token
                )),
                Some(fallback) => {
                    let value = state.get(state_token).cloned().unwrap_or(fallback);
                    stack.push(value);
                    Ok((ExecutionResult::Advance, DependencyList::of(state_token)))
                }
            }
        }
        Instruction::PushNull => {
            stack.push(factory.create_value_term(ValueTerm::Null));
            Ok((ExecutionResult::Advance, DependencyList::empty()))
        }
        Instruction::PushBoolean { value } => {
            stack.push(factory.create_value_term(ValueTerm::Boolean(*value)));
            Ok((ExecutionResult::Advance, DependencyList::empty()))
        }
        Instruction::PushInt { value } => {
            stack.push(factory.create_value_term(ValueTerm::Int(*value)));
            Ok((ExecutionResult::Advance, DependencyList::empty()))
        }
        Instruction::PushFloat { value } => {
            stack.push(factory.create_value_term(ValueTerm::Float(*value)));
            Ok((ExecutionResult::Advance, DependencyList::empty()))
        }
        Instruction::PushString { value } => {
            stack.push(factory.create_value_term(ValueTerm::String(
                allocator.create_string(String::from(value)),
            )));
            Ok((ExecutionResult::Advance, DependencyList::empty()))
        }
        Instruction::PushSymbol { value } => {
            stack.push(factory.create_value_term(ValueTerm::Symbol(*value)));
            Ok((ExecutionResult::Advance, DependencyList::empty()))
        }
        Instruction::PushHash { value } => {
            stack.push(factory.create_value_term(ValueTerm::Hash(*value)));
            Ok((ExecutionResult::Advance, DependencyList::empty()))
        }
        Instruction::PushFunction { target } => {
            let target_address = *target;
            match call_stack.lookup_instruction(target_address) {
                Some(Instruction::Function {
                    hash,
                    arity,
                    variadic,
                }) => {
                    let num_args = *arity;
                    stack.push(factory.create_compiled_function_term(
                        *hash,
                        target_address,
                        num_args,
                        *variadic,
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
            stack.push(default_factory.create_builtin_term(*target));
            Ok((ExecutionResult::Advance, DependencyList::empty()))
        }
        Instruction::PushNative { target } => {
            let uid = *target;
            match plugins.get(uid) {
                Some(target) => {
                    stack.push(default_factory.create_native_function_term(target.clone()));
                    Ok((ExecutionResult::Advance, DependencyList::empty()))
                }
                None => Err(format!("Invalid native function id: {}", uid)),
            }
        }
        Instruction::PushSignal {
            signal_type,
            num_args,
        } => {
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
            let target_address = *target;
            Ok((
                ExecutionResult::Jump(target_address),
                DependencyList::empty(),
            ))
        }
        Instruction::Call { target } => {
            let target_address = *target;
            let caller_address = call_stack.program_counter();
            let resume_address = call_stack.next_program_counter();
            Ok((
                ExecutionResult::CallFunction {
                    target_address,
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
                    let target_address = compiled_target.address();
                    let caller_address = call_stack.program_counter();
                    let resume_address = call_stack.next_program_counter();
                    if !partial_args.is_empty() {
                        stack.splice(num_args, partial_args.into_iter().cloned());
                    }
                    Ok((
                        ExecutionResult::CallFunction {
                            target_address,
                            caller_address,
                            resume_address,
                        },
                        DependencyList::empty(),
                    ))
                } else {
                    match target.arity() {
                        None => Err(format!("Invalid function application target: {}", target)),
                        Some(arity) => {
                            let variadic_args = if arity.variadic().is_some() {
                                let variadic_args = stack.pop().unwrap();
                                if let Some(args) = factory.match_tuple_term(&variadic_args) {
                                    Ok(Some(allocator.clone_list(args.fields())))
                                } else {
                                    Err(format!(
                                        "{}: Expected variadic argument list, received {}",
                                        target, variadic_args
                                    ))
                                }
                            } else {
                                Ok(None)
                            };
                            match variadic_args {
                                Err(error) => Err(error),
                                Ok(variadic_args) => {
                                    let variadic_offset =
                                        if variadic_args.is_some() { 1 } else { 0 };
                                    let num_positional_args = num_args - variadic_offset;
                                    let positional_args = stack.pop_multiple(num_positional_args);
                                    let args = match variadic_args {
                                        None => allocator.create_list(positional_args),
                                        Some(variadic_args) => allocator.create_sized_list(
                                            num_positional_args + variadic_args.len(),
                                            positional_args.into_iter().chain(variadic_args),
                                        ),
                                    };
                                    let result = if args.len() < arity.required() {
                                        create_error_signal(
                                            format!(
                                                "{}: expected {}{} arguments, received {}",
                                                target,
                                                arity.required(),
                                                if arity.variadic().is_some() {
                                                    " or more"
                                                } else {
                                                    ""
                                                },
                                                num_args,
                                            ),
                                            factory,
                                            allocator,
                                        )
                                    } else if let Some(signal) = get_short_circuit_signal(
                                        args.as_slice(),
                                        &arity,
                                        factory,
                                        allocator,
                                    ) {
                                        if let Some(_) =
                                            factory.match_signal_transformer_term(&target)
                                        {
                                            match target.apply(
                                                vec![signal],
                                                factory,
                                                allocator,
                                                &mut NoopCache::default(),
                                            ) {
                                                Ok(result) => result,
                                                Err(error) => {
                                                    create_error_signal(error, factory, allocator)
                                                }
                                            }
                                        } else {
                                            signal
                                        }
                                    } else {
                                        match target.apply(
                                            args.iter().cloned(),
                                            factory,
                                            allocator,
                                            &mut NoopCache::default(),
                                        ) {
                                            Ok(result) => result,
                                            Err(error) => {
                                                create_error_signal(error, factory, allocator)
                                            }
                                        }
                                    };
                                    stack.push(result);
                                    Ok((ExecutionResult::Advance, DependencyList::empty()))
                                }
                            }
                        }
                    }
                }
            }
        }
        Instruction::Function { .. } => Ok((ExecutionResult::Advance, DependencyList::empty())),
        Instruction::Return => Ok((ExecutionResult::Return, DependencyList::empty())),
        Instruction::Evaluate => match stack.peek().map(|target| target.is_static()) {
            None => Err(String::from("Missing evaluation target")),
            Some(true) => Ok((ExecutionResult::Advance, DependencyList::empty())),
            Some(false) => {
                let expression = stack.pop().unwrap();
                evaluate_expression(
                    &expression,
                    state,
                    stack,
                    call_stack,
                    factory,
                    allocator,
                    cache,
                )
            }
        },
        Instruction::EvaluateArgList => {
            if let Some(item) = call_stack.peek_next_list_item(call_stack.program_counter()) {
                evaluate_expression(item, state, stack, call_stack, factory, allocator, cache)
            } else {
                match stack.pop() {
                    Some(value) => {
                        if let Some(arg_list) = factory.match_tuple_term(&value) {
                            if arg_list.fields().is_empty() {
                                stack.push(value);
                                Ok((ExecutionResult::Advance, DependencyList::empty()))
                            } else {
                                let unresolved_values = allocator.clone_list(arg_list.fields());
                                Ok((
                                    ExecutionResult::ResolveList(unresolved_values),
                                    DependencyList::empty(),
                                ))
                            }
                        } else {
                            Err(format!("Expected <tuple>, received {}", value))
                        }
                    }
                    None => Err(String::from("No items on stack")),
                }
                .map_err(|err| format!("Unable to perform multiple evaluation: {}", err))
            }
        }
        Instruction::ConstructApplication { num_args } => {
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
            stack.push(factory.create_constructor_term(prototype.clone()));
            Ok((ExecutionResult::Advance, DependencyList::empty()))
        }
        Instruction::ConstructVector { size } => {
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
        Instruction::ConstructSignalTransformer => {
            if stack.len() < 1 {
                Err(format!(
                    "Unable to create signal transformer: insufficient arguments on stack",
                ))
            } else {
                let transform = stack.pop().unwrap();
                stack.push(factory.create_signal_transformer_term(transform));
                Ok((ExecutionResult::Advance, DependencyList::empty()))
            }
        }
    }
}

fn evaluate_expression<T: Expression>(
    expression: &T,
    state: &DynamicState<T>,
    stack: &mut VariableStack<T>,
    call_stack: &CallStack<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    cache: &mut impl InterpreterCache<T>,
) -> Result<(ExecutionResult<T>, DependencyList), String> {
    if expression.is_static() {
        stack.push(expression.clone());
        Ok((ExecutionResult::Advance, DependencyList::empty()))
    } else {
        let hash = expression.id();
        if let Some(cached_result) = cache.retrieve_result(hash, state) {
            stack.push(cached_result.result().clone());
            Ok((
                ExecutionResult::Advance,
                cached_result.dependencies().clone(),
            ))
        } else if let Some(application) = factory.match_application_term(expression) {
            let target = application.target();
            let args = application.args();
            let (target, partial_args) = extract_partial_args(target, factory);
            let num_args = partial_args.len() + args.len();
            let combined_args = partial_args.into_iter().chain(args.iter()).cloned();
            if !target.is_static() {
                Ok((
                    ExecutionResult::ResolveApplicationTarget {
                        target: target.clone(),
                        args: combined_args.collect(),
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
                if compiled_function.variadic() {
                    let num_positional_args = compiled_function.num_args();
                    let num_variadic_args = num_args - num_positional_args;
                    let mut combined_args = combined_args;
                    let mut positional_args = Vec::with_capacity(num_positional_args);
                    for _ in 0..num_positional_args {
                        positional_args.push(combined_args.next().unwrap());
                    }
                    stack.push_multiple(positional_args);
                    stack.push(factory.create_tuple_term(
                        allocator.create_sized_list(num_variadic_args, combined_args),
                    ));
                } else {
                    stack.push_multiple(combined_args);
                }
                Ok((
                    ExecutionResult::CallFunction {
                        target_address,
                        caller_address,
                        resume_address,
                    },
                    DependencyList::empty(),
                ))
            } else if let Some(constructor) = factory.match_constructor_term(target) {
                stack.push(factory.create_struct_term(
                    allocator.clone_struct_prototype(constructor.prototype()),
                    allocator.create_sized_list(num_args, combined_args),
                ));
                Ok((ExecutionResult::Advance, DependencyList::empty()))
            } else {
                Err(format!("Invalid function application target: {}", target))
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
                    .get(expression.state_token())
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

fn is_evaluate_instruction(instruction: &Instruction) -> bool {
    match instruction {
        Instruction::Evaluate => true,
        _ => false,
    }
}

fn is_evaluate_list_instruction(instruction: &Instruction) -> bool {
    match instruction {
        Instruction::EvaluateArgList => true,
        _ => false,
    }
}

fn match_compiled_application_target<'a, T: Expression>(
    target: &'a T,
    factory: &impl ExpressionFactory<T>,
) -> Option<(&'a CompiledFunctionTerm, Vec<&'a T>)> {
    let (target, partial_args) = extract_partial_args(target, factory);
    match factory.match_compiled_function_term(target) {
        Some(target) => Some((target, partial_args)),
        None => None,
    }
}

fn extract_partial_args<'a, T: Expression>(
    target: &'a T,
    factory: &impl ExpressionFactory<T>,
) -> (&'a T, Vec<&'a T>) {
    let mut partial_args = Vec::new();
    let mut current = target;
    while let Some(partial) = factory.match_partial_application_term(current) {
        partial_args.extend(partial.args());
        current = partial.target();
    }
    (current, partial_args)
}

fn create_error_signal<T: Expression>(
    message: String,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_signal_term(allocator.create_signal_list(once(allocator.create_signal(
        SignalType::Error,
        allocator.create_unit_list(
            factory.create_value_term(ValueTerm::String(allocator.create_string(message))),
        ),
    ))))
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

#[cfg(test)]
mod tests {
    use std::iter::empty;

    use crate::allocator::DefaultAllocator;
    use crate::compiler::Compiler;
    use crate::compiler::CompilerMode;
    use crate::compiler::CompilerOptions;
    use crate::core::DependencyList;
    use crate::interpreter::*;
    use crate::lang::*;
    use crate::parser::sexpr::parse;

    #[test]
    fn compiled_functions() {
        let factory = TermFactory::default();
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
        let (program, builtins, plugins) = compiler
            .compile(
                &expression,
                CompilerMode::Expression,
                true,
                empty(),
                &factory,
                &allocator,
            )
            .unwrap()
            .into_parts();
        let mut state = DynamicState::new();
        let (result, _) = execute(
            &program,
            InstructionPointer::default(),
            &state,
            &factory,
            &allocator,
            &builtins,
            &plugins,
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
            &program,
            InstructionPointer::default(),
            &state,
            &factory,
            &allocator,
            &builtins,
            &plugins,
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
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let expression = parse(
            "((lambda (foo bar) ((lambda (baz) (+ foo baz)) bar)) 3 4)",
            &factory,
            &allocator,
        )
        .unwrap();
        let compiler = Compiler::new(CompilerOptions::unoptimized(), None);
        let (program, builtins, plugins) = compiler
            .compile(
                &expression,
                CompilerMode::Expression,
                true,
                empty(),
                &factory,
                &allocator,
            )
            .unwrap()
            .into_parts();
        let (result, _) = execute(
            &program,
            InstructionPointer::default(),
            &mut DynamicState::new(),
            &factory,
            &allocator,
            &builtins,
            &plugins,
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
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let expression = parse(
            "(((lambda (foo) (lambda (bar) (+ foo bar))) 3) 4)",
            &factory,
            &allocator,
        )
        .unwrap();
        let compiler = Compiler::new(CompilerOptions::unoptimized(), None);
        let (program, builtins, plugins) = compiler
            .compile(
                &expression,
                CompilerMode::Expression,
                true,
                empty(),
                &factory,
                &allocator,
            )
            .unwrap()
            .into_parts();
        let (result, _) = execute(
            &program,
            InstructionPointer::default(),
            &mut DynamicState::new(),
            &factory,
            &allocator,
            &builtins,
            &plugins,
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
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let program = Program::new(vec![
            Instruction::PushInt { value: 3 },
            Instruction::Evaluate,
            Instruction::Return,
        ]);
        let state = DynamicState::new();
        let (result, _) = execute(
            &program,
            InstructionPointer::default(),
            &state,
            &factory,
            &allocator,
            &Vec::new(),
            &NativeFunctionRegistry::default(),
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
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let program = Program::new(vec![
            Instruction::PushInt { value: 5 },
            Instruction::PushInt { value: 4 },
            Instruction::PushInt { value: -3 },
            Instruction::PushBuiltin {
                target: BuiltinTerm::Abs,
            },
            Instruction::Apply { num_args: 1 },
            Instruction::Evaluate,
            Instruction::PushBuiltin {
                target: BuiltinTerm::Add,
            },
            Instruction::Apply { num_args: 2 },
            Instruction::Evaluate,
            Instruction::PushBuiltin {
                target: BuiltinTerm::Add,
            },
            Instruction::Apply { num_args: 2 },
            Instruction::Evaluate,
            Instruction::Return,
        ]);
        let state = DynamicState::new();
        let (result, _) = execute(
            &program,
            InstructionPointer::default(),
            &state,
            &factory,
            &allocator,
            &Vec::new(),
            &NativeFunctionRegistry::default(),
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
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let expression = factory.create_application_term(
            factory.create_builtin_term(BuiltinTerm::Add),
            allocator.create_pair(
                factory.create_value_term(ValueTerm::Int(3)),
                factory.create_value_term(ValueTerm::Int(4)),
            ),
        );
        let (program, builtins, plugins) = Compiler::new(CompilerOptions::unoptimized(), None)
            .compile(
                &expression,
                CompilerMode::Expression,
                true,
                empty(),
                &factory,
                &allocator,
            )
            .unwrap()
            .into_parts();
        let state = DynamicState::new();
        let (result, _) = execute(
            &program,
            InstructionPointer::default(),
            &state,
            &factory,
            &allocator,
            &builtins,
            &plugins,
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

        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let expression = factory.create_application_term(
            factory.create_builtin_term(BuiltinTerm::If),
            allocator.create_triple(
                factory.create_value_term(ValueTerm::Boolean(true)),
                factory.create_value_term(ValueTerm::Int(3)),
                factory.create_value_term(ValueTerm::Int(4)),
            ),
        );
        let (program, builtins, plugins) = Compiler::new(CompilerOptions::unoptimized(), None)
            .compile(
                &expression,
                CompilerMode::Expression,
                true,
                empty(),
                &factory,
                &allocator,
            )
            .unwrap()
            .into_parts();
        let state = DynamicState::new();
        let (result, _) = execute(
            &program,
            InstructionPointer::default(),
            &state,
            &factory,
            &allocator,
            &builtins,
            &plugins,
            &InterpreterOptions::default(),
            &mut cache,
        )
        .unwrap();
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Int(3)),
                DependencyList::empty()
            ),
        );

        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let expression = factory.create_application_term(
            factory.create_builtin_term(BuiltinTerm::If),
            allocator.create_triple(
                factory.create_value_term(ValueTerm::Boolean(false)),
                factory.create_value_term(ValueTerm::Int(3)),
                factory.create_value_term(ValueTerm::Int(4)),
            ),
        );
        let (program, builtins, plugins) = Compiler::new(CompilerOptions::unoptimized(), None)
            .compile(
                &expression,
                CompilerMode::Expression,
                true,
                empty(),
                &factory,
                &allocator,
            )
            .unwrap()
            .into_parts();
        let state = DynamicState::new();
        let (result, _) = execute(
            &program,
            InstructionPointer::default(),
            &state,
            &factory,
            &allocator,
            &builtins,
            &plugins,
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

        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let expression = factory.create_application_term(
            factory.create_builtin_term(BuiltinTerm::If),
            allocator.create_triple(
                factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::And),
                    allocator.create_pair(
                        factory.create_value_term(ValueTerm::Boolean(true)),
                        factory.create_value_term(ValueTerm::Boolean(true)),
                    ),
                ),
                factory.create_value_term(ValueTerm::Int(3)),
                factory.create_value_term(ValueTerm::Int(4)),
            ),
        );
        let (program, builtins, plugins) = Compiler::new(CompilerOptions::unoptimized(), None)
            .compile(
                &expression,
                CompilerMode::Expression,
                true,
                empty(),
                &factory,
                &allocator,
            )
            .unwrap()
            .into_parts();
        let state = DynamicState::new();
        let (result, _) = execute(
            &program,
            InstructionPointer::default(),
            &state,
            &factory,
            &allocator,
            &builtins,
            &plugins,
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

        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let expression = factory.create_application_term(
            factory.create_builtin_term(BuiltinTerm::If),
            allocator.create_triple(
                factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::And),
                    allocator.create_pair(
                        factory.create_value_term(ValueTerm::Boolean(true)),
                        factory.create_value_term(ValueTerm::Boolean(false)),
                    ),
                ),
                factory.create_value_term(ValueTerm::Int(3)),
                factory.create_value_term(ValueTerm::Int(4)),
            ),
        );
        let (program, builtins, plugins) = Compiler::new(CompilerOptions::unoptimized(), None)
            .compile(
                &expression,
                CompilerMode::Expression,
                true,
                empty(),
                &factory,
                &allocator,
            )
            .unwrap()
            .into_parts();
        let state = DynamicState::new();
        let (result, _) = execute(
            &program,
            InstructionPointer::default(),
            &state,
            &factory,
            &allocator,
            &builtins,
            &plugins,
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
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let expression = factory.create_application_term(
            factory.create_builtin_term(BuiltinTerm::Add),
            allocator.create_pair(
                factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::Add),
                    allocator.create_pair(
                        factory.create_value_term(ValueTerm::Int(3)),
                        factory.create_value_term(ValueTerm::Int(4)),
                    ),
                ),
                factory.create_value_term(ValueTerm::Int(5)),
            ),
        );
        let (program, builtins, plugins) = Compiler::new(CompilerOptions::unoptimized(), None)
            .compile(
                &expression,
                CompilerMode::Expression,
                true,
                empty(),
                &factory,
                &allocator,
            )
            .unwrap()
            .into_parts();
        let state = DynamicState::new();
        let (result, _) = execute(
            &program,
            InstructionPointer::default(),
            &state,
            &factory,
            &allocator,
            &builtins,
            &plugins,
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

        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let expression = factory.create_application_term(
            factory.create_builtin_term(BuiltinTerm::If),
            allocator.create_triple(
                factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::And),
                    allocator.create_pair(
                        factory.create_value_term(ValueTerm::Boolean(true)),
                        factory.create_value_term(ValueTerm::Boolean(false)),
                    ),
                ),
                factory.create_value_term(ValueTerm::Int(3)),
                factory.create_value_term(ValueTerm::Int(4)),
            ),
        );
        let (program, builtins, plugins) = Compiler::new(CompilerOptions::unoptimized(), None)
            .compile(
                &expression,
                CompilerMode::Expression,
                true,
                empty(),
                &factory,
                &allocator,
            )
            .unwrap()
            .into_parts();
        let state = DynamicState::new();
        let (result, _) = execute(
            &program,
            InstructionPointer::default(),
            &state,
            &factory,
            &allocator,
            &builtins,
            &plugins,
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
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let expression = factory.create_application_term(
            factory.create_builtin_term(BuiltinTerm::Append),
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
        let (program, builtins, plugins) = Compiler::new(CompilerOptions::unoptimized(), None)
            .compile(
                &expression,
                CompilerMode::Expression,
                true,
                empty(),
                &factory,
                &allocator,
            )
            .unwrap()
            .into_parts();
        let state = DynamicState::new();
        let (result, _) = execute(
            &program,
            InstructionPointer::default(),
            &state,
            &factory,
            &allocator,
            &builtins,
            &plugins,
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

        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let expression = factory.create_application_term(
            factory.create_builtin_term(BuiltinTerm::Append),
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
        let (program, builtins, plugins) = Compiler::new(CompilerOptions::unoptimized(), None)
            .compile(
                &expression,
                CompilerMode::Expression,
                true,
                empty(),
                &factory,
                &allocator,
            )
            .unwrap()
            .into_parts();
        let state = DynamicState::new();
        let (result, _) = execute(
            &program,
            InstructionPointer::default(),
            &state,
            &factory,
            &allocator,
            &builtins,
            &plugins,
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
}
