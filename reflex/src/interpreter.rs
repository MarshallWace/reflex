// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::once;

mod cache;
pub use cache::{DefaultInterpreterCache, InterpreterCache};
mod stack;
pub use stack::{CallStack, VariableStack};

use crate::{
    cache::SubstitutionCache,
    compiler::{Instruction, InstructionPointer, NativeFunctionRegistry, Program},
    core::{
        Applicable, DependencyList, DynamicState, EvaluationCache, EvaluationResult, Expression,
        ExpressionFactory, HeapAllocator, Reducible, Rewritable, SignalType,
    },
    hash::HashId,
    lang::{get_short_circuit_signal, CompiledFunctionTerm, ValueTerm},
};

pub trait Execute<T: Expression + Applicable<T>> {
    fn is_executable(&self) -> bool;
    fn execute(
        &self,
        state: &DynamicState<T>,
        stack: &mut VariableStack<T>,
        call_stack: &CallStack,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Result<(ExecutionResult, DependencyList), String>;
}

pub enum ExecutionResult {
    Repeat,
    Advance,
    Jump(InstructionPointer),
    CallFunction {
        target_address: InstructionPointer,
        resume_address: InstructionPointer,
    },
    CallSubroutine {
        subroutine: Program,
        num_args: usize,
        resume_address: InstructionPointer,
    },
    Return,
    End,
}

#[derive(Copy, Clone)]
pub struct InterpreterOptions {
    pub debug: bool,
    pub call_stack_size: Option<usize>,
    pub variable_stack_size: Option<usize>,
}
impl Default for InterpreterOptions {
    fn default() -> Self {
        Self {
            debug: false,
            call_stack_size: Some(1024),
            variable_stack_size: Some(4096),
        }
    }
}

pub fn execute<T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Execute<T>>(
    program: &Program,
    entry_point: InstructionPointer,
    state: &DynamicState<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    plugins: &NativeFunctionRegistry<T>,
    options: &InterpreterOptions,
    cache: &mut impl InterpreterCache<T>,
) -> Result<(EvaluationResult<T>, HashId), String> {
    let mut context = VariableStack::new(options.variable_stack_size);
    let mut evaluation_cache = SubstitutionCache::new();
    let call_stack = CallStack::new(program, entry_point, options.call_stack_size, options.debug);
    let cache_key = call_stack.hash();
    evaluate_program_loop(
        state,
        &mut context,
        call_stack,
        factory,
        allocator,
        plugins,
        cache,
        &mut evaluation_cache,
        options.debug,
    )
    .map(|result| (result, cache_key))
}

fn evaluate_program_loop<
    T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Execute<T>,
>(
    state: &DynamicState<T>,
    stack: &mut VariableStack<T>,
    mut call_stack: CallStack,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    plugins: &NativeFunctionRegistry<T>,
    cache: &mut impl InterpreterCache<T>,
    evaluation_cache: &mut impl EvaluationCache<T>,
    debug: bool,
) -> Result<EvaluationResult<T>, String> {
    loop {
        let result = match call_stack.lookup_instruction(call_stack.get_program_counter()) {
            None => Err(format!(
                "Invalid program instruction offset: {:x}",
                call_stack.get_program_counter(),
            )),
            Some(instruction) => {
                if debug {
                    println!(
                        "> {:x} {:?}{}",
                        call_stack.get_program_counter(),
                        instruction,
                        stack
                            .values()
                            .iter()
                            .rev()
                            .enumerate()
                            .map(|(offset, value)| format!("\n  {}: {}", offset, value))
                            .collect::<Vec<_>>()
                            .join("")
                    );
                }
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
                            Some(Instruction::Function { hash, arity }) => {
                                let num_args = *arity;
                                stack.push(factory.create_compiled_function_term(
                                    *hash,
                                    target_address,
                                    num_args,
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
                        stack.push(factory.create_builtin_term(*target));
                        Ok((ExecutionResult::Advance, DependencyList::empty()))
                    }
                    Instruction::PushNative { target } => {
                        let uid = *target;
                        match plugins.get(uid) {
                            Some(target) => {
                                stack.push(factory.create_native_function_term(target.clone()));
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
                            let signal = factory.create_signal_term(allocator.create_signal_list(
                                once(allocator.create_signal(
                                    signal_type.clone(),
                                    allocator.create_list(args),
                                )),
                            ));
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
                        let resume_address = call_stack.get_program_counter().advance();
                        Ok((
                            ExecutionResult::CallFunction {
                                target_address,
                                resume_address,
                            },
                            DependencyList::empty(),
                        ))
                    }
                    Instruction::Apply { num_args } => {
                        let num_args = *num_args;
                        if stack.len() < num_args + 1 {
                            Err(String::from(
                            "Unable to evaluate function application: insufficient arguments on stack",
                        ))
                        } else {
                            let target = stack.pop().unwrap();
                            if let Some((compiled_target, partial_args)) =
                                match_compiled_application_target(&target, factory)
                            {
                                let target_address = compiled_target.address();
                                let resume_address = call_stack.get_program_counter().advance();
                                if let Some(partial_args) = partial_args {
                                    stack.splice(num_args, partial_args.into_iter());
                                }
                                Ok((
                                    ExecutionResult::CallFunction {
                                        target_address,
                                        resume_address,
                                    },
                                    DependencyList::empty(),
                                ))
                            } else {
                                match target.arity() {
                                    None => Err(format!(
                                        "Invalid function application target: {}",
                                        target
                                    )),
                                    Some(arity) => {
                                        let args =
                                            allocator.create_list(stack.pop_multiple(num_args));
                                        let result = match get_short_circuit_signal(
                                            args.as_slice(),
                                            &arity,
                                            factory,
                                            allocator,
                                        ) {
                                            Some(signal) => {
                                                if let Some(_) =
                                                    factory.match_signal_transformer_term(&target)
                                                {
                                                    match target.apply(
                                                        vec![signal],
                                                        factory,
                                                        allocator,
                                                        evaluation_cache,
                                                    ) {
                                                        Ok(result) => result,
                                                        Err(error) => create_error_signal(
                                                            error, factory, allocator,
                                                        ),
                                                    }
                                                } else {
                                                    signal
                                                }
                                            }
                                            None => match target.apply(
                                                args,
                                                factory,
                                                allocator,
                                                evaluation_cache,
                                            ) {
                                                Ok(result) => result,
                                                Err(error) => {
                                                    create_error_signal(error, factory, allocator)
                                                }
                                            },
                                        };
                                        stack.push(result);
                                        Ok((ExecutionResult::Advance, DependencyList::empty()))
                                    }
                                }
                            }
                        }
                    }
                    Instruction::Function { .. } => {
                        Ok((ExecutionResult::Advance, DependencyList::empty()))
                    }
                    Instruction::Evaluate => {
                        let is_executable = match stack.peek() {
                            None => Err(String::from("Missing evaluation target")),
                            Some(target) => Ok(target.is_executable()),
                        };
                        match is_executable {
                            Err(error) => Err(error),
                            Ok(false) => Ok((ExecutionResult::Advance, DependencyList::empty())),
                            Ok(true) => {
                                let target = stack.pop().unwrap();
                                target.execute(state, stack, &call_stack, factory, allocator)
                            }
                        }
                    }
                    Instruction::Return => Ok((ExecutionResult::Return, DependencyList::empty())),
                    Instruction::End => Ok((ExecutionResult::End, DependencyList::empty())),
                    Instruction::ConstructApplication { num_args } => {
                        let num_args = *num_args;
                        if stack.len() < (1 + num_args) {
                            Err(String::from(
                        "Unable to create function application: insufficient arguments on stack",
                    ))
                        } else {
                            let target = stack.pop().unwrap();
                            let args = allocator.create_list(stack.pop_multiple(num_args));
                            let expression = factory.create_application_term(target, args);
                            stack.push(expression);
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
                            let expression = factory.create_partial_application_term(
                                target,
                                allocator.create_list(args),
                            );
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
                    Instruction::ConstructStruct { prototype, eager } => {
                        let size = prototype.keys().len();
                        if stack.len() < size {
                            Err(format!(
                                "Unable to create {}-field struct: insufficient arguments on stack",
                                size,
                            ))
                        } else {
                            let fields = allocator.create_list(stack.pop_multiple(size));
                            let result = if *eager
                                && fields
                                    .iter()
                                    .any(|field| factory.match_signal_term(field).is_some())
                            {
                                factory.create_signal_term(
                                    allocator.create_signal_list(
                                        fields
                                            .iter()
                                            .filter_map(|field| {
                                                match factory.match_signal_term(field) {
                                                    Some(signal) => Some(signal),
                                                    None => None,
                                                }
                                            })
                                            .flat_map(|field| {
                                                field
                                                    .signals()
                                                    .iter()
                                                    .map(|signal| allocator.clone_signal(signal))
                                            }),
                                    ),
                                )
                            } else {
                                factory.create_struct_term(
                                    allocator.clone_struct_prototype(prototype),
                                    fields,
                                )
                            };
                            stack.push(result);
                            Ok((ExecutionResult::Advance, DependencyList::empty()))
                        }
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
                            stack.push(
                                factory.create_signal_term(allocator.create_signal_list(signals)),
                            );
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
        };
        match result {
            Err(error) => return Err(format!("{:x} {}", call_stack.get_program_counter(), error)),
            Ok((result, dependencies)) => {
                call_stack.add_state_dependencies(dependencies.iter());
                match result {
                    ExecutionResult::Repeat => {}
                    ExecutionResult::Advance => {
                        call_stack.update_program_counter(call_stack.advance_program_counter())
                    }
                    ExecutionResult::Jump(next_address) => {
                        call_stack.update_program_counter(next_address)
                    }
                    ExecutionResult::CallFunction {
                        target_address,
                        resume_address,
                    } => {
                        if let Some((target_hash, num_args)) = call_stack
                            .lookup_instruction(target_address)
                            .and_then(|target| match target {
                                Instruction::Function { hash, arity } => Some((hash, *arity)),
                                _ => None,
                            })
                        {
                            let hash = stack.generate_function_call_hash(target_hash, num_args);
                            if let Some(cached_result) = cache.retrieve_result(hash, state) {
                                call_stack.add_subexpression(hash);
                                call_stack
                                    .add_state_dependencies(cached_result.dependencies().iter());
                                stack.pop_multiple(num_args);
                                stack.push(cached_result.result().clone());
                                call_stack.update_program_counter(resume_address);
                            } else {
                                call_stack.call_function(hash, target_address, resume_address);
                            }
                        } else {
                            return Err(format!(
                                "Invalid function call target address: {:x}",
                                target_address
                            ));
                        }
                    }
                    ExecutionResult::CallSubroutine {
                        subroutine,
                        num_args,
                        resume_address,
                    } => {
                        let hash = stack.generate_function_call_hash(&subroutine, num_args);
                        if let Some(cached_result) = cache.retrieve_result(hash, state) {
                            call_stack.add_subexpression(hash);
                            call_stack.add_state_dependencies(cached_result.dependencies().iter());
                            stack.pop_multiple(num_args);
                            stack.push(cached_result.result().clone());
                            call_stack.update_program_counter(resume_address);
                        } else {
                            call_stack.call_subroutine(hash, subroutine, resume_address);
                        }
                    }
                    ExecutionResult::Return => match call_stack.pop_call_stack() {
                        Some((hash, dependencies, subexpressions)) => match stack.peek() {
                            Some(result) => {
                                cache.store_result(
                                    hash,
                                    state,
                                    EvaluationResult::new(result.clone(), dependencies),
                                    subexpressions,
                                );
                            }
                            None => return Err(String::from(
                                "Unable to cache function result: insufficient arguments on stack",
                            )),
                        },
                        None => return Err(String::from("Stack underflow")),
                    },
                    ExecutionResult::End => {
                        match stack.pop() {
                            Some(value) => {
                                let (hash, dependencies, subexpressions) = call_stack.into_parts();
                                let result = EvaluationResult::new(value, dependencies);
                                cache.store_result(hash, state, result.clone(), subexpressions);
                                return Ok(result);
                            }
                            None => return Err(String::from("Program did not return a result")),
                        };
                    }
                }
            }
        };
    }
}

fn match_compiled_application_target<'a, T: Expression>(
    target: &'a T,
    factory: &impl ExpressionFactory<T>,
) -> Option<(
    &'a CompiledFunctionTerm,
    Option<impl IntoIterator<Item = T>>,
)> {
    let mut current_target = target;
    let mut args = Vec::new();
    loop {
        if let Some(partial) = factory.match_partial_application_term(current_target) {
            args.extend(partial.args().iter().rev().cloned());
            current_target = partial.target();
        } else if let Some(compiled_target) = factory.match_compiled_function_term(current_target) {
            let combined_args = if args.is_empty() {
                None
            } else {
                Some(args.into_iter().rev())
            };
            return Some((compiled_target, combined_args));
        } else {
            return None;
        }
    }
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

#[cfg(test)]
mod tests {
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
        let compiler = Compiler::new(CompilerOptions::default(), None);
        let compiled = compiler
            .compile(&expression, CompilerMode::Program, &factory, &allocator)
            .unwrap();
        let mut state = DynamicState::new();
        let (result, _) = execute(
            compiled.program(),
            InstructionPointer::default(),
            &state,
            &factory,
            &allocator,
            compiled.plugins(),
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
            compiled.program(),
            InstructionPointer::default(),
            &state,
            &factory,
            &allocator,
            compiled.plugins(),
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
    fn basic_operation() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();
        let program = Program::new(vec![
            Instruction::PushInt { value: 3 },
            Instruction::Evaluate,
            Instruction::End,
        ]);
        let state = DynamicState::new();
        let (result, _) = execute(
            &program,
            InstructionPointer::default(),
            &state,
            &factory,
            &allocator,
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
            Instruction::End,
        ]);
        let state = DynamicState::new();
        let (result, _) = execute(
            &program,
            InstructionPointer::default(),
            &state,
            &factory,
            &allocator,
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
        let program = Program::new(vec![
            Instruction::PushInt { value: 3 },
            Instruction::PushInt { value: 4 },
            Instruction::PushBuiltin {
                target: BuiltinTerm::Add,
            },
            Instruction::ConstructApplication { num_args: 2 },
            Instruction::Evaluate,
            Instruction::End,
        ]);
        let state = DynamicState::new();
        let (result, _) = execute(
            &program,
            InstructionPointer::default(),
            &state,
            &factory,
            &allocator,
            &NativeFunctionRegistry::default(),
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
        let program = Program::new(vec![
            Instruction::PushBoolean { value: true },
            Instruction::PushInt { value: 3 },
            Instruction::PushInt { value: 4 },
            Instruction::PushBuiltin {
                target: BuiltinTerm::If,
            },
            Instruction::ConstructApplication { num_args: 3 },
            Instruction::Evaluate,
            Instruction::End,
        ]);
        let state = DynamicState::new();
        let (result, _) = execute(
            &program,
            InstructionPointer::default(),
            &state,
            &factory,
            &allocator,
            &NativeFunctionRegistry::default(),
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
        let program = Program::new(vec![
            Instruction::PushBoolean { value: false },
            Instruction::PushInt { value: 3 },
            Instruction::PushInt { value: 4 },
            Instruction::PushBuiltin {
                target: BuiltinTerm::If,
            },
            Instruction::ConstructApplication { num_args: 3 },
            Instruction::Evaluate,
            Instruction::End,
        ]);
        let state = DynamicState::new();
        let (result, _) = execute(
            &program,
            InstructionPointer::default(),
            &state,
            &factory,
            &allocator,
            &NativeFunctionRegistry::default(),
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
        let program = Program::new(vec![
            Instruction::PushInt { value: 3 },
            Instruction::PushInt { value: 4 },
            Instruction::PushBuiltin {
                target: BuiltinTerm::Add,
            },
            Instruction::ConstructApplication { num_args: 2 },
            Instruction::PushInt { value: 5 },
            Instruction::PushBuiltin {
                target: BuiltinTerm::Add,
            },
            Instruction::ConstructApplication { num_args: 2 },
            Instruction::Evaluate,
            Instruction::End,
        ]);
        let state = DynamicState::new();
        let (result, _) = execute(
            &program,
            InstructionPointer::default(),
            &state,
            &factory,
            &allocator,
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
}
