// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::iter::{empty, once};

use reflex::{
    cache::NoopCache,
    core::{
        Applicable, ApplicationTermType, ArgType, Arity, BooleanTermType, Builtin, BuiltinTermType,
        CompiledFunctionTermType, ConditionListType, ConditionType, ConstructorTermType, Eagerness,
        EffectTermType, Expression, ExpressionFactory, ExpressionListType, FloatTermType,
        HashmapTermType, HashsetTermType, HeapAllocator, InstructionPointer, IntTermType,
        LambdaTermType, LetTermType, ListTermType, PartialApplicationTermType, RecordTermType,
        RecursiveTermType, Reducible, RefType, Rewritable, SignalTermType, StackOffset,
        StringTermType, StringValue, StructPrototypeType, Substitutions, SymbolTermType, Uid,
        VariableTermType,
    },
    hash::{hash_object, HashId},
};
use reflex_lang::{
    expression::{CachedExpression, SharedExpression},
    term::{
        ApplicationTerm, BooleanTerm, BuiltinTerm, CompiledFunctionTerm, ConstructorTerm,
        EffectTerm, FloatTerm, HashMapTerm, HashSetTerm, IntTerm, LambdaTerm, LetTerm, ListTerm,
        NilTerm, PartialApplicationTerm, RecordTerm, RecursiveTerm, SignalTerm, StringTerm,
        SymbolTerm, Term, VariableTerm,
    },
    CachedSharedTerm,
};

use crate::compiler::{compile_expressions, Compile, Compiler, Instruction, Program};

impl<TInner, TWrapper> Compile<TWrapper> for CachedExpression<TInner>
where
    TInner: Compile<TWrapper>,
    TWrapper: Expression + Compile<TWrapper>,
{
    fn compile(
        &self,
        eager: Eagerness,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<TWrapper>,
        allocator: &impl HeapAllocator<TWrapper>,
        compiler: &mut Compiler,
    ) -> Result<Program, String> {
        self.value()
            .compile(eager, stack_offset, factory, allocator, compiler)
    }
}

impl<TInner, TWrapper> Compile<TWrapper> for SharedExpression<TInner>
where
    TInner: Compile<TWrapper>,
    TWrapper: Expression + Compile<TWrapper>,
{
    fn compile(
        &self,
        eager: Eagerness,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<TWrapper>,
        allocator: &impl HeapAllocator<TWrapper>,
        compiler: &mut Compiler,
    ) -> Result<Program, String> {
        self.value()
            .compile(eager, stack_offset, factory, allocator, compiler)
    }
}

impl<TBuiltin: Builtin> Compile<Self> for CachedSharedTerm<TBuiltin> {
    fn compile(
        &self,
        eager: Eagerness,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<Self>,
        allocator: &impl HeapAllocator<Self>,
        compiler: &mut Compiler,
    ) -> Result<Program, String> {
        self.value()
            .compile(eager, stack_offset, factory, allocator, compiler)
    }
}

impl<T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>> Compile<T>
    for Term<T>
where
    T::String: std::hash::Hash,
{
    fn compile(
        &self,
        eager: Eagerness,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<Program, String> {
        match self {
            Self::Nil(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::Boolean(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::Int(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::Float(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::String(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::Symbol(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::Variable(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::Effect(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::Let(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::Lambda(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::Application(term) => {
                term.compile(eager, stack_offset, factory, allocator, compiler)
            }
            Self::PartialApplication(term) => {
                term.compile(eager, stack_offset, factory, allocator, compiler)
            }
            Self::Recursive(term) => {
                term.compile(eager, stack_offset, factory, allocator, compiler)
            }
            Self::CompiledFunction(term) => {
                term.compile(eager, stack_offset, factory, allocator, compiler)
            }
            Self::Builtin(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::Record(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::Constructor(term) => {
                term.compile(eager, stack_offset, factory, allocator, compiler)
            }
            Self::List(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::HashMap(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::HashSet(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::Signal(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
        }
    }
}

impl<T: Expression + Applicable<T> + Compile<T>> Compile<T> for ApplicationTerm<T> {
    fn compile(
        &self,
        eager: Eagerness,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<Program, String> {
        let target = self.target().as_deref();
        let args = self.args().as_deref();
        let num_args = args.len();
        let compiled_target =
            compiler.compile_term(target, eager, stack_offset + num_args, factory, allocator)?;
        match eager {
            Eagerness::Lazy => {
                let compiled_args = compile_args(
                    args.iter()
                        .map(|item| item.as_deref())
                        .map(|arg| (arg, ArgType::Lazy)),
                    stack_offset,
                    factory,
                    allocator,
                    compiler,
                )?;
                let mut result = compiled_args;
                result.extend(compiled_target);
                result.push(Instruction::ConstructApplication { num_args });
                Ok(result)
            }
            Eagerness::Eager => {
                let arity = target
                    .arity()
                    .unwrap_or_else(|| Arity::lazy(num_args, 0, false));
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
                    let compiled_args = compile_args(
                        args.iter().map(|item| item.as_deref()).zip(arity.iter()),
                        stack_offset,
                        factory,
                        allocator,
                        compiler,
                    )?;
                    if let Some((target_address, target_hash)) =
                        match_compiled_function_result(&compiled_target, compiler)
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

impl<T: Expression + Compile<T>> Compile<T> for BooleanTerm {
    fn compile(
        &self,
        _eager: Eagerness,
        _stack_offset: StackOffset,
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _compiler: &mut Compiler,
    ) -> Result<Program, String> {
        Ok(Program::new(once(Instruction::PushBoolean {
            value: self.value(),
        })))
    }
}

impl<T: Expression> Compile<T> for BuiltinTerm<T> {
    fn compile(
        &self,
        _eager: Eagerness,
        _stack_offset: StackOffset,
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _compiler: &mut Compiler,
    ) -> Result<Program, String> {
        Ok(Program::new(once(Instruction::PushBuiltin {
            target: self.target().uid(),
        })))
    }
}

impl<T: Expression + Compile<T>> Compile<T> for CompiledFunctionTerm {
    fn compile(
        &self,
        _eager: Eagerness,
        _stack_offset: StackOffset,
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _compiler: &mut Compiler,
    ) -> Result<Program, String> {
        Ok(Program::new(once(Instruction::PushFunction {
            target: self.address(),
            hash: self.hash(),
        })))
    }
}

impl<T: Expression + Compile<T>> Compile<T> for ConstructorTerm<T> {
    fn compile(
        &self,
        _eager: Eagerness,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<Program, String> {
        let keys = self.prototype().as_deref().keys().as_deref();
        let compiled_keys = compile_expressions(
            keys.iter().map(|item| item.as_deref()),
            Eagerness::Eager,
            stack_offset,
            factory,
            allocator,
            compiler,
        )?;
        let mut result = compiled_keys;
        result.push(Instruction::ConstructConstructor {
            num_fields: keys.len(),
        });
        Ok(result)
    }
}

impl<T: Expression + Compile<T>> Compile<T> for EffectTerm<T> {
    fn compile(
        &self,
        _eager: Eagerness,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<Program, String> {
        let compiled_condition = compile_signal(
            self.condition().as_deref(),
            stack_offset,
            factory,
            allocator,
            compiler,
        )?;
        let mut result = compiled_condition;
        result.push(Instruction::LoadEffect);
        Ok(result)
    }
}

impl<T: Expression + Compile<T>> Compile<T> for FloatTerm {
    fn compile(
        &self,
        _eager: Eagerness,
        _stack_offset: StackOffset,
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _compiler: &mut Compiler,
    ) -> Result<Program, String> {
        Ok(Program::new(once(Instruction::PushFloat {
            value: self.value().into(),
        })))
    }
}

impl<T: Expression + Compile<T>> Compile<T> for HashMapTerm<T> {
    fn compile(
        &self,
        _eager: Eagerness,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<Program, String> {
        let keys = self.keys();
        let values = self.values();
        let num_entries = keys.len();
        let keys_chunk = compile_expressions(
            keys.map(|item| item.as_deref()),
            Eagerness::Eager,
            stack_offset,
            factory,
            allocator,
            compiler,
        )?;
        let values_chunk = compile_expressions(
            values.map(|item| item.as_deref()),
            Eagerness::Lazy,
            stack_offset,
            factory,
            allocator,
            compiler,
        )?;
        let mut program = keys_chunk;
        program.extend(values_chunk);
        program.push(Instruction::ConstructHashMap { size: num_entries });
        Ok(program)
    }
}

impl<T: Expression + Compile<T>> Compile<T> for HashSetTerm<T> {
    fn compile(
        &self,
        _eager: Eagerness,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<Program, String> {
        let values = self.values();
        let num_values = values.len();
        compile_expressions(
            values.map(|item| item.as_deref()),
            Eagerness::Lazy,
            stack_offset,
            factory,
            allocator,
            compiler,
        )
        .map(|mut program| {
            program.push(Instruction::ConstructHashSet { size: num_values });
            program
        })
    }
}

impl<T: Expression + Compile<T>> Compile<T> for IntTerm {
    fn compile(
        &self,
        _eager: Eagerness,
        _stack_offset: StackOffset,
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _compiler: &mut Compiler,
    ) -> Result<Program, String> {
        Ok(Program::new(once(Instruction::PushInt {
            value: self.value(),
        })))
    }
}

impl<T: Expression + Compile<T>> Compile<T> for LambdaTerm<T> {
    fn compile(
        &self,
        _eager: Eagerness,
        _stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<Program, String> {
        let hash = hash_object(self);
        let body = self.body().as_deref();
        let num_args = self.num_args();
        let target_address = match compiler.retrieve_compiled_chunk_address(hash) {
            Some(address) => address,
            None => {
                let compiled_body =
                    compiler.compile_term(body, Eagerness::Eager, 0, factory, allocator)?;
                compiler.store_compiled_chunk(
                    hash,
                    Program::new(
                        once(Instruction::Function {
                            hash,
                            required_args: num_args,
                            optional_args: 0,
                        })
                        .chain(compiled_body)
                        .chain(if num_args > 0 {
                            Some(Instruction::Squash { depth: num_args })
                        } else {
                            None
                        })
                        .chain(once(Instruction::Return)),
                    ),
                )
            }
        };
        Ok(Program::new(once(Instruction::PushFunction {
            target: target_address,
            hash,
        })))
    }
}

impl<T: Expression + Rewritable<T> + Reducible<T> + Compile<T>> Compile<T> for LetTerm<T> {
    fn compile(
        &self,
        eager: Eagerness,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<Program, String> {
        let compiled_initializer = compiler.compile_term(
            self.initializer().as_deref(),
            eager,
            stack_offset,
            factory,
            allocator,
        )?;
        let compiled_body = {
            // Expressions encountered as the (non-first) argument of a function application will have a stack_offset
            // greater than 0, indicating that any stack offsets within the expression will need to be shifted to
            // accommodate the previous arguments which will be occupying slots in the stack at the point of evaluation.
            // Usually this is just a case of relaying the stack offset to any child expressions, which will cause their
            // respective stack offsets to be shifted by the given argument offset, however let-expressions introduce
            // their own child scope that assumes stack offset 0 refers to the initializer and stack offset 1 refers to
            // the most recently-defined variable in the parent scope.
            // This creates problems when there are arguments occupying the slots in between the parent scope and the
            // let-expression inner scope, seeing as offsets 0 and 1 within the inner scope now occupy non-contiguous
            // slots in the interpreter stack.
            // One approach to address this is to manually shift all variables with offset > 0 within the let-expression
            // body and then compile it with a stack offset of 0 to reflect the fact that any variables have already
            // been offset by the correct amount (this is what we do here).
            // A more thorough approach might be to allow an arbitrary function for the stack_offset argument, which
            // would allow returning non-contiguous offsets for variables declared in parent scopes.
            let inner_stack_offset = 0;
            let shifted_body = if stack_offset > 0 {
                self.body().as_deref().substitute_static(
                    &Substitutions::increase_scope_offset(stack_offset, 1),
                    factory,
                    allocator,
                    &mut NoopCache::default(),
                )
            } else {
                None
            };
            compiler.compile_term(
                shifted_body.as_ref().unwrap_or(self.body().as_deref()),
                eager,
                inner_stack_offset,
                factory,
                allocator,
            )
        }?;
        let mut program = compiled_initializer;
        program.extend(compiled_body);
        program.push(Instruction::Squash { depth: 1 });
        Ok(program)
    }
}

impl<T: Expression + Compile<T>> Compile<T> for ListTerm<T> {
    fn compile(
        &self,
        _eager: Eagerness,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<Program, String> {
        let items = self.items().as_deref();
        compile_expressions(
            items.iter().map(|item| item.as_deref()),
            Eagerness::Lazy,
            stack_offset,
            factory,
            allocator,
            compiler,
        )
        .map(|mut program| {
            program.push(Instruction::ConstructList { size: items.len() });
            program
        })
    }
}

impl<T: Expression + Compile<T>> Compile<T> for NilTerm {
    fn compile(
        &self,
        _eager: Eagerness,
        _stack_offset: StackOffset,
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _compiler: &mut Compiler,
    ) -> Result<Program, String> {
        Ok(Program::new(once(Instruction::PushNil)))
    }
}

impl<T: Expression + Applicable<T> + Compile<T>> Compile<T> for PartialApplicationTerm<T> {
    fn compile(
        &self,
        eager: Eagerness,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<Program, String> {
        let target = self.target().as_deref();
        let args = self.args().as_deref();
        let num_args = args.len();
        let arity = match eager {
            Eagerness::Eager => target.arity(),
            Eagerness::Lazy => None,
        }
        .unwrap_or_else(|| Arity::lazy(num_args, 0, false));
        let compiled_target =
            compiler.compile_term(target, eager, stack_offset + num_args, factory, allocator)?;
        let compiled_args = compile_args(
            args.iter().map(|item| item.as_deref()).zip(arity.iter()),
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

impl<T: Expression + Compile<T>> Compile<T> for RecursiveTerm<T> {
    fn compile(
        &self,
        eager: Eagerness,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<Program, String> {
        let compiled_factory = compiler.compile_term(
            self.factory().as_deref(),
            eager,
            stack_offset,
            factory,
            allocator,
        )?;
        let mut result = compiled_factory;
        result.push(Instruction::PushLocal { offset: 0 });
        result.push(Instruction::Apply { num_args: 1 });
        Ok(result)
    }
}

impl<T: Expression + Rewritable<T> + Reducible<T> + Compile<T>> Compile<T> for SignalTerm<T> {
    fn compile(
        &self,
        _eager: Eagerness,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<Program, String> {
        self.signals()
            .as_deref()
            .iter()
            .map(|item| item.as_deref())
            .enumerate()
            .fold(Ok(Program::new(empty())), |program, (index, signal)| {
                let mut program = program?;
                match compile_signal(signal, stack_offset + index, factory, allocator, compiler) {
                    Err(error) => Err(error),
                    Ok(compiled_signal) => {
                        program.extend(compiled_signal);
                        Ok(program)
                    }
                }
            })
    }
}
impl<T: Expression + Rewritable<T> + Reducible<T> + Compile<T>> Compile<T> for RecordTerm<T> {
    fn compile(
        &self,
        _eager: Eagerness,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<Program, String> {
        let values = self.values().as_deref();
        let num_entries = values.len();
        let compiled_values = compile_expressions(
            values.iter().map(|item| item.as_deref()),
            Eagerness::Lazy,
            stack_offset,
            factory,
            allocator,
            compiler,
        )?;
        let compiled_constructor = compiler.compile_term(
            &factory.create_constructor_term(allocator.clone_struct_prototype(self.prototype())),
            Eagerness::Eager,
            num_entries,
            factory,
            allocator,
        )?;
        let mut result = compiled_values;
        result.extend(compiled_constructor);
        result.push(Instruction::Apply {
            num_args: num_entries,
        });
        Ok(result)
    }
}

impl<T: Expression + Compile<T>> Compile<T> for StringTerm<T> {
    fn compile(
        &self,
        _eager: Eagerness,
        _stack_offset: StackOffset,
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _compiler: &mut Compiler,
    ) -> Result<Program, String> {
        Ok(Program::new(once(Instruction::PushString {
            value: Into::<String>::into(self.value().as_deref().as_str()),
        })))
    }
}

impl<T: Expression + Compile<T>> Compile<T> for SymbolTerm {
    fn compile(
        &self,
        _eager: Eagerness,
        _stack_offset: StackOffset,
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _compiler: &mut Compiler,
    ) -> Result<Program, String> {
        Ok(Program::new(once(Instruction::PushSymbol {
            id: self.id(),
        })))
    }
}

impl<T: Expression + Compile<T>> Compile<T> for VariableTerm {
    fn compile(
        &self,
        _eager: Eagerness,
        stack_offset: StackOffset,
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _compiler: &mut Compiler,
    ) -> Result<Program, String> {
        Ok(Program::new(once(Instruction::PushLocal {
            offset: self.offset() + stack_offset,
        })))
    }
}

fn compile_args<'a, T: Expression + Compile<T> + 'a>(
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
            match compiler.compile_term(
                arg,
                match arg_type {
                    ArgType::Eager | ArgType::Strict => Eagerness::Eager,
                    ArgType::Lazy => Eagerness::Lazy,
                },
                stack_offset + index,
                factory,
                allocator,
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

fn compile_signal<T: Expression + Compile<T>>(
    signal: &T::Signal,
    stack_offset: StackOffset,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    compiler: &mut Compiler,
) -> Result<Program, String> {
    let compiled_token = compiler.compile_term(
        signal.token().as_deref(),
        Eagerness::Lazy,
        stack_offset,
        factory,
        allocator,
    )?;
    let compiled_payload = compiler.compile_term(
        signal.payload().as_deref(),
        Eagerness::Lazy,
        stack_offset,
        factory,
        allocator,
    )?;
    let mut result = compiled_token;
    result.extend(compiled_payload);
    result.push(Instruction::ConstructCondition {
        signal_type: signal.signal_type().clone(),
    });
    Ok(result)
}

fn match_compiled_function_result(
    program: &Program,
    compiler: &Compiler,
) -> Option<(InstructionPointer, HashId)> {
    match program.instructions().first() {
        Some(&Instruction::PushFunction { target, hash }) if program.len() == 1 => {
            Some((target, hash))
        }
        Some(&Instruction::LoadStaticData { offset }) => compiler
            .get_data_section_item(offset)
            .and_then(|instructions| match_compiled_function_result(instructions, compiler)),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use reflex::core::{DependencyList, EvaluationResult, InstructionPointer, NodeId, StateCache};
    use reflex_lang::{allocator::DefaultAllocator, SharedTermFactory};
    use reflex_stdlib::{Abs, Add, If, Stdlib};

    use crate::{
        compiler::{
            hash_compiled_program, CompiledProgram, Compiler, CompilerMode, CompilerOptions,
        },
        execute, DefaultInterpreterCache, InterpreterOptions,
    };

    use super::*;

    #[test]
    fn constructor_interning() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let constructor = factory.create_constructor_term(allocator.create_struct_prototype(
            allocator.create_triple(
                factory.create_string_term(allocator.create_static_string("foo")),
                factory.create_string_term(allocator.create_static_string("bar")),
                factory.create_string_term(allocator.create_static_string("baz")),
            ),
        ));
        let expression = factory.create_application_term(
            constructor.clone(),
            allocator.create_triple(
                factory.create_int_term(3),
                factory.create_int_term(4),
                factory.create_int_term(5),
            ),
        );
        let program = Compiler::new(
            CompilerOptions {
                inline_static_data: true,
                ..CompilerOptions::unoptimized()
            },
            None,
        )
        .compile(&expression, CompilerMode::Expression, &factory, &allocator)
        .unwrap();
        assert_eq!(
            program,
            CompiledProgram {
                instructions: Program::new([
                    Instruction::LoadStaticData { offset: 4 },
                    Instruction::LoadStaticData { offset: 5 },
                    Instruction::LoadStaticData { offset: 6 },
                    Instruction::LoadStaticData { offset: 3 },
                    Instruction::Apply { num_args: 3 }
                ]),
                data_section: vec![
                    (
                        factory
                            .create_string_term(allocator.create_static_string("foo"))
                            .id(),
                        Program::new([
                            Instruction::PushString {
                                value: String::from("foo")
                            },
                            Instruction::Return
                        ])
                    ),
                    (
                        factory
                            .create_string_term(allocator.create_static_string("bar"))
                            .id(),
                        Program::new([
                            Instruction::PushString {
                                value: String::from("bar")
                            },
                            Instruction::Return
                        ])
                    ),
                    (
                        factory
                            .create_string_term(allocator.create_static_string("baz"))
                            .id(),
                        Program::new([
                            Instruction::PushString {
                                value: String::from("baz")
                            },
                            Instruction::Return
                        ])
                    ),
                    (
                        constructor.id(),
                        Program::new([
                            Instruction::LoadStaticData { offset: 0 },
                            Instruction::LoadStaticData { offset: 1 },
                            Instruction::LoadStaticData { offset: 2 },
                            Instruction::ConstructConstructor { num_fields: 3 },
                            Instruction::Return
                        ])
                    ),
                    (
                        factory.create_int_term(3).id(),
                        Program::new([Instruction::PushInt { value: 3 }, Instruction::Return])
                    ),
                    (
                        factory.create_int_term(4).id(),
                        Program::new([Instruction::PushInt { value: 4 }, Instruction::Return])
                    ),
                    (
                        factory.create_int_term(5).id(),
                        Program::new([Instruction::PushInt { value: 5 }, Instruction::Return])
                    ),
                ],
            }
        );
    }

    #[test]
    fn compiled_let_expression_scoping() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let mut cache = DefaultInterpreterCache::default();

        let expression =
            factory.create_let_term(
                factory.create_int_term(3),
                factory.create_application_term(
                    factory.create_builtin_term(Abs),
                    allocator.create_unit_list(factory.create_let_term(
                        factory.create_nil_term(),
                        factory.create_variable_term(1),
                    )),
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

        let expression = factory.create_let_term(
            factory.create_int_term(3),
            factory.create_application_term(
                factory.create_builtin_term(Add),
                allocator.create_pair(
                    factory.create_int_term(4),
                    factory.create_let_term(
                        factory.create_nil_term(),
                        factory.create_variable_term(1),
                    ),
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
            EvaluationResult::new(factory.create_int_term(3 + 4), DependencyList::empty(),),
        );

        let expression = factory.create_let_term(
            factory.create_int_term(3),
            factory.create_application_term(
                factory.create_builtin_term(If),
                allocator.create_triple(
                    factory.create_boolean_term(false),
                    factory.create_nil_term(),
                    factory.create_application_term(
                        factory.create_builtin_term(Add),
                        allocator.create_pair(
                            factory.create_int_term(4),
                            factory.create_let_term(
                                factory.create_nil_term(),
                                factory.create_variable_term(1),
                            ),
                        ),
                    ),
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
            EvaluationResult::new(factory.create_int_term(3 + 4), DependencyList::empty(),),
        );

        let expression = factory.create_application_term(
            factory.create_let_term(
                factory.create_lambda_term(1, factory.create_variable_term(0)),
                factory.create_lambda_term(
                    1,
                    factory.create_application_term(
                        factory.create_builtin_term(Abs),
                        allocator.create_unit_list(factory.create_application_term(
                            factory.create_variable_term(1),
                            allocator.create_unit_list(factory.create_variable_term(0)),
                        )),
                    ),
                ),
            ),
            allocator.create_unit_list(factory.create_int_term(3)),
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
    }
}
