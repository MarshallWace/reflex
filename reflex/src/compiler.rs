// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{BTreeMap, HashMap},
    convert::identity,
    iter::{empty, once, FromIterator},
};

use crate::{
    cache::SubstitutionCache,
    core::{
        Applicable, Expression, ExpressionFactory, HeapAllocator, Reducible, Rewritable,
        SignalType, StackOffset, StateToken, StructPrototype, VarArgs,
    },
    hash::{combine_hashes, hash_object, HashId},
    lang::{
        compile_builtin_function, compile_native_function, BuiltinTerm, FloatValue, IntValue,
        NativeFunction, NativeFunctionId, SymbolId,
    },
};
use strum::IntoEnumIterator;

pub trait Compile<T: Expression + Compile<T>> {
    fn compile(
        &self,
        eager: VarArgs,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<(Program, NativeFunctionRegistry<T>), String>;
}

#[derive(Hash, PartialEq, Clone, Debug)]
pub struct Program {
    instructions: Vec<Instruction>,
}
impl Program {
    pub fn new(instructions: impl IntoIterator<Item = Instruction>) -> Self {
        Self {
            instructions: instructions.into_iter().collect(),
        }
    }
    pub fn instructions(&self) -> &[Instruction] {
        &self.instructions
    }
    pub fn get(&self, address: InstructionPointer) -> Option<&Instruction> {
        self.instructions.get(address.0)
    }
    pub fn len(&self) -> usize {
        self.instructions.len()
    }
    pub fn into_instructions(self) -> Vec<Instruction> {
        self.instructions
    }
    pub fn push(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }
    pub fn extend(&mut self, instructions: impl IntoIterator<Item = Instruction>) {
        self.instructions.extend(instructions)
    }
}
impl Eq for Program {}
impl IntoIterator for Program {
    type Item = Instruction;
    type IntoIter = std::vec::IntoIter<Instruction>;
    fn into_iter(self) -> Self::IntoIter {
        self.instructions.into_iter()
    }
}
impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.instructions
                .iter()
                .enumerate()
                .map(|(offset, instruction)| { format!("0x{:08x} {:?}", offset, instruction) })
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Instruction {
    PushStatic {
        offset: StackOffset,
    },
    PushDynamic {
        state_token: StateToken,
    },
    PushNull,
    PushBoolean {
        value: bool,
    },
    PushInt {
        value: IntValue,
    },
    PushFloat {
        value: FloatValue,
    },
    PushString {
        value: String,
    },
    PushSymbol {
        value: SymbolId,
    },
    PushHash {
        value: HashId,
    },
    PushFunction {
        target: InstructionPointer,
    },
    PushBuiltin {
        target: BuiltinTerm,
    },
    PushNative {
        target: NativeFunctionId,
    },
    PushConstructor {
        prototype: StructPrototype,
    },
    PushSignal {
        signal_type: SignalType,
        num_args: usize,
    },
    Pop {
        count: usize,
    },
    Squash {
        depth: StackOffset,
    },
    Move {
        offset: StackOffset,
    },
    Jump {
        target: InstructionPointer,
    },
    Call {
        target: InstructionPointer,
    },
    Apply {
        num_args: usize,
    },
    Function {
        hash: HashId,
        arity: usize,
        variadic: bool,
    },
    Return,
    Evaluate,
    EvaluateArgList,
    ConstructApplication {
        num_args: usize,
    },
    ConstructPartialApplication {
        num_args: usize,
    },
    ConstructTuple {
        size: usize,
    },
    ConstructVector {
        size: usize,
    },
    ConstructHashMap {
        size: usize,
    },
    ConstructHashSet {
        size: usize,
    },
    CombineSignals {
        count: usize,
    },
    ConstructSignalTransformer,
}
impl std::hash::Hash for Instruction {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::PushStatic { offset } => {
                state.write_u8(0);
                offset.hash(state);
            }
            Self::PushDynamic { state_token } => {
                state.write_u8(1);
                state_token.hash(state);
            }
            Self::PushNull => {
                state.write_u8(2);
            }
            Self::PushBoolean { value } => {
                state.write_u8(3);
                value.hash(state);
            }
            Self::PushInt { value } => {
                state.write_u8(4);
                value.hash(state);
            }
            Self::PushFloat { value } => {
                state.write_u8(5);
                state.write(&value.to_be_bytes())
            }
            Self::PushString { value } => {
                state.write_u8(6);
                value.hash(state);
            }
            Self::PushSymbol { value } => {
                state.write_u8(7);
                value.hash(state);
            }
            Self::PushHash { value } => {
                state.write_u8(8);
                value.hash(state);
            }
            Self::PushFunction { target } => {
                state.write_u8(9);
                target.hash(state);
            }
            Self::PushBuiltin { target } => {
                state.write_u8(10);
                target.hash(state);
            }
            Self::PushNative { target } => {
                state.write_u8(11);
                target.hash(state);
            }
            Self::PushConstructor { prototype } => {
                state.write_u8(11);
                prototype.hash(state);
            }
            Self::PushSignal {
                signal_type,
                num_args,
            } => {
                state.write_u8(12);
                signal_type.hash(state);
                num_args.hash(state);
            }
            Self::Pop { count } => {
                state.write_u8(13);
                count.hash(state);
            }
            Self::Squash { depth } => {
                state.write_u8(14);
                depth.hash(state);
            }
            Self::Move { offset } => {
                state.write_u8(15);
                offset.hash(state);
            }
            Self::Jump { target } => {
                state.write_u8(16);
                target.hash(state);
            }
            Self::Call { target } => {
                state.write_u8(17);
                target.hash(state);
            }
            Self::Apply { num_args } => {
                state.write_u8(18);
                num_args.hash(state);
            }
            Self::Function {
                hash,
                arity,
                variadic,
            } => {
                state.write_u8(19);
                hash.hash(state);
                arity.hash(state);
                variadic.hash(state);
            }
            Self::Return => {
                state.write_u8(20);
            }
            Self::Evaluate => {
                state.write_u8(21);
            }
            Self::EvaluateArgList => {
                state.write_u8(22);
            }
            Self::ConstructApplication { num_args } => {
                state.write_u8(23);
                num_args.hash(state);
            }
            Self::ConstructPartialApplication { num_args } => {
                state.write_u8(24);
                num_args.hash(state);
            }
            Self::ConstructTuple { size } => {
                state.write_u8(25);
                size.hash(state);
            }
            Self::ConstructVector { size } => {
                state.write_u8(27);
                size.hash(state);
            }
            Self::ConstructHashMap { size } => {
                state.write_u8(28);
                size.hash(state);
            }
            Self::ConstructHashSet { size } => {
                state.write_u8(29);
                size.hash(state);
            }
            Self::CombineSignals { count } => {
                state.write_u8(30);
                count.hash(state);
            }
            Self::ConstructSignalTransformer => {
                state.write_u8(31);
            }
        }
    }
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Default, Clone, Copy)]
pub struct InstructionPointer(usize);
impl InstructionPointer {
    pub fn new(address: usize) -> Self {
        Self(address)
    }
    pub fn get(&self) -> usize {
        self.0
    }
    pub fn advance(&self) -> Self {
        self.offset(1)
    }
    fn offset(&self, offset: usize) -> Self {
        Self(self.0 + offset)
    }
}
impl std::fmt::LowerHex for InstructionPointer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "0x{:08x}", self.0)
    }
}
impl std::fmt::UpperHex for InstructionPointer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "0x{:08X}", self.0)
    }
}
impl std::fmt::Debug for InstructionPointer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<address:{:x}>", self)
    }
}

#[derive(PartialEq, Debug)]
pub struct NativeFunctionRegistry<T: Expression> {
    values: HashMap<NativeFunctionId, (NativeFunction<T>, InstructionPointer)>,
}
impl<T: Expression> Default for NativeFunctionRegistry<T> {
    fn default() -> Self {
        Self {
            values: HashMap::new(),
        }
    }
}
impl<T: Expression> NativeFunctionRegistry<T> {
    pub fn from(entry: (NativeFunction<T>, InstructionPointer)) -> Self {
        let (target, address) = entry;
        Self {
            values: HashMap::from_iter(once((target.uid(), (target, address)))),
        }
    }
    pub fn get(&self, uid: NativeFunctionId) -> Option<&NativeFunction<T>> {
        self.values.get(&uid).map(|(target, _)| target)
    }
    pub fn len(&self) -> usize {
        self.values.len()
    }
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }
    pub fn iter(&self) -> impl Iterator<Item = &(NativeFunction<T>, InstructionPointer)> {
        self.values.values()
    }
    pub fn add(&mut self, target: NativeFunction<T>, address: InstructionPointer) {
        self.values.insert(target.uid(), (target, address));
    }
    pub fn extend(
        &mut self,
        entries: impl IntoIterator<Item = (NativeFunction<T>, InstructionPointer)>,
    ) {
        for (target, address) in entries.into_iter() {
            self.add(target, address);
        }
    }
    pub fn union(self, other: NativeFunctionRegistry<T>) -> Self {
        if self.is_empty() {
            other
        } else if other.is_empty() {
            self
        } else {
            let mut combined_values = self.values;
            combined_values.extend(other.values);
            Self {
                values: combined_values,
            }
        }
    }
}
impl<T: Expression> FromIterator<(NativeFunction<T>, InstructionPointer)>
    for NativeFunctionRegistry<T>
{
    fn from_iter<TIter: IntoIterator<Item = (NativeFunction<T>, InstructionPointer)>>(
        iter: TIter,
    ) -> Self {
        Self {
            values: iter
                .into_iter()
                .map(|(target, address)| (target.uid(), (target, address)))
                .collect(),
        }
    }
}
impl<T: Expression> IntoIterator for NativeFunctionRegistry<T> {
    type Item = (NativeFunction<T>, InstructionPointer);
    type IntoIter = std::collections::hash_map::IntoValues<NativeFunctionId, Self::Item>;
    fn into_iter(self) -> Self::IntoIter {
        self.values.into_values()
    }
}

#[derive(Clone, Copy, Debug)]
pub struct CompilerOptions {
    pub debug: bool,
    pub hoist_free_variables: bool,
    pub normalize: bool,
}
impl CompilerOptions {
    pub fn unoptimized() -> Self {
        Self {
            debug: false,
            hoist_free_variables: true,
            normalize: false,
        }
    }
    pub fn debug() -> Self {
        Self {
            debug: true,
            hoist_free_variables: true,
            normalize: true,
        }
    }
}
impl Default for CompilerOptions {
    fn default() -> Self {
        Self {
            debug: false,
            hoist_free_variables: true,
            normalize: true,
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct CompilerOutput<T: Expression> {
    program: Program,
    builtins: Vec<(BuiltinTerm, InstructionPointer)>,
    plugins: NativeFunctionRegistry<T>,
}
impl<T: Expression> CompilerOutput<T> {
    pub fn new(
        program: Program,
        builtins: impl IntoIterator<Item = (BuiltinTerm, InstructionPointer)>,
        plugins: NativeFunctionRegistry<T>,
    ) -> Self {
        Self {
            program,
            builtins: builtins.into_iter().collect(),
            plugins,
        }
    }
    pub fn program(&self) -> &Program {
        &self.program
    }
    pub fn builtins(&self) -> impl IntoIterator<Item = (BuiltinTerm, InstructionPointer)> + '_ {
        self.builtins.iter().copied()
    }
    pub fn plugins(&self) -> &NativeFunctionRegistry<T> {
        &self.plugins
    }
    pub fn into_parts(
        self,
    ) -> (
        Program,
        Vec<(BuiltinTerm, InstructionPointer)>,
        NativeFunctionRegistry<T>,
    ) {
        (self.program, self.builtins, self.plugins)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum CompilerMode {
    Expression,
    Thunk,
}

pub struct Compiler {
    options: CompilerOptions,
    next_chunk_address: InstructionPointer,
    prelude: Option<Program>,
    compiled_chunks: HashMap<HashId, (InstructionPointer, Program)>,
}
impl Compiler {
    pub fn new(options: CompilerOptions, prelude: Option<Program>) -> Self {
        Self {
            options,
            next_chunk_address: InstructionPointer::default(),
            prelude,
            compiled_chunks: HashMap::new(),
        }
    }
    pub fn options(&self) -> &CompilerOptions {
        &self.options
    }
    pub(crate) fn retrieve_compiled_chunk_address(&self, id: HashId) -> Option<InstructionPointer> {
        self.compiled_chunks.get(&id).map(|(address, _)| *address)
    }
    pub(crate) fn store_compiled_chunk(
        &mut self,
        id: HashId,
        instructions: Program,
    ) -> InstructionPointer {
        let chunk_address = self.next_chunk_address;
        self.next_chunk_address = chunk_address.advance();
        self.compiled_chunks
            .insert(id, (chunk_address, instructions));
        chunk_address
    }
    pub fn compile<T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>>(
        mut self,
        expression: &T,
        mode: CompilerMode,
        builtins: bool,
        plugins: impl IntoIterator<Item = NativeFunction<T>>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Result<CompilerOutput<T>, String> {
        let optimized_expression = match self.options.hoist_free_variables {
            false => None,
            true => expression.hoist_free_variables(factory, allocator),
        };
        let optimized_expression = match self.options.normalize {
            false => optimized_expression,
            true => optimized_expression
                .as_ref()
                .unwrap_or(expression)
                .normalize(factory, allocator, &mut SubstitutionCache::new())
                .or(optimized_expression),
        };
        let (compiled_expression, mut compiled_plugins) = optimized_expression
            .as_ref()
            .unwrap_or(expression)
            .compile(VarArgs::Eager, 0, factory, allocator, &mut self)?;
        let program = match mode {
            CompilerMode::Thunk => Program::new(
                once(Instruction::Function {
                    hash: hash_object(&compiled_expression),
                    arity: 0,
                    variadic: false,
                })
                .chain(compiled_expression)
                .chain(once(Instruction::Return)),
            ),
            CompilerMode::Expression => {
                let mut program = compiled_expression;
                program.push(Instruction::Evaluate);
                program.push(Instruction::Return);
                program
            }
        };
        let builtins = if builtins {
            create_builtin_wrapper_functions::<T>(&mut self)
                .into_iter()
                .collect::<Vec<_>>()
        } else {
            Vec::new()
        };
        let plugins = create_native_wrapper_functions(plugins, &mut self)
            .into_iter()
            .collect::<Vec<_>>();
        let compiled_chunks = std::mem::replace(&mut self.compiled_chunks, HashMap::new());
        let (compiled_program, builtins, plugins) =
            link_compiled_chunks(program, self.prelude, compiled_chunks, builtins, plugins)?;
        if self.options.debug {
            println!("{}", compiled_program);
        }
        compiled_plugins.extend(plugins);
        Ok(CompilerOutput::new(
            compiled_program,
            builtins,
            compiled_plugins,
        ))
    }
    pub fn intern(&mut self, target: &Program) -> Result<Program, String> {
        match target.instructions().first() {
            Some(Instruction::Function { .. }) => {
                extract_compiled_chunk(target.instructions(), target, &mut self.compiled_chunks)
                    .map(|chunk| {
                        let chunk_hash = hash_object(&chunk);
                        let chunk_index = InstructionPointer::new(self.compiled_chunks.len());
                        self.compiled_chunks
                            .insert(chunk_hash, (chunk_index, chunk));
                        Program::new(once(Instruction::PushFunction {
                            target: chunk_index,
                        }))
                    })
            }
            Some(instruction) => Err(format!(
                "Invalid function intern: Expected Function, received {:?}",
                instruction
            )),
            None => Err(format!("Invalid function inter: empty program")),
        }
    }
    pub fn extract<T: Expression>(target: &Program, program: &Program) -> Result<Program, String> {
        let mut compiled_chunks = HashMap::new();
        let chunk = extract_compiled_chunk(target.instructions(), program, &mut compiled_chunks)?;
        let (program, _builtins, _plugins) = link_compiled_chunks(
            chunk,
            None,
            compiled_chunks,
            empty(),
            empty::<(NativeFunction<T>, InstructionPointer)>(),
        )?;
        Ok(program)
    }
}

fn link_compiled_chunks<T: Expression>(
    entry_chunk: Program,
    prelude: Option<Program>,
    compiled_chunks: HashMap<HashId, (InstructionPointer, Program)>,
    builtins: impl IntoIterator<Item = (BuiltinTerm, InstructionPointer)>,
    plugins: impl IntoIterator<Item = (NativeFunction<T>, InstructionPointer)>,
) -> Result<
    (
        Program,
        Vec<(BuiltinTerm, InstructionPointer)>,
        Vec<(NativeFunction<T>, InstructionPointer)>,
    ),
    String,
> {
    let prelude_length = prelude.as_ref().map(|chunk| chunk.len()).unwrap_or(0);
    let compiled_instruction_offset = prelude_length + entry_chunk.len();
    let ordered_chunks = compiled_chunks
        .into_iter()
        .map(|(_hash, (address, chunk))| (address, chunk))
        .collect::<BTreeMap<_, _>>();
    let address_mappings = ordered_chunks
        .iter()
        .scan(compiled_instruction_offset, |offset, (index, chunk)| {
            let result = (*index, InstructionPointer::new(*offset));
            *offset += chunk.len();
            Some(result)
        })
        .collect::<HashMap<_, _>>();
    let compiled_instructions = ordered_chunks
        .into_iter()
        .map(|(_index, chunk)| chunk)
        .flat_map(|chunk| chunk);
    let builtins = builtins
        .into_iter()
        .map(|(target, address)| match address_mappings.get(&address) {
            Some(address) => Ok((target, *address)),
            None => Err(format!(
                "Invalid builtin function address for {}: {:x}",
                target, address
            )),
        })
        .collect::<Result<Vec<_>, _>>()?;
    let plugins = plugins
        .into_iter()
        .map(|(target, address)| match address_mappings.get(&address) {
            Some(address) => Ok((target, *address)),
            None => Err(format!(
                "Invalid native function address for {}: {:x}",
                target, address
            )),
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok((
        Program::new(
            prelude.into_iter().flat_map(identity).chain(
                entry_chunk
                    .into_iter()
                    .chain(compiled_instructions)
                    .map(|instruction| {
                        let result = match instruction {
                            // TODO: Differentiate between prelude functions and compiled chunks
                            Instruction::PushFunction { target } => match address_mappings
                                .get(&target)
                            {
                                Some(address) => Ok(Instruction::PushFunction { target: *address }),
                                None if target.get() < prelude_length => {
                                    Ok(Instruction::PushFunction { target })
                                }
                                _ => Err((instruction, target)),
                            },
                            Instruction::Jump { target } => match address_mappings.get(&target) {
                                Some(address) => Ok(Instruction::Jump { target: *address }),
                                None if target.get() < prelude_length => {
                                    Ok(Instruction::Jump { target })
                                }
                                _ => Err((instruction, target)),
                            },
                            Instruction::Call { target } => match address_mappings.get(&target) {
                                Some(address) => Ok(Instruction::Call { target: *address }),
                                None if target.get() < prelude_length => {
                                    Ok(Instruction::Call { target })
                                }
                                _ => Err((instruction, target)),
                            },
                            _ => Ok(instruction),
                        };
                        match result {
                            Ok(result) => Ok(result),
                            Err((instruction, target_address)) => Err(format!(
                                "Invalid address when compiling instruction {:?}: {:x}",
                                instruction, target_address,
                            )),
                        }
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            ),
        ),
        builtins,
        plugins,
    ))
}

fn extract_compiled_chunk<'a>(
    instructions: impl IntoIterator<Item = &'a Instruction>,
    program: &'a Program,
    compiled_chunks: &mut HashMap<HashId, (InstructionPointer, Program)>,
) -> Result<Program, String> {
    let mut instructions = instructions.into_iter();
    let mut chunk = Program::new(empty());
    while let Some(instruction) = instructions.next() {
        match instruction {
            Instruction::Call { target } => {
                let remapped_target = remap_compiled_address(*target, program, compiled_chunks)?;
                chunk.push(Instruction::Call {
                    target: remapped_target,
                })
            }
            Instruction::Jump { target } => {
                let remapped_target = remap_compiled_address(*target, program, compiled_chunks)?;
                chunk.push(Instruction::Jump {
                    target: remapped_target,
                })
            }
            Instruction::PushFunction { target } => {
                let remapped_target = remap_compiled_address(*target, program, compiled_chunks)?;
                chunk.push(Instruction::PushFunction {
                    target: remapped_target,
                })
            }
            // TODO: allow extracting functions containing early exit
            Instruction::Return => {
                chunk.push(instruction.clone());
                break;
            }
            _ => chunk.push(instruction.clone()),
        }
    }
    Ok(chunk)
}

fn remap_compiled_address(
    target: InstructionPointer,
    program: &Program,
    compiled_chunks: &mut HashMap<HashId, (InstructionPointer, Program)>,
) -> Result<InstructionPointer, String> {
    let target_hash = get_instruction_hash(target, program);
    if let Some((address, _)) = compiled_chunks.get(&target_hash) {
        Ok(*address)
    } else {
        match program.get(target) {
            Some(Instruction::Function {
                hash,
                arity,
                variadic,
            }) => {
                let chunk_index = InstructionPointer::new(compiled_chunks.len());
                let placeholder = Program::new(once(Instruction::Function {
                    hash: *hash,
                    arity: *arity,
                    variadic: *variadic,
                }));
                compiled_chunks.insert(target_hash, (chunk_index, placeholder));
                let instructions = program.instructions().iter().skip(target.get());
                let chunk = extract_compiled_chunk(instructions, program, compiled_chunks)?;
                compiled_chunks.insert(target_hash, (chunk_index, chunk));
                Ok(chunk_index)
            }
            _ => Err(format!(
                "Unable to extract target instruction: {:x}",
                target
            )),
        }
    }
}

fn get_instruction_hash(target: InstructionPointer, program: &Program) -> HashId {
    combine_hashes(program, &target)
}

pub(crate) fn compile_expressions<'a, T: Expression + Compile<T> + 'a>(
    values: impl IntoIterator<Item = &'a T>,
    eager: VarArgs,
    stack_offset: StackOffset,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    compiler: &mut Compiler,
) -> Result<(Program, NativeFunctionRegistry<T>), String> {
    values.into_iter().enumerate().fold(
        Ok((Program::new(empty()), NativeFunctionRegistry::default())),
        |result, (index, value)| {
            let (mut program, mut native_functions) = result?;
            match value.compile(eager, stack_offset + index, factory, allocator, compiler) {
                Err(error) => Err(error),
                Ok((compiled_value, value_native_functions)) => {
                    program.extend(compiled_value);
                    native_functions.extend(value_native_functions);
                    Ok((program, native_functions))
                }
            }
        },
    )
}

fn create_builtin_wrapper_functions<'a, T: Expression + Applicable<T>>(
    compiler: &'a mut Compiler,
) -> impl IntoIterator<Item = (BuiltinTerm, InstructionPointer)> + 'a {
    BuiltinTerm::iter().into_iter().map(move |target| {
        let compiled_address = compile_builtin_function::<T>(target, compiler);
        (target, compiled_address)
    })
}

fn create_native_wrapper_functions<'a, T: Expression + Applicable<T> + Compile<T> + 'a>(
    plugins: impl IntoIterator<
        Item = NativeFunction<T>,
        IntoIter = impl Iterator<Item = NativeFunction<T>> + 'a,
    >,
    compiler: &'a mut Compiler,
) -> impl IntoIterator<Item = (NativeFunction<T>, InstructionPointer)> + 'a {
    plugins.into_iter().map(move |target| {
        let compiled_address = compile_native_function(&target, compiler);
        (target, compiled_address)
    })
}
