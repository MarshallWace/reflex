// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{
    collections::{hash_map::DefaultHasher, BTreeMap, HashMap},
    convert::identity,
    hash::{Hash, Hasher},
    iter::{empty, once, FromIterator},
};

use crate::{
    cache::SubstitutionCache,
    core::{
        Applicable, Expression, ExpressionFactory, HeapAllocator, Reducible, Rewritable,
        SignalType, StackOffset, StateToken, StructPrototype, VarArgs,
    },
    hash::{combine_hashes, hash_object, HashId},
    lang::{BuiltinTerm, FloatValue, IntValue, NativeFunctionId, SymbolId},
};
use serde::{Deserialize, Serialize};

pub trait Compile<T: Expression + Compile<T>> {
    fn compile(
        &self,
        eager: VarArgs,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<Program, String>;
}

#[derive(Hash, PartialEq, Clone, Debug, Serialize, Deserialize)]
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
impl FromIterator<Instruction> for Program {
    fn from_iter<T: IntoIterator<Item = Instruction>>(iter: T) -> Self {
        Self::new(iter)
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

#[derive(PartialEq, Clone, Debug, Serialize, Deserialize)]
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
        num_args: usize,
    },
    Apply {
        num_args: usize,
    },
    Function {
        hash: HashId,
        required_args: usize,
        optional_args: usize,
    },
    Return,
    Evaluate,
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
            Self::Call { target, num_args } => {
                state.write_u8(17);
                target.hash(state);
                num_args.hash(state);
            }
            Self::Apply { num_args } => {
                state.write_u8(18);
                num_args.hash(state);
            }
            Self::Function {
                hash,
                required_args,
                optional_args,
            } => {
                state.write_u8(19);
                hash.hash(state);
                required_args.hash(state);
                optional_args.hash(state);
            }
            Self::Return => {
                state.write_u8(20);
            }
            Self::Evaluate => {
                state.write_u8(21);
            }
            Self::ConstructApplication { num_args } => {
                state.write_u8(22);
                num_args.hash(state);
            }
            Self::ConstructPartialApplication { num_args } => {
                state.write_u8(23);
                num_args.hash(state);
            }
            Self::ConstructTuple { size } => {
                state.write_u8(24);
                size.hash(state);
            }
            Self::ConstructVector { size } => {
                state.write_u8(26);
                size.hash(state);
            }
            Self::ConstructHashMap { size } => {
                state.write_u8(27);
                size.hash(state);
            }
            Self::ConstructHashSet { size } => {
                state.write_u8(28);
                size.hash(state);
            }
            Self::CombineSignals { count } => {
                state.write_u8(28);
                count.hash(state);
            }
        }
    }
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Default, Clone, Copy, Serialize, Deserialize)]
pub struct InstructionPointer(pub usize);
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
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Result<Program, String> {
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
        let compiled_expression = optimized_expression
            .as_ref()
            .unwrap_or(expression)
            .compile(VarArgs::Eager, 0, factory, allocator, &mut self)?;
        let program = match mode {
            CompilerMode::Thunk => Program::new(
                once(Instruction::Function {
                    hash: hash_object(&compiled_expression),
                    required_args: 0,
                    optional_args: 0,
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
        let compiled_chunks = std::mem::replace(&mut self.compiled_chunks, HashMap::new());
        let compiled_program = link_compiled_chunks(program, self.prelude, compiled_chunks)?;
        if self.options.debug {
            println!("{}", compiled_program);
        }
        Ok(compiled_program)
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
        link_compiled_chunks(chunk, None, compiled_chunks)
    }
}

fn link_compiled_chunks(
    entry_chunk: Program,
    prelude: Option<Program>,
    compiled_chunks: HashMap<HashId, (InstructionPointer, Program)>,
) -> Result<Program, String> {
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
    Ok(Program::new(
        prelude.into_iter().flat_map(identity).chain(
            entry_chunk
                .into_iter()
                .chain(compiled_instructions)
                .map(|instruction| {
                    let result = match instruction {
                        // FIXME: Differentiate between addresses of unlinked functions vs previously-linked prelude functions
                        Instruction::PushFunction { target } => match address_mappings.get(&target)
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
                        Instruction::Call { target, num_args } => {
                            match address_mappings.get(&target) {
                                Some(address) => Ok(Instruction::Call {
                                    target: *address,
                                    num_args,
                                }),
                                None if target.get() < prelude_length => {
                                    Ok(Instruction::Call { target, num_args })
                                }
                                _ => Err((instruction, target)),
                            }
                        }
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
            Instruction::Call { target, num_args } => {
                let remapped_target = remap_compiled_address(*target, program, compiled_chunks)?;
                chunk.push(Instruction::Call {
                    target: remapped_target,
                    num_args: *num_args,
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
            Some(&Instruction::Function {
                hash,
                required_args,
                optional_args,
            }) => {
                let chunk_index = InstructionPointer::new(compiled_chunks.len());
                let placeholder = Program::new(once(Instruction::Function {
                    hash,
                    required_args,
                    optional_args,
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

pub fn hash_program_root(program: &Program, entry_point: &InstructionPointer) -> HashId {
    let mut hasher = DefaultHasher::new();
    program.hash(&mut hasher);
    entry_point.hash(&mut hasher);
    hasher.finish()
}

pub(crate) fn compile_expressions<'a, T: Expression + Compile<T> + 'a>(
    values: impl IntoIterator<Item = &'a T>,
    eager: VarArgs,
    stack_offset: StackOffset,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    compiler: &mut Compiler,
) -> Result<Program, String> {
    values
        .into_iter()
        .enumerate()
        .fold(Ok(Program::new(empty())), |result, (index, value)| {
            let mut program = result?;
            match value.compile(eager, stack_offset + index, factory, allocator, compiler) {
                Err(error) => Err(error),
                Ok(compiled_expression) => {
                    program.extend(compiled_expression);
                    Ok(program)
                }
            }
        })
}
