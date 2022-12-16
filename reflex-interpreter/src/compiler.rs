// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::{
    collections::{
        hash_map::{DefaultHasher, Entry},
        BTreeMap,
    },
    hash::{Hash, Hasher},
    iter::{empty, once, FromIterator},
};

use reflex::{
    cache::SubstitutionCache,
    core::{
        Applicable, Eagerness, Expression, ExpressionFactory, HeapAllocator, InstructionPointer,
        IntValue, Internable, Reducible, Rewritable, SignalType, StackOffset, SymbolId, Uuid,
    },
    hash::{hash_object, FnvHashMap, FnvHasher, HashId, IntMap},
};
use serde::{Deserialize, Serialize};

pub trait Compile<T: Expression>: Internable {
    fn compile(
        &self,
        eager: Eagerness,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<Program, String>;
}

#[derive(Hash, PartialEq, Clone, Debug, Serialize, Deserialize, Default)]
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

#[derive(PartialEq, Clone, Debug, Serialize, Deserialize, Copy)]
pub struct FloatValue(f64);

impl std::hash::Hash for FloatValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write(&self.0.to_be_bytes())
    }
}

impl From<f64> for FloatValue {
    fn from(v: f64) -> Self {
        FloatValue(v)
    }
}

impl From<FloatValue> for f64 {
    fn from(v: FloatValue) -> Self {
        v.0
    }
}

#[derive(PartialEq, Clone, Debug, Serialize, Deserialize, Hash)]
pub enum Instruction {
    LoadStaticData {
        offset: StackOffset,
    },
    PushLocal {
        offset: StackOffset,
    },
    PushNil,
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
        id: SymbolId,
    },
    PushFunction {
        target: InstructionPointer,
        hash: HashId,
    },
    PushBuiltin {
        target: Uuid,
    },
    /// Load the effect from the context, or create a signal if unresolved in that context.
    LoadEffect,
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
        target_address: InstructionPointer,
        target_hash: HashId,
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
    ConstructConstructor {
        num_fields: usize,
    },
    ConstructList {
        size: usize,
    },
    ConstructHashMap {
        size: usize,
    },
    ConstructHashSet {
        size: usize,
    },
    ConstructCondition {
        signal_type: SignalType,
    },
    CombineSignals {
        count: usize,
    },
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub struct CompilerOptions {
    pub debug: bool,
    pub hoist_free_variables: bool,
    pub normalize: bool,
    pub inline_static_data: bool,
}
impl CompilerOptions {
    pub fn unoptimized() -> Self {
        Self {
            debug: false,
            hoist_free_variables: true,
            normalize: false,
            inline_static_data: false,
        }
    }
    pub fn debug() -> Self {
        Self {
            debug: true,
            ..Default::default()
        }
    }
}
impl Default for CompilerOptions {
    fn default() -> Self {
        Self {
            debug: false,
            hoist_free_variables: true,
            normalize: false,
            inline_static_data: true,
        }
    }
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub enum CompilerMode {
    /// Wraps the compiled expression in a 'main function' that evaluates the expression before returning
    Function,
    /// Compiles the expression without any wrapping
    Expression,
}

#[derive(Hash, PartialEq, Clone, Debug, Serialize, Deserialize, Default)]
pub struct CompiledProgram {
    pub instructions: Program,
    pub data_section: Vec<(HashId, Program)>,
}

impl std::fmt::Display for CompiledProgram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "--- COMPILED PROGRAM ---\n\n{}\n\n--- DATA SECTION ---\n\n{}\n\n--- END OF COMPILED PROGRAM ---",
            self.instructions,
            self.data_section
                .iter()
                .map(|(_, instructions)| { instructions.instructions.iter() })
                .flatten()
                .enumerate()
                .map(|(offset, instruction)| { format!("0x{:08x} {:?}", offset, instruction) })
                .collect::<Vec<_>>()
                .join("\n"),
        )
    }
}

pub struct Compiler {
    options: CompilerOptions,
    next_chunk_address: InstructionPointer,
    prelude: Program,
    data_section: Vec<Program>,
    data_section_offsets: IntMap<HashId, usize>,
    compiled_chunks: IntMap<HashId, (InstructionPointer, Program)>,
}
impl Compiler {
    pub fn new(options: CompilerOptions, prelude: Option<CompiledProgram>) -> Self {
        let CompiledProgram {
            instructions: prelude_instructions,
            data_section: prelude_data_section,
        } = prelude.unwrap_or_default();
        let (data_section, data_section_offsets): (Vec<_>, IntMap<_, _>) = prelude_data_section
            .into_iter()
            .enumerate()
            .map(|(index, (key, instructions))| (instructions, (key, index)))
            .unzip();
        Self {
            options,
            next_chunk_address: UNLINKED_FUNCTION_OFFSET.advance(),
            prelude: prelude_instructions,
            data_section: data_section,
            data_section_offsets: data_section_offsets,
            compiled_chunks: IntMap::default(),
        }
    }
    pub fn options(&self) -> &CompilerOptions {
        &self.options
    }
    pub fn get_data_section_item(&self, offset: usize) -> Option<&Program> {
        self.data_section.get(offset)
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
    ) -> Result<CompiledProgram, String> {
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

        let compiled_expression = self.compile_term(
            optimized_expression.as_ref().unwrap_or(expression),
            Eagerness::Eager,
            0,
            factory,
            allocator,
        )?;
        let program = match mode {
            CompilerMode::Expression => compiled_expression,
            CompilerMode::Function => Program::new(
                once(Instruction::Function {
                    hash: hash_object(&compiled_expression),
                    required_args: 0,
                    optional_args: 0,
                })
                .chain(compiled_expression)
                .chain(once(Instruction::Evaluate))
                .chain(once(Instruction::Return)),
            ),
        };
        let compiled_chunks = std::mem::replace(&mut self.compiled_chunks, IntMap::default());

        // At this point all the instructions have been compiled, however any compiled function offsets are zero-indexed
        // rather than being the actual offsets in the output code.
        // We need to run this through a linking process to rewrite the zero-indexed function offsets and replace them
        // with the actual instruction offsets.
        let unlinked_program = CompiledProgram {
            instructions: program,
            data_section: self.data_section_offsets.into_iter().fold(
                vec![(0, Program::default()); self.data_section.len()],
                |mut data_section, (key, offset)| {
                    let value =
                        std::mem::replace(&mut self.data_section[offset], Default::default());
                    data_section[offset] = (key, value);
                    data_section
                },
            ),
        };

        let compiled_program =
            link_compiled_chunks(unlinked_program, self.prelude, compiled_chunks)?;

        if self.options.debug {
            eprintln!("{}", compiled_program);
        }

        Ok(compiled_program)
    }

    pub fn compile_term<TTerm: Expression + Compile<T>, T: Expression + Compile<T>>(
        &mut self,
        term: &TTerm,
        eager: Eagerness,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Result<Program, String> {
        if self.options.inline_static_data && term.should_intern(eager) {
            term.compile(Eagerness::Lazy, stack_offset, factory, allocator, self)
                .map(|compiled_term| self.intern_term(term, compiled_term))
        } else {
            term.compile(eager, stack_offset, factory, allocator, self)
        }
    }

    fn intern_term<T: Expression>(&mut self, term: &T, compiled_term: Program) -> Program {
        let next_index = self.data_section.len();
        let heap_offset = match self.data_section_offsets.entry(term.id()) {
            Entry::Occupied(entry) => {
                let heap_offset = *entry.get();
                heap_offset
            }
            Entry::Vacant(entry) => {
                let mut compiled_term = compiled_term;
                compiled_term.push(Instruction::Return);
                self.data_section.push(compiled_term);

                let heap_offset = *entry.insert(next_index);
                heap_offset
            }
        };

        Program::new(once(Instruction::LoadStaticData {
            offset: heap_offset,
        }))
    }
}

fn link_compiled_chunks(
    program: CompiledProgram,
    prelude: Program,
    compiled_chunks: IntMap<HashId, (InstructionPointer, Program)>,
) -> Result<CompiledProgram, String> {
    let CompiledProgram {
        instructions: entry_chunk,
        data_section,
    } = program;
    let prelude_length = prelude.len();
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
        .collect::<FnvHashMap<_, _>>();
    let compiled_instructions = ordered_chunks
        .into_iter()
        .map(|(_index, chunk)| chunk)
        .flat_map(|chunk| chunk);
    let compiled_instructions = Program::new(entry_chunk.into_iter().chain(compiled_instructions));
    let linked_program = Program::new(
        prelude.into_iter().chain(
            remap_function_addresses(compiled_instructions, prelude_length, &address_mappings)?
                .into_iter(),
        ),
    );
    let linked_data_section = data_section
        .into_iter()
        .map(|(hash, instructions)| {
            remap_function_addresses(instructions, prelude_length, &address_mappings)
                .map(|linked_instructions| (hash, linked_instructions))
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(CompiledProgram {
        instructions: linked_program,
        data_section: linked_data_section,
    })
}

const UNLINKED_FUNCTION_OFFSET: InstructionPointer = InstructionPointer(0xFFFFFFFF);

fn remap_function_addresses(
    compiled_instructions: Program,
    prelude_length: usize,
    address_mappings: &FnvHashMap<InstructionPointer, InstructionPointer>,
) -> Result<Program, String> {
    compiled_instructions
        .into_iter()
        .map(|instruction| {
            let result = match instruction {
                // FIXME: Discriminate between addresses of unlinked functions in the current program vs previously-linked prelude functions
                Instruction::PushFunction { target, hash } => {
                    if target > UNLINKED_FUNCTION_OFFSET {
                        match address_mappings.get(&target) {
                            Some(address) => Ok(Instruction::PushFunction {
                                target: *address,
                                hash,
                            }),
                            None => Err((instruction, target)),
                        }
                    } else {
                        Ok(Instruction::PushFunction { target, hash })
                    }
                }
                Instruction::Jump { target } => match address_mappings.get(&target) {
                    Some(address) => Ok(Instruction::Jump { target: *address }),
                    None if target.get() < prelude_length => Ok(Instruction::Jump { target }),
                    _ => Err((instruction, target)),
                },
                Instruction::Call {
                    target_address,
                    target_hash,
                    num_args,
                } => {
                    // TODO: use target hash to discriminate between unlinked chunks and prelude functions
                    match address_mappings.get(&target_address) {
                        Some(address) => Ok(Instruction::Call {
                            target_address: *address,
                            target_hash,
                            num_args,
                        }),
                        None if target_address.get() < prelude_length => Ok(Instruction::Call {
                            target_address,
                            target_hash,
                            num_args,
                        }),
                        _ => Err((instruction, target_address)),
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
        .collect::<Result<Vec<_>, _>>()
        .map(Program::new)
}

pub fn hash_compiled_program(
    program: &CompiledProgram,
    entry_point: &InstructionPointer,
) -> HashId {
    let mut hasher = DefaultHasher::new();
    program.hash(&mut hasher);
    entry_point.hash(&mut hasher);
    hasher.finish()
}

pub fn hash_program_root(program: &Program, entry_point: &InstructionPointer) -> HashId {
    let mut hasher = FnvHasher::default();
    program.hash(&mut hasher);
    entry_point.hash(&mut hasher);
    hasher.finish()
}

pub fn create_main_function(instructions: impl IntoIterator<Item = Instruction>) -> Program {
    let mut instructions = once(Instruction::Function {
        hash: 0,
        required_args: 0,
        optional_args: 0,
    })
    .chain(instructions)
    .collect::<Vec<_>>();
    instructions[0] = Instruction::Function {
        hash: hash_object(&instructions),
        required_args: 0,
        optional_args: 0,
    };
    Program::new(instructions)
}

pub(crate) fn compile_expressions<'a, T: Expression + Compile<T> + 'a>(
    values: impl IntoIterator<Item = &'a T>,
    eager: Eagerness,
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
            match compiler.compile_term(value, eager, stack_offset + index, factory, allocator) {
                Err(error) => Err(error),
                Ok(compiled_expression) => {
                    program.extend(compiled_expression);
                    Ok(program)
                }
            }
        })
}
