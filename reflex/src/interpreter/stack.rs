// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
};

use crate::{
    compiler::{Instruction, InstructionPointer, Program},
    core::{DependencyList, Expression, StackOffset, StateToken},
    hash::HashId,
};

struct Subroutine {
    offset: InstructionPointer,
    instructions: Program,
}
impl Subroutine {
    fn start_address(&self) -> InstructionPointer {
        self.offset
    }
    fn end_address(&self) -> InstructionPointer {
        InstructionPointer::new(self.offset.get() + self.instructions.len())
    }
    fn contains_address(&self, target_address: InstructionPointer) -> bool {
        target_address >= self.start_address() && target_address < self.end_address()
    }
}

struct StackEntry {
    hash: HashId,
    subroutine: bool,
    #[allow(dead_code)]
    target_address: InstructionPointer,
    resume_address: InstructionPointer,
    parent_dependencies: DependencyList,
}

pub struct CallStack<'a> {
    debug: bool,
    program: &'a Program,
    program_counter: InstructionPointer,
    state_dependencies: DependencyList,
    call_stack: Vec<StackEntry>,
    subroutines: Vec<Subroutine>,
}
impl<'a> CallStack<'a> {
    pub fn new(
        program: &'a Program,
        entry_point: InstructionPointer,
        call_stack_size: Option<usize>,
        debug: bool,
    ) -> Self {
        Self {
            debug,
            program,
            program_counter: entry_point,
            call_stack: create_stack(call_stack_size),
            state_dependencies: DependencyList::empty(),
            subroutines: Vec::new(),
        }
    }
    pub fn program(&self) -> &'a Program {
        self.program
    }
    pub fn get_program_counter(&self) -> InstructionPointer {
        self.program_counter
    }
    pub fn advance_program_counter(&self) -> InstructionPointer {
        self.program_counter.advance()
    }
    pub(crate) fn update_program_counter(&mut self, value: InstructionPointer) {
        self.program_counter = value
    }
    pub(crate) fn call_function(
        &mut self,
        hash: HashId,
        target_address: InstructionPointer,
        resume_address: InstructionPointer,
    ) {
        let parent_dependencies = self.replace_state_dependencies(DependencyList::empty());
        self.call_stack.push(StackEntry {
            hash,
            subroutine: false,
            target_address,
            resume_address,
            parent_dependencies,
        });
        self.program_counter = target_address;
    }
    pub(crate) fn call_subroutine(
        &mut self,
        hash: HashId,
        instructions: Program,
        resume_address: InstructionPointer,
    ) -> InstructionPointer {
        let subroutine_address = match self.subroutines.last() {
            Some(subroutine) => subroutine.end_address(),
            None => InstructionPointer::new(self.program.len()),
        };
        self.subroutines.push(Subroutine {
            instructions,
            offset: subroutine_address,
        });
        if self.debug {
            println!(
                "< {:x}:\n{}",
                subroutine_address,
                self.subroutines.last().unwrap().instructions
            );
        }
        let parent_dependencies = self.replace_state_dependencies(DependencyList::empty());
        self.call_stack.push(StackEntry {
            hash,
            subroutine: true,
            target_address: subroutine_address,
            resume_address,
            parent_dependencies,
        });
        self.program_counter = subroutine_address;
        subroutine_address
    }
    pub(crate) fn pop_call_stack(&mut self) -> Option<(HashId, DependencyList)> {
        match self.call_stack.pop() {
            Some(entry) => {
                if entry.subroutine {
                    self.subroutines.pop();
                }
                self.program_counter = entry.resume_address;
                let child_dependencies = self.replace_state_dependencies(entry.parent_dependencies);
                self.state_dependencies.extend(child_dependencies.iter());
                Some((entry.hash, child_dependencies))
            }
            None => None,
        }
    }
    pub fn call_stack_depth(&self) -> usize {
        self.call_stack.len()
    }
    pub(crate) fn add_state_dependencies(
        &mut self,
        state_tokens: impl IntoIterator<Item = StateToken>,
    ) {
        self.state_dependencies.extend(state_tokens);
    }
    pub fn get_state_dependencies(&self) -> &DependencyList {
        &self.state_dependencies
    }
    pub(crate) fn into_state_dependencies(self) -> DependencyList {
        self.state_dependencies
    }
    pub fn lookup_instruction(&'a self, address: InstructionPointer) -> Option<&'a Instruction> {
        if address.get() < self.program.len() {
            self.program.get(address)
        } else {
            let subroutine = self
                .subroutines
                .iter()
                .rev()
                .find(|subroutine| subroutine.contains_address(address));
            match subroutine {
                Some(subroutine) => subroutine.instructions.get(InstructionPointer::new(
                    address.get() - subroutine.offset.get(),
                )),
                None => None,
            }
        }
    }
    fn replace_state_dependencies(&mut self, dependencies: DependencyList) -> DependencyList {
        std::mem::replace(&mut self.state_dependencies, dependencies)
    }
}

pub struct VariableStack<T: Expression> {
    variable_stack: Vec<T>,
}
impl<T: Expression> VariableStack<T> {
    pub(crate) fn new(variable_stack_size: Option<usize>) -> Self {
        Self {
            variable_stack: create_stack(variable_stack_size),
        }
    }
    pub fn push(&mut self, value: T) {
        self.variable_stack.push(value);
    }
    pub fn push_multiple(&mut self, values: impl IntoIterator<Item = T>) {
        self.variable_stack.extend(values);
    }
    pub fn pop(&mut self) -> Option<T> {
        self.variable_stack.pop()
    }
    pub fn pop_multiple(
        &mut self,
        count: usize,
    ) -> impl IntoIterator<
        Item = T,
        IntoIter = (impl ExactSizeIterator<Item = T> + DoubleEndedIterator<Item = T> + '_),
    > + '_ {
        self.variable_stack
            .drain((self.variable_stack.len() - count)..)
    }
    pub fn peek(&self) -> Option<&T> {
        self.variable_stack.last()
    }
    pub fn get(&self, offset: StackOffset) -> Option<&T> {
        self.variable_stack
            .get(self.variable_stack.len() - 1 - offset)
    }
    pub fn update(&mut self, offset: StackOffset, value: T) {
        let target_offset = self.variable_stack.len() - 1 - offset;
        self.variable_stack[target_offset] = value;
    }
    pub fn splice(&mut self, offset: StackOffset, values: impl IntoIterator<Item = T>) {
        let index = self.variable_stack.len() - offset;
        self.variable_stack.splice(index..index, values);
    }
    pub fn values(&self) -> &[T] {
        &self.variable_stack
    }
    pub fn len(&self) -> usize {
        self.variable_stack.len()
    }
    pub fn generate_function_call_hash(&self, target: &impl Hash, num_args: StackOffset) -> HashId {
        let mut hasher = DefaultHasher::default();
        target.hash(&mut hasher);
        for arg in self.variable_stack.iter().rev().take(num_args) {
            arg.hash(&mut hasher);
        }
        hasher.finish()
    }
}

fn create_stack<T>(capacity: Option<usize>) -> Vec<T> {
    match capacity {
        Some(capacity) => Vec::with_capacity(capacity),
        None => Vec::new(),
    }
}
