// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::collections::HashSet;

use crate::{
    compiler::{Instruction, InstructionPointer, Program},
    core::{DependencyList, Expression, StackOffset, StateToken},
    hash::HashId,
};

struct StackEntry<T: Expression> {
    hash: Option<HashId>,
    context: StackFrame<T>,
    parent_state_dependencies: DependencyList,
    parent_subexpressions: Vec<HashId>,
}

#[derive(PartialEq, Eq, Debug)]
pub(crate) enum StackFrame<T: Expression> {
    Expression {
        address: InstructionPointer,
    },
    ApplicationTarget {
        args: Vec<T>,
    },
    Function {
        #[allow(dead_code)]
        target_address: InstructionPointer,
        caller_address: InstructionPointer,
        resume_address: InstructionPointer,
    },
    List {
        address: InstructionPointer,
        items: Vec<T>,
        results: Vec<T>,
    },
}

pub struct CallStack<'a, T: Expression> {
    call_stack_size: Option<usize>,
    program: &'a Program,
    program_counter: InstructionPointer,
    state_dependencies: DependencyList,
    subexpressions: Vec<HashId>,
    call_stack: Vec<StackEntry<T>>,
}
impl<'a, T: Expression> std::fmt::Debug for CallStack<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CallStack")
            .field(
                "call_stack",
                &self
                    .call_stack
                    .iter()
                    .rev()
                    .map(|entry| format!("{:?}", entry.context))
                    .collect::<Vec<_>>()
                    .join("\n"),
            )
            .finish()
    }
}
impl<'src, T: Expression> CallStack<'src, T> {
    pub fn new(
        program: &'src Program,
        entry_point: InstructionPointer,
        call_stack_size: Option<usize>,
    ) -> Self {
        Self {
            program,
            program_counter: entry_point,
            call_stack: create_stack(call_stack_size),
            state_dependencies: DependencyList::empty(),
            subexpressions: Vec::new(),
            call_stack_size,
        }
    }
    pub fn call_stack_size(&self) -> Option<usize> {
        self.call_stack_size
    }
    pub fn call_stack_depth(&self) -> usize {
        self.call_stack.len()
    }
    pub fn program(&self) -> &'src Program {
        self.program
    }
    pub fn program_counter(&self) -> InstructionPointer {
        self.program_counter
    }
    pub fn current_instruction(&'src self) -> Option<&'src Instruction> {
        self.lookup_instruction(self.program_counter)
    }
    pub fn lookup_instruction(
        &'src self,
        address: InstructionPointer,
    ) -> Option<&'src Instruction> {
        self.program.get(address)
    }
    pub fn next_program_counter(&self) -> InstructionPointer {
        self.program_counter.advance()
    }
    pub(crate) fn update_program_counter(&mut self, value: InstructionPointer) {
        self.program_counter = value
    }
    pub(crate) fn enter_expression(&mut self, hash: HashId) {
        self.enter_stack_frame(
            Some(hash),
            StackFrame::Expression {
                address: self.program_counter,
            },
        )
    }
    pub(crate) fn peek_expression_stack(
        &mut self,
        program_counter: InstructionPointer,
    ) -> Option<HashId> {
        self.call_stack
            .last()
            .and_then(|entry| match entry.context {
                StackFrame::Expression { address } if address == program_counter => entry.hash,
                _ => None,
            })
    }
    pub(crate) fn pop_expression_stack(
        &mut self,
        program_counter: InstructionPointer,
    ) -> Option<(HashId, DependencyList, Vec<HashId>)> {
        if self.peek_expression_stack(program_counter).is_some() {
            self.pop_call_stack()
                .map(|(hash, dependencies, subexpressions, _)| {
                    (hash.unwrap(), dependencies, subexpressions)
                })
        } else {
            None
        }
    }
    pub(crate) fn enter_application_target(&mut self, args: Vec<T>) {
        self.enter_stack_frame(None, StackFrame::ApplicationTarget { args });
    }
    pub(crate) fn peek_application_target_stack(&self) -> Option<&[T]> {
        self.call_stack
            .last()
            .and_then(|entry| match &entry.context {
                StackFrame::ApplicationTarget { args } => Some(args.as_slice()),
                _ => None,
            })
    }
    pub(crate) fn pop_application_target_stack(
        &mut self,
    ) -> Option<(Vec<T>, DependencyList, Vec<HashId>)> {
        if self.peek_application_target_stack().is_some() {
            self.pop_call_stack()
                .and_then(|(_, dependencies, subexpressions, frame)| match frame {
                    StackFrame::ApplicationTarget { args } => {
                        Some((args, dependencies, subexpressions))
                    }
                    _ => None,
                })
        } else {
            None
        }
    }
    pub(crate) fn enter_function(
        &mut self,
        hash: HashId,
        target_address: InstructionPointer,
        caller_address: InstructionPointer,
        resume_address: InstructionPointer,
    ) {
        self.program_counter = target_address;
        self.enter_stack_frame(
            Some(hash),
            StackFrame::Function {
                target_address,
                caller_address,
                resume_address,
            },
        );
    }
    pub(crate) fn peek_function_stack(&mut self) -> Option<InstructionPointer> {
        self.call_stack
            .last()
            .and_then(|entry| match entry.context {
                StackFrame::Function { target_address, .. } => Some(target_address),
                _ => None,
            })
    }
    pub(crate) fn pop_function_stack(
        &mut self,
    ) -> Option<(
        HashId,
        DependencyList,
        Vec<HashId>,
        InstructionPointer,
        InstructionPointer,
    )> {
        if self.peek_function_stack().is_some() {
            self.pop_call_stack()
                .map(|(hash, dependencies, subexpressions, frame)| {
                    let (caller_address, resume_address) = match frame {
                        StackFrame::Function {
                            caller_address,
                            resume_address,
                            ..
                        } => Some((caller_address, resume_address)),
                        _ => None,
                    }
                    .unwrap();
                    (
                        hash.unwrap(),
                        dependencies,
                        subexpressions,
                        caller_address,
                        resume_address,
                    )
                })
        } else {
            None
        }
    }
    pub(crate) fn enter_list(&mut self, items: Vec<T>, results: Option<Vec<T>>) {
        let num_items = items.len();
        self.enter_stack_frame(
            None,
            StackFrame::List {
                address: self.program_counter,
                items,
                results: results.unwrap_or_else(|| Vec::with_capacity(num_items)),
            },
        );
    }
    pub(crate) fn peek_list_stack(
        &self,
        program_counter: InstructionPointer,
    ) -> Option<(&[T], &[T])> {
        self.call_stack
            .last()
            .and_then(|entry| match &entry.context {
                StackFrame::List {
                    address,
                    items,
                    results,
                } if *address == program_counter => Some((items.as_slice(), results.as_slice())),
                _ => None,
            })
    }
    pub(crate) fn peek_next_list_item(&self, program_counter: InstructionPointer) -> Option<&T> {
        self.peek_list_stack(program_counter)
            .and_then(|(items, results)| items.get(results.len()))
    }
    pub(crate) fn pop_list_stack(
        &mut self,
        program_counter: InstructionPointer,
    ) -> Option<(Vec<T>, Vec<T>, DependencyList, Vec<HashId>)> {
        if self.peek_list_stack(program_counter).is_some() {
            self.pop_call_stack()
                .map(|(_, dependencies, subexpressions, frame)| {
                    let (items, results) = match frame {
                        StackFrame::List { items, results, .. } => Some((items, results)),
                        _ => None,
                    }
                    .unwrap();
                    (items, results, dependencies, subexpressions)
                })
        } else {
            None
        }
    }
    pub(crate) fn add_state_dependencies(
        &mut self,
        state_tokens: impl IntoIterator<Item = StateToken>,
    ) {
        self.state_dependencies.extend(state_tokens);
    }
    pub(crate) fn add_subexpression(&mut self, hash: HashId) {
        self.subexpressions.push(hash);
    }
    pub(crate) fn add_subexpressions(&mut self, hashes: impl IntoIterator<Item = HashId>) {
        self.subexpressions.extend(hashes);
    }
    pub(crate) fn drain(&mut self) -> (DependencyList, Vec<HashId>) {
        if self.call_stack.is_empty() {
            (
                self.replace_state_dependencies(DependencyList::empty()),
                self.replace_subexpressions(Vec::new()),
            )
        } else {
            let (mut dependencies, mut subexpressions) = self.truncate_to_depth(0);
            dependencies.extend(self.replace_state_dependencies(DependencyList::empty()));
            subexpressions.extend(self.replace_subexpressions(Vec::new()));
            (dependencies, subexpressions)
        }
    }
    fn enter_stack_frame(&mut self, hash: Option<HashId>, frame: StackFrame<T>) {
        if let Some(hash) = hash {
            self.add_subexpression(hash);
        }
        let parent_state_dependencies = self.replace_state_dependencies(DependencyList::empty());
        let parent_subexpressions = self.replace_subexpressions(Vec::new());
        self.call_stack.push(StackEntry {
            hash,
            context: frame,
            parent_state_dependencies,
            parent_subexpressions,
        });
    }
    fn pop_call_stack(
        &mut self,
    ) -> Option<(Option<HashId>, DependencyList, Vec<HashId>, StackFrame<T>)> {
        match self.call_stack.pop() {
            Some(entry) => {
                let child_state_dependencies =
                    self.replace_state_dependencies(entry.parent_state_dependencies);
                let child_subexpressions = self.replace_subexpressions(entry.parent_subexpressions);
                Some((
                    entry.hash,
                    child_state_dependencies,
                    child_subexpressions,
                    entry.context,
                ))
            }
            None => None,
        }
    }
    pub fn truncate_to_depth(&mut self, depth: usize) -> (DependencyList, Vec<HashId>) {
        let mut dependencies = DependencyList::empty();
        let mut subexpressions = HashSet::new();
        while self.call_stack.len() > depth {
            let (_, frame_dependencies, frame_subexpressions, _) = self.pop_call_stack().unwrap();
            dependencies.extend(frame_dependencies);
            subexpressions.extend(frame_subexpressions);
        }
        (dependencies, subexpressions.into_iter().collect())
    }
    fn replace_state_dependencies(&mut self, value: DependencyList) -> DependencyList {
        std::mem::replace(&mut self.state_dependencies, value)
    }
    fn replace_subexpressions(&mut self, value: Vec<HashId>) -> Vec<HashId> {
        std::mem::replace(&mut self.subexpressions, value)
    }
}

#[derive(Clone)]
pub struct VariableStack<T: Expression> {
    variable_stack: Vec<T>,
    variable_stack_size: Option<usize>,
}
impl<T: Expression> VariableStack<T> {
    pub(crate) fn new(variable_stack_size: Option<usize>) -> Self {
        Self {
            variable_stack: create_stack(variable_stack_size),
            variable_stack_size,
        }
    }
    pub fn variable_stack_size(&self) -> Option<usize> {
        self.variable_stack_size
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
    pub fn slice(&self, count: usize) -> impl IntoIterator<Item = &T> {
        self.variable_stack.iter().rev().take(count)
    }
    pub fn clone_shallow(&self, depth: StackOffset) -> Self {
        Self {
            variable_stack: self.variable_stack[self.variable_stack.len() - depth..]
                .iter()
                .cloned()
                .collect(),
            variable_stack_size: self.variable_stack_size,
        }
    }
}

fn create_stack<T>(capacity: Option<usize>) -> Vec<T> {
    match capacity {
        Some(capacity) => Vec::with_capacity(capacity),
        None => Vec::new(),
    }
}
