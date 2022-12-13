// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::collections::HashSet;
use tracing::trace_span;

use reflex::{
    core::{Arity, DependencyList, Expression, InstructionPointer, StackOffset, StateToken},
    hash::HashId,
};

use crate::compiler::{Instruction, Program};

struct StackEntry<T: Expression> {
    hash: Option<HashId>,
    context: StackFrame<T>,
    parent_state_dependencies: DependencyList,
    parent_subexpressions: Vec<HashId>,
    span: Option<tracing::span::EnteredSpan>,
}

#[derive(Debug)]
pub(crate) enum StackFrame<T: Expression> {
    Expression {
        address: InstructionPointer,
    },
    ApplicationTarget {
        args: T::ExpressionList<T>,
    },
    ApplicationArg,
    ApplicationArgList {
        target: T,
        arity: Arity,
        caller_address: InstructionPointer,
        resume_address: InstructionPointer,
        args: Vec<T>,
        results: Vec<T>,
    },
    Function {
        #[allow(dead_code)]
        target_address: InstructionPointer,
        caller_address: InstructionPointer,
        resume_address: InstructionPointer,
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
    const EVALUATE_ARG_INSTRUCTION: Instruction = Instruction::Evaluate;
    const EVALUATE_ARG_ADDRESS: InstructionPointer = InstructionPointer(usize::MAX);
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
    pub fn evaluate_arg_program_counter(&self) -> InstructionPointer {
        Self::EVALUATE_ARG_ADDRESS
    }
    pub fn current_instruction(&'src self) -> Option<&'src Instruction> {
        self.lookup_instruction(self.program_counter)
    }
    pub fn lookup_instruction(
        &'src self,
        address: InstructionPointer,
    ) -> Option<&'src Instruction> {
        self.program.get(address).or_else(|| {
            if address == Self::EVALUATE_ARG_ADDRESS {
                Some(&Self::EVALUATE_ARG_INSTRUCTION)
            } else {
                None
            }
        })
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
    pub(crate) fn enter_application_target(&mut self, args: T::ExpressionList<T>) {
        self.enter_stack_frame(None, StackFrame::ApplicationTarget { args });
    }
    pub(crate) fn peek_application_target_stack(&self) -> Option<&T::ExpressionList<T>> {
        self.call_stack
            .last()
            .and_then(|entry| match &entry.context {
                StackFrame::ApplicationTarget { args } => Some(args),
                _ => None,
            })
    }
    pub(crate) fn pop_application_target_stack(
        &mut self,
    ) -> Option<(T::ExpressionList<T>, DependencyList, Vec<HashId>)> {
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
    pub(crate) fn pop_application_arg_stack(&mut self) -> Option<(DependencyList, Vec<HashId>)> {
        let is_application_arg = self
            .call_stack
            .last()
            .map(|entry| matches!(&entry.context, &StackFrame::ApplicationArg))
            .unwrap_or(false);
        if is_application_arg {
            self.pop_call_stack()
                .and_then(|(_, dependencies, subexpressions, frame)| match frame {
                    StackFrame::ApplicationArg => Some((dependencies, subexpressions)),
                    _ => None,
                })
        } else {
            None
        }
    }
    pub(crate) fn enter_application_arg(&mut self) {
        self.enter_stack_frame(None, StackFrame::ApplicationArg);
    }
    pub(crate) fn enter_application_arg_list(
        &mut self,
        target: T,
        arity: Arity,
        caller_address: InstructionPointer,
        resume_address: InstructionPointer,
        args: Vec<T>,
        results: Vec<T>,
    ) {
        self.enter_stack_frame(
            None,
            StackFrame::ApplicationArgList {
                target,
                arity,
                caller_address,
                resume_address,
                args,
                results,
            },
        );
    }
    pub(crate) fn peek_application_arg_list_stack(&self) -> Option<(&[T], &[T])> {
        self.call_stack
            .last()
            .and_then(|entry| match &entry.context {
                StackFrame::ApplicationArgList {
                    args: items,
                    results,
                    ..
                } => Some((items.as_slice(), results.as_slice())),
                _ => None,
            })
    }
    pub(crate) fn pop_application_arg_list_stack(
        &mut self,
    ) -> Option<(
        T,
        Arity,
        InstructionPointer,
        InstructionPointer,
        Vec<T>,
        Vec<T>,
        DependencyList,
        Vec<HashId>,
    )> {
        if self.peek_application_arg_list_stack().is_some() {
            self.pop_call_stack()
                .map(|(_, dependencies, subexpressions, frame)| {
                    let (target, arity, caller_address, resume_address, args, results) =
                        match frame {
                            StackFrame::ApplicationArgList {
                                target,
                                arity,
                                caller_address,
                                resume_address,
                                args,
                                results,
                            } => {
                                Some((target, arity, caller_address, resume_address, args, results))
                            }
                            _ => None,
                        }
                        .unwrap();
                    (
                        target,
                        arity,
                        caller_address,
                        resume_address,
                        args,
                        results,
                        dependencies,
                        subexpressions,
                    )
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
    pub(crate) fn add_state_dependencies(
        &mut self,
        state_tokens: impl IntoIterator<Item = StateToken>,
    ) {
        self.state_dependencies.extend(state_tokens);
    }
    pub(crate) fn add_subexpression(&mut self, subexpression: HashId) {
        self.subexpressions.push(subexpression);
    }
    pub(crate) fn add_subexpressions(&mut self, subexpressions: impl IntoIterator<Item = HashId>) {
        self.subexpressions.extend(subexpressions);
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
        let span_guard = hash.map(|hash| {
            let span = trace_span!("stack_frame_enter", hash);
            span.entered()
        });
        let parent_state_dependencies = self.replace_state_dependencies(DependencyList::empty());
        let parent_subexpressions = self.replace_subexpressions(Vec::new());
        self.call_stack.push(StackEntry {
            hash,
            context: frame,
            parent_state_dependencies,
            parent_subexpressions,
            span: span_guard,
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
                if let Some(guard) = entry.span {
                    guard.exit();
                }
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
    pub fn slice(&self, count: usize) -> &[T] {
        &self.variable_stack[(self.variable_stack.len() - count)..]
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
