// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{collections::HashSet, iter::once};

use serde::{Deserialize, Serialize};
use strum_macros::EnumIter;

pub use abs::*;
pub use add::*;
pub use and::*;
pub use append::*;
pub use apply::*;
pub use car::*;
pub use cdr::*;
pub use ceil::*;
pub use collect::*;
pub use concat::*;
pub use cons::*;
pub use construct::*;
pub use contains::*;
pub use divide::*;
pub use effect::*;
pub use ends_with::*;
pub use entries::*;
pub use eq::*;
pub use equal::*;
pub use filter::*;
pub use floor::*;
pub use get::*;
pub use gt::*;
pub use gte::*;
pub use if_error::*;
pub use if_pending::*;
pub use insert::*;
pub use keys::*;
pub use lt::*;
pub use lte::*;
pub use map::*;
pub use max::*;
pub use merge::*;
pub use min::*;
pub use multiply::*;
pub use not::*;
pub use or::*;
pub use pow::*;
pub use push::*;
pub use push_front::*;
pub use r#if::*;
pub use r#match::*;
pub use reduce::*;
pub use remainder::*;
pub use replace::*;
pub use resolve::*;
pub use resolve_deep::*;
pub use round::*;
pub use sequence::*;
pub use slice::*;
pub use split::*;
pub use starts_with::*;
pub use subtract::*;
pub use values::*;

use crate::{
    compiler::{
        Compile, Compiler, Instruction, InstructionPointer, NativeFunctionRegistry, Program,
    },
    core::{
        Applicable, Arity, DependencyList, EvaluationCache, Expression, ExpressionFactory,
        GraphNode, HeapAllocator, SerializeJson, StackOffset, VarArgs,
    },
    hash::{hash_object, HashId},
    lang::with_eagerness,
};

mod abs;
mod add;
mod and;
mod append;
mod apply;
mod car;
mod cdr;
mod ceil;
mod collect;
mod concat;
mod cons;
mod construct;
mod contains;
mod divide;
mod effect;
mod ends_with;
mod entries;
mod eq;
mod equal;
mod filter;
mod floor;
mod get;
mod gt;
mod gte;
mod r#if;
mod if_error;
mod if_pending;
mod insert;
mod keys;
mod lt;
mod lte;
mod map;
mod r#match;
mod max;
mod merge;
mod min;
mod multiply;
mod not;
mod or;
mod pow;
mod push;
mod push_front;
mod reduce;
mod remainder;
mod replace;
mod resolve;
mod resolve_deep;
pub use resolve_deep::*;
mod resolve_shallow;
pub use resolve_shallow::*;
mod round;
mod sequence;
mod slice;
mod split;
mod starts_with;
mod subtract;
mod values;

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug, EnumIter, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum BuiltinTerm {
    Add,
    Abs,
    And,
    Append,
    Apply,
    Car,
    Cdr,
    Ceil,
    Collect,
    CollectFilterResults,
    CollectHashMap,
    CollectHashSet,
    CollectStruct,
    CollectTuple,
    CollectVector,
    ConstructHashMap,
    ConstructHashSet,
    ConstructStruct,
    ConstructTuple,
    ConstructVector,
    Concat,
    Cons,
    Contains,
    Divide,
    Effect,
    EndsWith,
    Entries,
    Eq,
    Equal,
    Filter,
    Floor,
    Get,
    Gt,
    Gte,
    If,
    IfError,
    IfPending,
    Insert,
    Keys,
    Lt,
    Lte,
    Map,
    Match,
    Max,
    Merge,
    Min,
    Multiply,
    Not,
    Or,
    Pow,
    Push,
    PushFront,
    Reduce,
    Remainder,
    Replace,
    ResolveDeep,
    ResolveHashMap,
    ResolveHashSet,
    ResolveShallow,
    ResolveStruct,
    ResolveTuple,
    ResolveVector,
    Round,
    Sequence,
    Slice,
    Split,
    StartsWith,
    Subtract,
    Values,
}
impl GraphNode for BuiltinTerm {
    fn capture_depth(&self) -> StackOffset {
        0
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        HashSet::new()
    }
    fn dynamic_dependencies(&self) -> crate::core::DependencyList {
        DependencyList::empty()
    }
    fn is_static(&self) -> bool {
        true
    }
    fn is_atomic(&self) -> bool {
        true
    }
}
impl<T: Expression + Applicable<T>> Applicable<T> for BuiltinTerm {
    fn arity(&self) -> Option<Arity> {
        match self {
            Self::Add => Applicable::<T>::arity(&Add {}),
            Self::Abs => Applicable::<T>::arity(&Abs {}),
            Self::And => Applicable::<T>::arity(&And {}),
            Self::Append => Applicable::<T>::arity(&Append {}),
            Self::Apply => Applicable::<T>::arity(&Apply {}),
            Self::Car => Applicable::<T>::arity(&Car {}),
            Self::Cdr => Applicable::<T>::arity(&Cdr {}),
            Self::Ceil => Applicable::<T>::arity(&Ceil {}),
            Self::Collect => Applicable::<T>::arity(&Collect {}),
            Self::CollectFilterResults => Applicable::<T>::arity(&CollectFilterResults {}),
            Self::CollectHashMap => Applicable::<T>::arity(&CollectHashMap {}),
            Self::CollectHashSet => Applicable::<T>::arity(&CollectHashSet {}),
            Self::CollectStruct => Applicable::<T>::arity(&CollectStruct {}),
            Self::CollectTuple => Applicable::<T>::arity(&CollectTuple {}),
            Self::CollectVector => Applicable::<T>::arity(&CollectVector {}),
            Self::ConstructHashMap => Applicable::<T>::arity(&ConstructHashMap {}),
            Self::ConstructHashSet => Applicable::<T>::arity(&ConstructHashSet {}),
            Self::ConstructStruct => Applicable::<T>::arity(&ConstructStruct {}),
            Self::ConstructTuple => Applicable::<T>::arity(&ConstructTuple {}),
            Self::ConstructVector => Applicable::<T>::arity(&ConstructVector {}),
            Self::Concat => Applicable::<T>::arity(&Concat {}),
            Self::Cons => Applicable::<T>::arity(&Cons {}),
            Self::Contains => Applicable::<T>::arity(&Contains {}),
            Self::Divide => Applicable::<T>::arity(&Divide {}),
            Self::Effect => Applicable::<T>::arity(&Effect {}),
            Self::EndsWith => Applicable::<T>::arity(&EndsWith {}),
            Self::Entries => Applicable::<T>::arity(&Entries {}),
            Self::Eq => Applicable::<T>::arity(&Eq {}),
            Self::Equal => Applicable::<T>::arity(&Equal {}),
            Self::Filter => Applicable::<T>::arity(&Filter {}),
            Self::Floor => Applicable::<T>::arity(&Floor {}),
            Self::Get => Applicable::<T>::arity(&Get {}),
            Self::Gt => Applicable::<T>::arity(&Gt {}),
            Self::Gte => Applicable::<T>::arity(&Gte {}),
            Self::If => Applicable::<T>::arity(&If {}),
            Self::IfError => Applicable::<T>::arity(&IfError {}),
            Self::IfPending => Applicable::<T>::arity(&IfPending {}),
            Self::Insert => Applicable::<T>::arity(&Insert {}),
            Self::Keys => Applicable::<T>::arity(&Keys {}),
            Self::Lt => Applicable::<T>::arity(&Lt {}),
            Self::Lte => Applicable::<T>::arity(&Lte {}),
            Self::Map => Applicable::<T>::arity(&Map {}),
            Self::Match => Applicable::<T>::arity(&Match {}),
            Self::Max => Applicable::<T>::arity(&Max {}),
            Self::Merge => Applicable::<T>::arity(&Merge {}),
            Self::Min => Applicable::<T>::arity(&Min {}),
            Self::Multiply => Applicable::<T>::arity(&Multiply {}),
            Self::Not => Applicable::<T>::arity(&Not {}),
            Self::Or => Applicable::<T>::arity(&Or {}),
            Self::Pow => Applicable::<T>::arity(&Pow {}),
            Self::Push => Applicable::<T>::arity(&Push {}),
            Self::PushFront => Applicable::<T>::arity(&PushFront {}),
            Self::Reduce => Applicable::<T>::arity(&Reduce {}),
            Self::Remainder => Applicable::<T>::arity(&Remainder {}),
            Self::Replace => Applicable::<T>::arity(&Replace {}),
            Self::ResolveDeep => Applicable::<T>::arity(&ResolveDeep {}),
            Self::ResolveHashMap => Applicable::<T>::arity(&ResolveHashMap {}),
            Self::ResolveHashSet => Applicable::<T>::arity(&ResolveHashSet {}),
            Self::ResolveShallow => Applicable::<T>::arity(&ResolveShallow {}),
            Self::ResolveStruct => Applicable::<T>::arity(&ResolveStruct {}),
            Self::ResolveTuple => Applicable::<T>::arity(&ResolveTuple {}),
            Self::ResolveVector => Applicable::<T>::arity(&ResolveVector {}),
            Self::Round => Applicable::<T>::arity(&Round {}),
            Self::Sequence => Applicable::<T>::arity(&Sequence {}),
            Self::Slice => Applicable::<T>::arity(&Slice {}),
            Self::Split => Applicable::<T>::arity(&Split {}),
            Self::StartsWith => Applicable::<T>::arity(&StartsWith {}),
            Self::Subtract => Applicable::<T>::arity(&Subtract {}),
            Self::Values => Applicable::<T>::arity(&Values {}),
        }
    }
    fn apply(
        &self,
        args: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        match self {
            Self::Add => Applicable::<T>::apply(&Add {}, args, factory, allocator, cache),
            Self::Abs => Applicable::<T>::apply(&Abs {}, args, factory, allocator, cache),
            Self::And => Applicable::<T>::apply(&And {}, args, factory, allocator, cache),
            Self::Append => Applicable::<T>::apply(&Append {}, args, factory, allocator, cache),
            Self::Apply => Applicable::<T>::apply(&Apply {}, args, factory, allocator, cache),
            Self::Car => Applicable::<T>::apply(&Car {}, args, factory, allocator, cache),
            Self::Cdr => Applicable::<T>::apply(&Cdr {}, args, factory, allocator, cache),
            Self::Ceil => Applicable::<T>::apply(&Ceil {}, args, factory, allocator, cache),
            Self::Collect => Applicable::<T>::apply(&Collect {}, args, factory, allocator, cache),
            Self::CollectFilterResults => {
                Applicable::<T>::apply(&CollectFilterResults {}, args, factory, allocator, cache)
            }
            Self::CollectHashMap => {
                Applicable::<T>::apply(&CollectHashMap {}, args, factory, allocator, cache)
            }
            Self::CollectHashSet => {
                Applicable::<T>::apply(&CollectHashSet {}, args, factory, allocator, cache)
            }
            Self::CollectStruct => {
                Applicable::<T>::apply(&CollectStruct {}, args, factory, allocator, cache)
            }
            Self::CollectTuple => {
                Applicable::<T>::apply(&CollectTuple {}, args, factory, allocator, cache)
            }
            Self::CollectVector => {
                Applicable::<T>::apply(&CollectVector {}, args, factory, allocator, cache)
            }
            Self::ConstructHashMap => {
                Applicable::<T>::apply(&ConstructHashMap {}, args, factory, allocator, cache)
            }
            Self::ConstructHashSet => {
                Applicable::<T>::apply(&ConstructHashSet {}, args, factory, allocator, cache)
            }
            Self::ConstructStruct => {
                Applicable::<T>::apply(&ConstructStruct {}, args, factory, allocator, cache)
            }
            Self::ConstructTuple => {
                Applicable::<T>::apply(&ConstructTuple {}, args, factory, allocator, cache)
            }
            Self::ConstructVector => {
                Applicable::<T>::apply(&ConstructVector {}, args, factory, allocator, cache)
            }
            Self::Concat => Applicable::<T>::apply(&Concat {}, args, factory, allocator, cache),
            Self::Cons => Applicable::<T>::apply(&Cons {}, args, factory, allocator, cache),
            Self::Contains => Applicable::<T>::apply(&Contains {}, args, factory, allocator, cache),
            Self::Divide => Applicable::<T>::apply(&Divide {}, args, factory, allocator, cache),
            Self::Effect => Applicable::<T>::apply(&Effect {}, args, factory, allocator, cache),
            Self::EndsWith => Applicable::<T>::apply(&EndsWith {}, args, factory, allocator, cache),
            Self::Entries => Applicable::<T>::apply(&Entries {}, args, factory, allocator, cache),
            Self::Eq => Applicable::<T>::apply(&Eq {}, args, factory, allocator, cache),
            Self::Equal => Applicable::<T>::apply(&Equal {}, args, factory, allocator, cache),
            Self::Filter => Applicable::<T>::apply(&Filter {}, args, factory, allocator, cache),
            Self::Floor => Applicable::<T>::apply(&Floor {}, args, factory, allocator, cache),
            Self::Get => Applicable::<T>::apply(&Get {}, args, factory, allocator, cache),
            Self::Gt => Applicable::<T>::apply(&Gt {}, args, factory, allocator, cache),
            Self::Gte => Applicable::<T>::apply(&Gte {}, args, factory, allocator, cache),
            Self::If => Applicable::<T>::apply(&If {}, args, factory, allocator, cache),
            Self::IfError => Applicable::<T>::apply(&IfError {}, args, factory, allocator, cache),
            Self::IfPending => {
                Applicable::<T>::apply(&IfPending {}, args, factory, allocator, cache)
            }
            Self::Insert => Applicable::<T>::apply(&Insert {}, args, factory, allocator, cache),
            Self::Keys => Applicable::<T>::apply(&Keys {}, args, factory, allocator, cache),
            Self::Lt => Applicable::<T>::apply(&Lt {}, args, factory, allocator, cache),
            Self::Lte => Applicable::<T>::apply(&Lte {}, args, factory, allocator, cache),
            Self::Map => Applicable::<T>::apply(&Map {}, args, factory, allocator, cache),
            Self::Match => Applicable::<T>::apply(&Match {}, args, factory, allocator, cache),
            Self::Max => Applicable::<T>::apply(&Max {}, args, factory, allocator, cache),
            Self::Merge => Applicable::<T>::apply(&Merge {}, args, factory, allocator, cache),
            Self::Min => Applicable::<T>::apply(&Min {}, args, factory, allocator, cache),
            Self::Multiply => Applicable::<T>::apply(&Multiply {}, args, factory, allocator, cache),
            Self::Not => Applicable::<T>::apply(&Not {}, args, factory, allocator, cache),
            Self::Or => Applicable::<T>::apply(&Or {}, args, factory, allocator, cache),
            Self::Pow => Applicable::<T>::apply(&Pow {}, args, factory, allocator, cache),
            Self::Push => Applicable::<T>::apply(&Push {}, args, factory, allocator, cache),
            Self::PushFront => {
                Applicable::<T>::apply(&PushFront {}, args, factory, allocator, cache)
            }
            Self::Reduce => Applicable::<T>::apply(&Reduce {}, args, factory, allocator, cache),
            Self::Remainder => {
                Applicable::<T>::apply(&Remainder {}, args, factory, allocator, cache)
            }
            Self::Replace => Applicable::<T>::apply(&Replace {}, args, factory, allocator, cache),
            Self::ResolveDeep => {
                Applicable::<T>::apply(&ResolveDeep {}, args, factory, allocator, cache)
            }
            Self::ResolveHashMap => {
                Applicable::<T>::apply(&ResolveHashMap {}, args, factory, allocator, cache)
            }
            Self::ResolveHashSet => {
                Applicable::<T>::apply(&ResolveHashSet {}, args, factory, allocator, cache)
            }
            Self::ResolveShallow => {
                Applicable::<T>::apply(&ResolveShallow {}, args, factory, allocator, cache)
            }
            Self::ResolveStruct => {
                Applicable::<T>::apply(&ResolveStruct {}, args, factory, allocator, cache)
            }
            Self::ResolveTuple => {
                Applicable::<T>::apply(&ResolveTuple {}, args, factory, allocator, cache)
            }
            Self::ResolveVector => {
                Applicable::<T>::apply(&ResolveVector {}, args, factory, allocator, cache)
            }
            Self::Round => Applicable::<T>::apply(&Round {}, args, factory, allocator, cache),
            Self::Sequence => Applicable::<T>::apply(&Sequence {}, args, factory, allocator, cache),
            Self::Slice => Applicable::<T>::apply(&Slice {}, args, factory, allocator, cache),
            Self::Split => Applicable::<T>::apply(&Split {}, args, factory, allocator, cache),
            Self::StartsWith => {
                Applicable::<T>::apply(&StartsWith {}, args, factory, allocator, cache)
            }
            Self::Subtract => Applicable::<T>::apply(&Subtract {}, args, factory, allocator, cache),
            Self::Values => Applicable::<T>::apply(&Values {}, args, factory, allocator, cache),
        }
        .map_err(|err| format!("{}: {}", self, err))
    }
}
impl<T: Expression + Applicable<T> + Compile<T>> Compile<T> for BuiltinTerm {
    fn compile(
        &self,
        _eager: VarArgs,
        _stack_offset: StackOffset,
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<(Program, NativeFunctionRegistry<T>), String> {
        Ok((
            Program::new(once(Instruction::PushFunction {
                target: compile_builtin_function::<T>(*self, compiler),
            })),
            NativeFunctionRegistry::default(),
        ))
    }
}
impl std::fmt::Display for BuiltinTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<builtin:{:?}>", self)
    }
}

impl SerializeJson for BuiltinTerm {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        Err(format!("Unable to serialize term: {}", self))
    }
}

pub(crate) fn compile_builtin_function<T: Expression + Applicable<T>>(
    target: BuiltinTerm,
    compiler: &mut Compiler,
) -> InstructionPointer {
    compile_function(
        hash_object(&target),
        Applicable::<T>::arity(&target).unwrap(),
        Instruction::PushBuiltin { target },
        compiler,
    )
}

pub(crate) fn compile_function(
    hash: HashId,
    arity: Arity,
    target: Instruction,
    compiler: &mut Compiler,
) -> InstructionPointer {
    match compiler.retrieve_compiled_chunk_address(hash) {
        Some(address) => address,
        None => {
            let num_args = arity.required();
            let variadic_offset = if arity.variadic().is_some() { 1 } else { 0 };
            compiler.store_compiled_chunk(
                hash,
                Program::new(
                    once(Instruction::Function {
                        hash,
                        arity: num_args,
                        variadic: arity.variadic().is_some(),
                    })
                    .chain(
                        with_eagerness(0..arity.required(), &arity)
                            .into_iter()
                            .flat_map(|(_, eager)| {
                                once(Instruction::PushStatic {
                                    offset: num_args + variadic_offset - 1,
                                })
                                .chain(match eager {
                                    VarArgs::Eager => Some(Instruction::Evaluate),
                                    VarArgs::Lazy => None,
                                })
                            }),
                    )
                    .chain(arity.variadic().into_iter().flat_map(|eager| {
                        once(Instruction::PushStatic { offset: num_args }).chain(match eager {
                            VarArgs::Eager => Some(Instruction::EvaluateArgList),
                            VarArgs::Lazy => None,
                        })
                    }))
                    .chain(once(target))
                    .chain(once(Instruction::Apply {
                        num_args: num_args + variadic_offset,
                    }))
                    .chain(if num_args + variadic_offset > 0 {
                        Some(Instruction::Squash {
                            depth: num_args + variadic_offset,
                        })
                    } else {
                        None
                    })
                    .chain(once(Instruction::Return)),
                ),
            )
        }
    }
}
