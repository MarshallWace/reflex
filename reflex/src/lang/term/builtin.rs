// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{collections::HashSet, iter::once};

use crate::{
    compiler::{Compile, Compiler, Instruction, NativeFunctionRegistry, Program},
    core::{
        Applicable, Arity, DependencyList, EvaluationCache, Expression, ExpressionFactory,
        GraphNode, HeapAllocator, Reducible, Rewritable, StackOffset, VarArgs,
    },
    hash::hash_object,
    lang::with_eagerness,
};

mod add;
pub use add::*;
mod abs;
pub use abs::*;
mod and;
pub use and::*;
mod append;
pub use append::*;
mod car;
pub use car::*;
mod cdr;
pub use cdr::*;
mod ceil;
pub use ceil::*;
mod collect;
pub use collect::*;
mod concat;
pub use concat::*;
mod cons;
pub use cons::*;
mod contains;
pub use contains::*;
mod divide;
pub use divide::*;
mod effect;
pub use effect::*;
mod ends_with;
pub use ends_with::*;
mod entries;
pub use entries::*;
mod eq;
pub use eq::*;
mod equal;
pub use equal::*;
mod filter;
pub use filter::*;
mod floor;
pub use floor::*;
mod get;
pub use get::*;
mod gt;
pub use gt::*;
mod gte;
pub use gte::*;
mod r#if;
pub use r#if::*;
mod hashmap;
pub use hashmap::*;
mod insert;
pub use insert::*;
mod keys;
pub use keys::*;
mod lt;
pub use lt::*;
mod lte;
pub use lte::*;
mod map;
pub use map::*;
mod r#match;
pub use r#match::*;
mod max;
pub use max::*;
mod merge;
pub use merge::*;
mod min;
pub use min::*;
mod multiply;
pub use multiply::*;
mod not;
pub use not::*;
mod or;
pub use or::*;
mod pow;
pub use pow::*;
mod push;
pub use push::*;
mod push_front;
pub use push_front::*;
mod reduce;
pub use reduce::*;
mod remainder;
pub use remainder::*;
mod replace;
pub use replace::*;
mod resolve_deep;
pub use resolve_deep::*;
mod round;
pub use round::*;
mod slice;
pub use slice::*;
mod split;
pub use split::*;
mod starts_with;
pub use starts_with::*;
mod r#struct;
pub use r#struct::*;
mod subtract;
pub use subtract::*;
mod tuple;
pub use tuple::*;
mod values;
pub use values::*;

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug)]
pub enum BuiltinTerm {
    Add,
    Abs,
    And,
    Append,
    Car,
    Cdr,
    Ceil,
    Collect,
    CollectFilterResults,
    CollectHashMap,
    CollectHashSet,
    CollectTuple,
    CollectVector,
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
    HashMap,
    If,
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
    Round,
    Slice,
    Split,
    StartsWith,
    Struct,
    Subtract,
    Tuple,
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
impl<T: Expression + Applicable<T> + Rewritable<T> + Reducible<T> + Compile<T>> Applicable<T>
    for BuiltinTerm
{
    fn arity(&self) -> Option<Arity> {
        match self {
            Self::Add => Applicable::<T>::arity(&Add {}),
            Self::Abs => Applicable::<T>::arity(&Abs {}),
            Self::And => Applicable::<T>::arity(&And {}),
            Self::Append => Applicable::<T>::arity(&Append {}),
            Self::Car => Applicable::<T>::arity(&Car {}),
            Self::Cdr => Applicable::<T>::arity(&Cdr {}),
            Self::Ceil => Applicable::<T>::arity(&Ceil {}),
            Self::Collect => Applicable::<T>::arity(&Collect {}),
            Self::CollectFilterResults => Applicable::<T>::arity(&CollectFilterResults {}),
            Self::CollectHashMap => Applicable::<T>::arity(&CollectHashMap {}),
            Self::CollectHashSet => Applicable::<T>::arity(&CollectHashSet {}),
            Self::CollectTuple => Applicable::<T>::arity(&CollectTuple {}),
            Self::CollectVector => Applicable::<T>::arity(&CollectVector {}),
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
            Self::HashMap => Applicable::<T>::arity(&HashMap {}),
            Self::If => Applicable::<T>::arity(&If {}),
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
            Self::Round => Applicable::<T>::arity(&Round {}),
            Self::Slice => Applicable::<T>::arity(&Slice {}),
            Self::Split => Applicable::<T>::arity(&Replace {}),
            Self::StartsWith => Applicable::<T>::arity(&StartsWith {}),
            Self::Struct => Applicable::<T>::arity(&Struct {}),
            Self::Subtract => Applicable::<T>::arity(&Subtract {}),
            Self::Tuple => Applicable::<T>::arity(&Tuple {}),
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
            Self::CollectTuple => {
                Applicable::<T>::apply(&CollectTuple {}, args, factory, allocator, cache)
            }
            Self::CollectVector => {
                Applicable::<T>::apply(&CollectVector {}, args, factory, allocator, cache)
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
            Self::HashMap => Applicable::<T>::apply(&HashMap {}, args, factory, allocator, cache),
            Self::If => Applicable::<T>::apply(&If {}, args, factory, allocator, cache),
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
            Self::Round => Applicable::<T>::apply(&Round {}, args, factory, allocator, cache),
            Self::Slice => Applicable::<T>::apply(&Slice {}, args, factory, allocator, cache),
            Self::Split => Applicable::<T>::apply(&Replace {}, args, factory, allocator, cache),
            Self::StartsWith => {
                Applicable::<T>::apply(&StartsWith {}, args, factory, allocator, cache)
            }
            Self::Struct => Applicable::<T>::apply(&Struct {}, args, factory, allocator, cache),
            Self::Subtract => Applicable::<T>::apply(&Subtract {}, args, factory, allocator, cache),
            Self::Tuple => Applicable::<T>::apply(&Tuple {}, args, factory, allocator, cache),
            Self::Values => Applicable::<T>::apply(&Values {}, args, factory, allocator, cache),
        }
        .map_err(|err| format!("{}: {}", self, err))
    }
}
impl<T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>> Compile<T>
    for BuiltinTerm
{
    fn compile(
        &self,
        _eager: VarArgs,
        _stack_offset: StackOffset,
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<(Program, NativeFunctionRegistry<T>), String> {
        match Applicable::<T>::arity(self) {
            None => Err(format!("Unable to compile builtin function: {}", self)),
            Some(arity) => match arity.variadic() {
                Some(_) => Ok((
                    Program::new(once(Instruction::PushBuiltin { target: *self })),
                    NativeFunctionRegistry::default(),
                )),
                None => {
                    let hash = hash_object(self);
                    Ok((
                        Program::new(once(Instruction::PushFunction {
                            target: match compiler.retrieve_compiled_chunk_address(hash) {
                                Some(address) => address,
                                None => {
                                    let num_args = arity.required();
                                    compiler.store_compiled_chunk(
                                        hash,
                                        Program::new(
                                            once(Instruction::Function {
                                                hash,
                                                arity: num_args,
                                            })
                                            .chain(
                                                with_eagerness(0..arity.required(), &arity)
                                                    .into_iter()
                                                    .flat_map(|(_, eager)| {
                                                        once(Instruction::PushStatic {
                                                            offset: num_args - 1,
                                                        })
                                                        .chain(match eager {
                                                            VarArgs::Eager => {
                                                                Some(Instruction::Evaluate)
                                                            }
                                                            VarArgs::Lazy => None,
                                                        })
                                                    }),
                                            )
                                            .chain(once(Instruction::PushBuiltin { target: *self }))
                                            .chain(once(Instruction::Apply { num_args }))
                                            .chain(if num_args > 0 {
                                                Some(Instruction::Squash { depth: num_args })
                                            } else {
                                                None
                                            })
                                            .chain(once(Instruction::Return)),
                                        ),
                                    )
                                }
                            },
                        })),
                        NativeFunctionRegistry::default(),
                    ))
                }
            },
        }
    }
}
impl std::fmt::Display for BuiltinTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<builtin:{:?}>", self)
    }
}
impl serde::Serialize for BuiltinTerm {
    fn serialize<S>(&self, _serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        Err(serde::ser::Error::custom(format!(
            "Unable to serialize term: {}",
            self
        )))
    }
}
