// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::convert::TryInto;
use std::fmt::Formatter;
use std::marker::PhantomData;
use std::{collections::HashSet, iter::once};

use serde::de::{Error, SeqAccess, Visitor};
use serde::{Deserializer, Serializer};

use crate::core::Uuid;
use crate::{
    compiler::{Compile, Compiler, Instruction, Program},
    core::{
        Applicable, Arity, Builtin, DependencyList, EvaluationCache, Expression, ExpressionFactory,
        GraphNode, HeapAllocator, SerializeJson, StackOffset, Uid, VarArgs,
    },
};

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug)]
pub struct BuiltinTerm<TBuiltin: Builtin> {
    target: TBuiltin,
}
impl<TBuiltin: Builtin> BuiltinTerm<TBuiltin> {
    pub fn new(target: TBuiltin) -> Self {
        Self { target }
    }
    pub fn arity(&self) -> Arity {
        self.target.arity()
    }
}
impl<TBuiltin: Builtin> Uid for BuiltinTerm<TBuiltin> {
    fn uid(&self) -> Uuid {
        self.target.uid()
    }
}
impl<TBuiltin: Builtin> GraphNode for BuiltinTerm<TBuiltin> {
    fn capture_depth(&self) -> StackOffset {
        0
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        HashSet::new()
    }
    fn count_variable_usages(&self, _offset: StackOffset) -> usize {
        0
    }
    fn dynamic_dependencies(&self, _deep: bool) -> crate::core::DependencyList {
        DependencyList::empty()
    }
    fn has_dynamic_dependencies(&self, _deep: bool) -> bool {
        false
    }
    fn is_static(&self) -> bool {
        true
    }
    fn is_atomic(&self) -> bool {
        true
    }
    fn is_complex(&self) -> bool {
        false
    }
}
impl<T: Expression + Applicable<T>> Applicable<T> for BuiltinTerm<T::Builtin> {
    fn arity(&self) -> Option<Arity> {
        Some(self.target.arity())
    }
    fn apply(
        &self,
        args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        self.target
            .apply(args, factory, allocator, cache)
            .map_err(|err| format!("{}: {}", self, err))
    }
    fn should_parallelize(&self, args: &[T]) -> bool {
        self.target.should_parallelize(args)
    }
}
impl<T: Expression> Compile<T> for BuiltinTerm<T::Builtin> {
    fn compile(
        &self,
        _eager: VarArgs,
        _stack_offset: StackOffset,
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _compiler: &mut Compiler,
    ) -> Result<Program, String> {
        Ok(Program::new(once(Instruction::PushBuiltin {
            target: self.target.uid(),
        })))
    }
}
impl<TBuiltin: Builtin> std::fmt::Display for BuiltinTerm<TBuiltin> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.target, f)
    }
}
impl<TBuiltin: Builtin> SerializeJson for BuiltinTerm<TBuiltin> {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        Err(format!("Unable to serialize term: {}", self))
    }
}
impl<TBuiltin: Builtin> serde::ser::Serialize for BuiltinTerm<TBuiltin> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_bytes(self.target.uid().as_ref())
    }
}
impl<'de, TBuiltin: Builtin> serde::de::Deserialize<'de> for BuiltinTerm<TBuiltin> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_bytes(BuiltinTermDeserializeVisitor::<TBuiltin>::new())
    }
}
struct BuiltinTermDeserializeVisitor<TBuiltin: Builtin> {
    _phantom: PhantomData<TBuiltin>,
}
impl<TBuiltin: Builtin> BuiltinTermDeserializeVisitor<TBuiltin> {
    fn new() -> Self {
        Self {
            _phantom: PhantomData,
        }
    }
}
impl<'de, TBuiltin: Builtin> Visitor<'de> for BuiltinTermDeserializeVisitor<TBuiltin> {
    type Value = BuiltinTerm<TBuiltin>;
    fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
        formatter.write_str("Only accepts bytes representing a valid uuid")
    }
    fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
    where
        E: Error,
    {
        let uuid = Uuid::from_slice(v).map_err(|err| E::custom(format!("{:?}", err)))?;
        let builtin: TBuiltin = uuid
            .try_into()
            .map_err(|_err| E::custom("uuid not found"))?;
        Ok(BuiltinTerm::new(builtin))
    }
    // Some types of serde (e.g. serde-json) do not distinguish between bytes and a sequence
    // so we need to implement both visit methods
    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: SeqAccess<'de>,
    {
        let mut bytes = Vec::new();
        while let Ok(Some(byte)) = seq.next_element() {
            bytes.push(byte)
        }
        self.visit_bytes(&bytes)
    }
}
