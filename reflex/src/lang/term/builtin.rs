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
pub struct BuiltinTerm<T: Expression> {
    target: T::Builtin,
}
impl<T: Expression> serde::ser::Serialize for BuiltinTerm<T>
where
    T::String: serde::ser::Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_bytes(self.target.uid().as_ref())
    }
}
impl<'de, T: Expression> serde::de::Deserialize<'de> for BuiltinTerm<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_bytes(BuiltinTermVisitor::<T>::new())
    }
}
struct BuiltinTermVisitor<T> {
    _phantom: PhantomData<T>,
}
impl<T> BuiltinTermVisitor<T> {
    fn new() -> Self {
        Self {
            _phantom: PhantomData,
        }
    }
}
impl<'de, T: Expression> Visitor<'de> for BuiltinTermVisitor<T> {
    type Value = BuiltinTerm<T>;

    fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
        formatter.write_str("Only accepts bytes representing a valid uuid")
    }

    fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
    where
        E: Error,
    {
        let uuid = Uuid::from_slice(v).map_err(|err| E::custom(format!("{:?}", err)))?;
        let builtin: T::Builtin = uuid
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

impl<T: Expression> BuiltinTerm<T> {
    pub fn new(target: T::Builtin) -> Self {
        Self { target }
    }
}
impl<T: Expression> GraphNode for BuiltinTerm<T> {
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
}
impl<T: Expression + Applicable<T>> Applicable<T> for BuiltinTerm<T> {
    fn arity(&self) -> Option<Arity> {
        Builtin::arity::<T>(&self.target)
    }
    fn apply(
        &self,
        args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        Builtin::apply(&self.target, args, factory, allocator, cache)
            .map_err(|err| format!("{}: {}", self, err))
    }
}
impl<T: Expression + Applicable<T> + Compile<T>> Compile<T> for BuiltinTerm<T> {
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
impl<T: Expression> std::fmt::Display for BuiltinTerm<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.target, f)
    }
}
impl<T: Expression> SerializeJson for BuiltinTerm<T> {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        Err(format!("Unable to serialize term: {}", self))
    }
}
