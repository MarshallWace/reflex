// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::{collections::HashSet, convert::TryInto, fmt::Formatter, marker::PhantomData};

use serde::{
    de::{Error, SeqAccess, Visitor},
    Deserializer, Serializer,
};

use reflex::core::{
    Applicable, Arity, Builtin, BuiltinTermType, DependencyList, Eagerness, EvaluationCache,
    Expression, ExpressionFactory, GraphNode, HeapAllocator, Internable, SerializeJson,
    StackOffset, Uid, Uuid,
};

#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub struct BuiltinTerm<T: Expression> {
    target: T::Builtin,
}

impl<T: Expression> std::hash::Hash for BuiltinTerm<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.target.uid().hash(state);
    }
}

impl<T: Expression> BuiltinTerm<T> {
    pub fn new(target: T::Builtin) -> Self {
        Self { target }
    }
}
impl<T: Expression> BuiltinTermType<T> for BuiltinTerm<T> {
    fn target<'a>(&'a self) -> T::Ref<'a, T::Builtin>
    where
        T::Builtin: 'a,
        T: 'a,
    {
        (&self.target).into()
    }
}
impl<T: Expression> Uid for BuiltinTerm<T> {
    fn uid(&self) -> Uuid {
        self.target.uid()
    }
}
impl<T: Expression> GraphNode for BuiltinTerm<T> {
    fn size(&self) -> usize {
        1
    }
    fn capture_depth(&self) -> StackOffset {
        0
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        HashSet::new()
    }
    fn count_variable_usages(&self, _offset: StackOffset) -> usize {
        0
    }
    fn dynamic_dependencies(&self, _deep: bool) -> DependencyList {
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
impl<T: Expression + Applicable<T>> Applicable<T> for BuiltinTerm<T> {
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

impl<T: Expression> Internable for BuiltinTerm<T> {
    fn should_intern(&self, _eager: Eagerness) -> bool {
        true
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
impl<T: Expression> serde::ser::Serialize for BuiltinTerm<T> {
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
        deserializer.deserialize_bytes(BuiltinTermDeserializeVisitor::<T>::new())
    }
}
struct BuiltinTermDeserializeVisitor<T: Expression> {
    _phantom: PhantomData<T>,
}
impl<T: Expression> BuiltinTermDeserializeVisitor<T> {
    fn new() -> Self {
        Self {
            _phantom: PhantomData,
        }
    }
}
impl<'de, T: Expression> Visitor<'de> for BuiltinTermDeserializeVisitor<T> {
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
