// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{collections::HashSet, iter::once};

use serde::{Deserialize, Serialize};

use crate::{
    compiler::{Compile, Compiler, Instruction, Program},
    core::{
        Applicable, Arity, Builtin, DependencyList, EvaluationCache, Expression, ExpressionFactory,
        GraphNode, HeapAllocator, SerializeJson, StackOffset, Uid, VarArgs,
    },
};

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug, Serialize, Deserialize)]
#[serde(tag = "type")]
pub struct BuiltinTerm<T: Expression> {
    target: T::Builtin,
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
