// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{collections::HashSet, hash::Hash, iter::once};

use crate::{
    compiler::{
        Compile, Compiler, Instruction, InstructionPointer, NativeFunctionRegistry, Program,
    },
    core::{
        Applicable, Arity, DependencyList, EvaluationCache, Expression, ExpressionFactory,
        ExpressionList, GraphNode, HeapAllocator, NativeAllocator, StackOffset, VarArgs,
    },
    hash::HashId,
    lang::compile_function,
};

pub type NativeFunctionId = HashId;

#[derive(Clone)]
pub struct NativeFunction<T: Expression> {
    uid: NativeFunctionId,
    name: Option<&'static str>,
    arity: Arity,
    apply: fn(
        ExpressionList<T>,
        &dyn ExpressionFactory<T>,
        &dyn NativeAllocator<T>,
    ) -> Result<T, String>,
}
impl<T: Expression> NativeFunction<T> {
    pub fn new(
        uid: NativeFunctionId,
        name: Option<&'static str>,
        arity: Arity,
        apply: fn(
            ExpressionList<T>,
            &dyn ExpressionFactory<T>,
            &dyn NativeAllocator<T>,
        ) -> Result<T, String>,
    ) -> Self {
        Self {
            uid,
            name,
            arity,
            apply,
        }
    }
    pub fn uid(&self) -> NativeFunctionId {
        self.uid
    }
    pub fn name(&self) -> Option<&'static str> {
        self.name
    }
    pub fn arity(&self) -> Arity {
        self.arity
    }
}
impl<T: Expression> PartialEq for NativeFunction<T> {
    fn eq(&self, other: &Self) -> bool {
        self.uid == other.uid
    }
}
impl<T: Expression> std::fmt::Display for NativeFunction<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "<native:{}>",
            match self.name {
                Some(name) => String::from(name),
                None => format!("{:016x}", self.uid),
            },
        )
    }
}
impl<T: Expression> std::fmt::Debug for NativeFunction<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}

pub struct NativeFunctionTerm<T: Expression> {
    target: NativeFunction<T>,
}
impl<T: Expression> NativeFunctionTerm<T> {
    pub fn new(target: NativeFunction<T>) -> Self {
        Self { target }
    }
    pub fn uid(&self) -> NativeFunctionId {
        self.target.uid
    }
    pub fn target(&self) -> &NativeFunction<T> {
        &self.target
    }
}
impl<T: Expression> Applicable<T> for NativeFunctionTerm<T> {
    fn arity(&self) -> Option<Arity> {
        Some(self.target.arity)
    }
    fn apply(
        &self,
        args: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        (self.target.apply)(allocator.create_list(args), factory, allocator.as_object())
    }
}
impl<T: Expression> GraphNode for NativeFunctionTerm<T> {
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
impl<T: Expression> Eq for NativeFunctionTerm<T> {}
impl<T: Expression> Hash for NativeFunctionTerm<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_u64(self.uid());
    }
}
impl<T: Expression> PartialEq for NativeFunctionTerm<T> {
    fn eq(&self, other: &Self) -> bool {
        self.uid() == other.uid()
    }
}
impl<T: Expression + Compile<T>> Compile<T> for NativeFunctionTerm<T> {
    fn compile(
        &self,
        _eager: VarArgs,
        _stack_offset: StackOffset,
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<(Program, NativeFunctionRegistry<T>), String> {
        let compiled_address = compile_native_function(&self.target, compiler);
        Ok((
            Program::new(once(Instruction::PushFunction {
                target: compiled_address,
            })),
            NativeFunctionRegistry::from((self.target.clone(), compiled_address)),
        ))
    }
}
impl<T: Expression> std::fmt::Display for NativeFunctionTerm<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.target, f)
    }
}
impl<T: Expression> std::fmt::Debug for NativeFunctionTerm<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}
impl<T: Expression> serde::Serialize for NativeFunctionTerm<T> {
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

pub(crate) fn compile_native_function<T: Expression>(
    target: &NativeFunction<T>,
    compiler: &mut Compiler,
) -> InstructionPointer {
    compile_function(
        target.uid(),
        target.arity(),
        Instruction::PushNative {
            target: target.uid(),
        },
        compiler,
    )
}
