// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{collections::HashSet, hash::Hash, iter::once};

use crate::{
    compiler::{Compile, Compiler, Instruction, NativeFunctionRegistry, Program},
    core::{
        Applicable, Arity, DependencyList, EvaluationCache, Expression, ExpressionFactory,
        ExpressionList, GraphNode, HeapAllocator, NativeAllocator, Reducible, Rewritable,
        StackOffset, VarArgs,
    },
    hash::{hash_object, HashId},
    lang::with_eagerness,
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
impl<T: Expression + Rewritable<T> + Reducible<T> + Compile<T>> Compile<T>
    for NativeFunctionTerm<T>
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
            None => Err(format!("Unable to compile native function: {}", self)),
            Some(arity) => match arity.variadic() {
                Some(_) => Ok((
                    Program::new(once(Instruction::PushNative {
                        target: self.target.uid(),
                    })),
                    NativeFunctionRegistry::from(self.target.clone()),
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
                                            once(Instruction::Function { hash, arity: num_args })
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
                                                .chain(once(Instruction::PushNative {
                                                    target: self.target.uid(),
                                                }))
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
                        NativeFunctionRegistry::from(self.target.clone()),
                    ))
                }
            },
        }
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
