// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::HashSet,
    iter::{empty, once},
};

use crate::{
    compiler::{
        compile_expressions, Compile, Compiler, Instruction, NativeFunctionRegistry, Program,
    },
    core::{
        Applicable, Arity, DependencyList, DynamicState, EvaluationCache, Expression,
        ExpressionFactory, GraphNode, HeapAllocator, Reducible, Rewritable, Signal, SignalList,
        StackOffset, Substitutions, VarArgs,
    },
};

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct SignalTerm<T: Expression> {
    signals: SignalList<T>,
}
impl<T: Expression> SignalTerm<T> {
    pub fn new(signals: SignalList<T>) -> Self {
        Self { signals }
    }
    pub fn signals(&self) -> &SignalList<T> {
        &self.signals
    }
}
impl<T: Expression> GraphNode for SignalTerm<T> {
    fn capture_depth(&self) -> StackOffset {
        0
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        HashSet::new()
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        DependencyList::empty()
    }
    fn is_static(&self) -> bool {
        true
    }
    fn is_atomic(&self) -> bool {
        true
    }
}
impl<T: Expression + Rewritable<T> + Reducible<T> + Compile<T>> Compile<T> for SignalTerm<T> {
    fn compile(
        &self,
        _eager: VarArgs,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<(Program, NativeFunctionRegistry<T>), String> {
        self.signals().iter().enumerate().fold(
            Ok((Program::new(empty()), NativeFunctionRegistry::default())),
            |results, (index, signal)| {
                let (mut program, mut native_functions) = results?;
                match compile_signal(signal, stack_offset + index, factory, allocator, compiler) {
                    Err(error) => Err(error),
                    Ok((compiled_signal, signal_native_functions)) => {
                        program.extend(compiled_signal);
                        native_functions.extend(signal_native_functions);
                        Ok((program, native_functions))
                    }
                }
            },
        )
    }
}
impl<T: Expression> std::fmt::Display for SignalTerm<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}]",
            self.signals
                .iter()
                .map(|signal| format!("{}", signal))
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}
impl<T: Expression> serde::Serialize for SignalTerm<T> {
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

fn compile_signal<T: Expression + Compile<T>>(
    signal: &Signal<T>,
    stack_offset: StackOffset,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    compiler: &mut Compiler,
) -> Result<(Program, NativeFunctionRegistry<T>), String> {
    let (compiled_args, native_functions) = compile_expressions(
        signal.args().iter(),
        VarArgs::Lazy,
        stack_offset,
        factory,
        allocator,
        compiler,
    )?;
    let mut result = compiled_args;
    result.push(Instruction::PushSignal {
        signal_type: signal.signal_type().clone(),
        num_args: signal.args().len(),
    });
    Ok((result, native_functions))
}

#[derive(Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub struct SignalTransformerTerm<T: Expression> {
    handler: T,
}
impl<T: Expression> SignalTransformerTerm<T> {
    pub fn new(handler: T) -> Self {
        Self { handler }
    }
    pub fn handler(&self) -> &T {
        &self.handler
    }
}
impl<T: Expression> GraphNode for SignalTransformerTerm<T> {
    fn capture_depth(&self) -> StackOffset {
        self.handler.capture_depth()
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        self.handler.free_variables()
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        self.handler.dynamic_dependencies()
    }
    fn is_static(&self) -> bool {
        true
    }
    fn is_atomic(&self) -> bool {
        false
    }
}
impl<T: Expression + Rewritable<T>> Rewritable<T> for SignalTransformerTerm<T> {
    fn subexpressions(&self) -> Vec<&T> {
        once(&self.handler)
            .chain(self.handler.subexpressions())
            .collect()
    }
    fn substitute_static(
        &self,
        substitutions: &Substitutions<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        self.handler
            .substitute_static(substitutions, factory, allocator, cache)
            .map(|handler| factory.create_signal_transformer_term(handler))
    }
    fn substitute_dynamic(
        &self,
        state: &impl DynamicState<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        self.handler
            .substitute_dynamic(state, factory, allocator, cache)
            .map(|handler| factory.create_signal_transformer_term(handler))
    }
    fn hoist_free_variables(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Option<T> {
        self.handler
            .hoist_free_variables(factory, allocator)
            .map(|handler| factory.create_signal_transformer_term(handler))
    }
    fn normalize(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        self.handler
            .normalize(factory, allocator, cache)
            .map(|handler| factory.create_signal_transformer_term(handler))
    }
}
impl<T: Expression> Applicable<T> for SignalTransformerTerm<T> {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(1, 0, None))
    }
    fn apply(
        &self,
        args: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let arg = args.into_iter().next().unwrap();
        if let Some(_) = factory.match_signal_term(&arg) {
            Ok(factory
                .create_application_term(self.handler.clone(), allocator.create_unit_list(arg)))
        } else {
            Ok(arg)
        }
    }
}
impl<T: Expression + Rewritable<T> + Reducible<T> + Compile<T>> Compile<T>
    for SignalTransformerTerm<T>
{
    fn compile(
        &self,
        eager: VarArgs,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<(Program, NativeFunctionRegistry<T>), String> {
        let (compiled_handler, native_functions) =
            self.handler
                .compile(eager, stack_offset, factory, allocator, compiler)?;
        let mut result = compiled_handler;
        result.push(Instruction::ConstructSignalTransformer);
        Ok((result, native_functions))
    }
}
impl<T: Expression> std::fmt::Display for SignalTransformerTerm<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<transformer:{}>", self.handler)
    }
}
impl<T: Expression> serde::Serialize for SignalTransformerTerm<T> {
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
