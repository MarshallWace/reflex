// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{iter::once, sync::Arc};

use futures::{stream, FutureExt};
use reflex::{
    compiler::{
        create_main_function, Compile, Compiler, CompilerMode, CompilerOptions, Instruction,
        InstructionPointer, Program,
    },
    core::{
        Applicable, Expression, ExpressionFactory, HeapAllocator, Reducible, Rewritable, Signal,
        SignalId, SignalType, StateToken,
    },
    lang::ValueTerm,
};
use tokio_stream::Stream;

use crate::{
    create_subscription, AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator,
    RuntimeCommandChannel, RuntimeEffect,
};

pub trait SignalHandler<T>: Send + Sync + Clone + 'static
where
    T: AsyncExpression + Compile<T>,
    T::String: Send + Sync,
{
    fn handle(
        &self,
        signal_type: &str,
        signals: &[&Signal<T>],
        helpers: &SignalHelpers<T>,
    ) -> SignalHandlerResult<T>;
}
pub type SignalResult<T> = (T, Option<RuntimeEffect<T>>);
pub type SignalHandlerResult<T> = Option<Vec<Result<SignalResult<T>, String>>>;
impl<T, F> SignalHandler<T> for F
where
    T: AsyncExpression + Compile<T>,
    T::String: Send + Sync,
    F: Fn(&str, &[&Signal<T>], &SignalHelpers<T>) -> SignalHandlerResult<T>
        + Send
        + Sync
        + Clone
        + 'static,
{
    fn handle(
        &self,
        signal_type: &str,
        signals: &[&Signal<T>],
        helpers: &SignalHelpers<T>,
    ) -> SignalHandlerResult<T> {
        self(signal_type, signals, helpers)
    }
}

#[derive(Clone)]
pub struct SignalHelpers<T: AsyncExpression + Compile<T>>
where
    T::String: Send + Sync,
{
    program: Arc<Program>,
    commands: RuntimeCommandChannel<T>,
    compiler_options: CompilerOptions,
}
impl<T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>>
    SignalHelpers<T>
where
    T::String: Send + Sync,
{
    pub(crate) fn new(
        program: Arc<Program>,
        commands: RuntimeCommandChannel<T>,
        compiler_options: CompilerOptions,
    ) -> Self {
        Self {
            program,
            commands,
            compiler_options,
        }
    }
    pub fn program(&self) -> &Program {
        &self.program
    }
    pub fn watch_expression(
        self,
        expression: T,
        initiators: impl IntoIterator<Item = SignalId>,
        factory: &impl AsyncExpressionFactory<T>,
        allocator: &impl AsyncHeapAllocator<T>,
    ) -> Result<impl Stream<Item = T> + Send + 'static, String> {
        let initiators = initiators.into_iter().collect::<Vec<_>>();
        let entry_point = InstructionPointer::new(self.program.len());
        let prelude = (*self.program).clone();
        Compiler::new(self.compiler_options, Some(prelude))
            .compile(&expression, CompilerMode::Function, factory, allocator)
            .map(|program| {
                // TODO: Error if runtime expression depends on unrecognized native functions
                self.watch_compiled_expression(program, entry_point, initiators, factory, allocator)
            })
    }
    pub fn watch_state(
        self,
        state_token: StateToken,
        initiators: impl IntoIterator<Item = SignalId>,
        factory: &impl AsyncExpressionFactory<T>,
        allocator: &impl AsyncHeapAllocator<T>,
    ) -> impl Stream<Item = T> + Send + 'static {
        let initiators = initiators.into_iter().collect::<Vec<_>>();
        let program = compile_state_subscription_expression(state_token);
        self.watch_compiled_expression(
            program,
            InstructionPointer::default(),
            initiators,
            factory,
            allocator,
        )
    }
    fn watch_compiled_expression(
        self,
        program: Program,
        entry_point: InstructionPointer,
        initiators: Vec<SignalId>,
        factory: &impl AsyncExpressionFactory<T>,
        allocator: &impl AsyncHeapAllocator<T>,
    ) -> impl Stream<Item = T> + Send + 'static {
        futures::StreamExt::flatten(
            {
                let factory = factory.clone();
                let allocator = allocator.clone();
                async move {
                    match create_subscription(
                        &self.commands,
                        program,
                        entry_point,
                        Some(initiators),
                    )
                    .await
                    {
                        Err(error) => futures::StreamExt::left_stream(stream::iter(once(
                            create_error_expression(
                                once(factory.create_value_term(ValueTerm::String(
                                    allocator.create_string(error),
                                ))),
                                &factory,
                                &allocator,
                            ),
                        ))),
                        Ok(subscription) => {
                            futures::StreamExt::right_stream(subscription.into_stream())
                        }
                    }
                }
            }
            .into_stream(),
        )
    }
}

fn compile_state_subscription_expression(state_token: StateToken) -> Program {
    create_main_function([
        Instruction::PushSignal {
            signal_type: SignalType::Pending,
            num_args: 0,
        },
        Instruction::PushDynamic { state_token },
        Instruction::Evaluate,
        Instruction::Return,
    ])
}

fn create_error_expression<T: Expression>(
    errors: impl IntoIterator<Item = T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_signal_term(
        allocator.create_signal_list(errors.into_iter().map(|error| {
            allocator.create_signal(SignalType::Error, allocator.create_unit_list(error))
        })),
    )
}
