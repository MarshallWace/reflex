// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use reflex::core::{
    Applicable, Expression, ExpressionFactory, HeapAllocator, Reducible, Rewritable,
};
use reflex_dispatcher::{Action, TaskFactory};
use reflex_interpreter::compiler::Compile;
use reflex_macros::{blanket_trait, task_factory_enum};

use crate::{
    task::bytecode_worker::{BytecodeWorkerAction, BytecodeWorkerTask, BytecodeWorkerTaskFactory},
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator,
};

pub mod bytecode_worker;

blanket_trait!(
    pub trait RuntimeTaskAction<T: Expression>: BytecodeWorkerAction<T> {}
);
blanket_trait!(
    pub trait RuntimeTask<T, TFactory, TAllocator>:
        BytecodeWorkerTask<T, TFactory, TAllocator>
    where
        T: Expression,
        TFactory: ExpressionFactory<T>,
        TAllocator: HeapAllocator<T>,
    {
    }
);

// TODO: Implement Serialize/Deserialize traits for RuntimeTaskFactory
task_factory_enum!({
    #[derive(Clone)]
    pub enum RuntimeTaskFactory<T, TFactory, TAllocator>
    where
        T: Expression,
        TFactory: ExpressionFactory<T>,
        TAllocator: HeapAllocator<T>,
    {
        BytecodeWorker(BytecodeWorkerTaskFactory<T, TFactory, TAllocator>),
    }
    impl<T, TFactory, TAllocator, TAction, TTask> TaskFactory<TAction, TTask>
        for RuntimeTaskFactory<T, TFactory, TAllocator>
    where
        T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
        TFactory: AsyncExpressionFactory<T> + Default,
        TAllocator: AsyncHeapAllocator<T> + Default,
        TAction: Action + RuntimeTaskAction<T> + Send + 'static,
        TTask: TaskFactory<TAction, TTask>,
    {
    }
});
