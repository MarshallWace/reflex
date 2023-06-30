// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::once;

use reflex::core::{
    uuid, Applicable, ArgType, Arity, Builtin, EvaluationCache, Expression, ExpressionFactory,
    FunctionArity, HeapAllocator, Uid, Uuid,
};
use reflex_json::stringify;
use reflex_stdlib::ResolveDeep;

pub trait LogBuiltin: Builtin + From<LogArgs> + From<ResolveDeep> {}
impl<T> LogBuiltin for T where T: Builtin + From<LogArgs> + From<ResolveDeep> {}

pub struct Log;
impl Log {
    pub const UUID: Uuid = uuid!("c0f30755-91b9-4252-b038-1aa3cd6f267c");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Eager],
        optional: [],
        variadic: Some(ArgType::Eager),
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Log {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Log
where
    T::Builtin: LogBuiltin,
{
    fn arity(&self) -> Option<Arity> {
        Some(Self::arity())
    }
    fn should_parallelize(&self, _args: &[T]) -> bool {
        false
    }
    fn apply(
        &self,
        args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let args = args.into_iter();
        Ok(factory.create_application_term(
            factory.create_builtin_term(LogArgs),
            allocator.create_list(args.map(|arg| {
                factory.create_application_term(
                    factory.create_builtin_term(ResolveDeep),
                    allocator.create_unit_list(arg),
                )
            })),
        ))
    }
}

pub struct LogArgs;
impl LogArgs {
    pub const UUID: Uuid = uuid!("ee23acbf-a549-498c-b463-12373556221a");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Eager],
        optional: [],
        variadic: Some(ArgType::Eager),
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for LogArgs {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for LogArgs {
    fn arity(&self) -> Option<Arity> {
        Some(Self::arity())
    }
    fn should_parallelize(&self, _args: &[T]) -> bool {
        false
    }
    fn apply(
        &self,
        args: impl ExactSizeIterator<Item = T>,
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
        let expression = args.next().unwrap();
        println!(
            "{}",
            once(stringify_value(&expression))
                .chain(args.map(|value| stringify_value(&value)))
                .collect::<Vec<_>>()
                .join(" ")
        );
        Ok(expression)
    }
}

fn stringify_value<T: Expression>(expression: &T) -> String {
    match stringify(expression) {
        Ok(result) => {
            format!("{}", result)
        }
        Err(_) => {
            format!("{}", expression)
        }
    }
}
