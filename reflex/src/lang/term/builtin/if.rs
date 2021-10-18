// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{
        Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory, FunctionArity,
        HeapAllocator,
    },
    lang::ValueTerm,
};

pub struct If {}
impl If {
    const ARITY: FunctionArity<3, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Lazy, ArgType::Lazy],
        optional: [],
        variadic: None,
    };
}
impl<T: Expression> Applicable<T> for If {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(&Self::ARITY))
    }
    fn apply(
        &self,
        args: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
        factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
        let condition = args.next().unwrap();
        let consequent = args.next().unwrap();
        let alternate = args.next().unwrap();
        if is_truthy(&condition, factory) {
            Ok(consequent)
        } else {
            Ok(alternate)
        }
    }
}

pub fn is_truthy<T: Expression>(value: &T, factory: &impl ExpressionFactory<T>) -> bool {
    match factory.match_value_term(value) {
        Some(ValueTerm::Boolean(value)) => *value,
        Some(ValueTerm::Null) => false,
        _ => true,
    }
}
