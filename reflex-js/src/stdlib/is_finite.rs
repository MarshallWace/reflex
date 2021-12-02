// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::{
    core::{
        uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
        FunctionArity, HeapAllocator, Uid, Uuid,
    },
    lang::ValueTerm,
};

pub struct IsFinite {}
impl IsFinite {
    pub(crate) const UUID: Uuid = uuid!("fc23d93b-fb6b-412b-a3ca-ad2d30953aeb");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: Some(ArgType::Eager),
    };
}
impl Uid for IsFinite {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for IsFinite {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(&Self::ARITY))
    }
    fn should_parallelize(&self, _args: &[T]) -> bool {
        false
    }
    fn apply(
        &self,
        args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
        let value = args.next().unwrap();
        match factory.match_value_term(&value) {
            Some(ValueTerm::Float(value)) => {
                Ok(factory.create_value_term(ValueTerm::Boolean(value.is_finite())))
            }
            Some(ValueTerm::Int(_)) => Ok(factory.create_value_term(ValueTerm::Boolean(true))),
            _ => Err(format!("Expected Float or Int, received {}", value,)),
        }
    }
}
