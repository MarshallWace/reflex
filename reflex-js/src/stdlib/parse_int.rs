// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    FloatTermType, FunctionArity, HeapAllocator, IntValue, Uid, Uuid,
};

pub struct ParseInt;
impl ParseInt {
    pub const UUID: Uuid = uuid!("62e9e98a-6f04-46d6-b97e-60aa5bf505e4");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for ParseInt {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for ParseInt {
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
        _allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
        let value = args.next().unwrap();
        match factory.match_int_term(&value) {
            Some(_) => Ok(value),
            _ => match factory.match_float_term(&value) {
                Some(term) => Ok(factory.create_int_term(term.value() as IntValue)),
                _ => Err(format!(
                    "Invalid integer conversion: Expected Float or Int, received {}",
                    value,
                )),
            },
        }
    }
}
