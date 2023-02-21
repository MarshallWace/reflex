// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    FunctionArity, HeapAllocator, IntTermType, Uid, Uuid,
};

pub struct ParseFloat;
impl ParseFloat {
    pub const UUID: Uuid = uuid!("55e1f68f-e20f-4fab-a6f3-3e38b3dbd6ad");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for ParseFloat {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for ParseFloat {
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
        match factory.match_float_term(&value) {
            Some(_) => Ok(value),
            _ => match factory.match_int_term(&value) {
                Some(term) => Ok(factory.create_float_term(term.value() as f64)),
                _ => Err(format!(
                    "Invalid float conversion: Expected Int or Float, received {}",
                    value,
                )),
            },
        }
    }
}
