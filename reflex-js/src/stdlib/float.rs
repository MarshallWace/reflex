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

pub struct Float {}
impl Float {
    pub(crate) const UUID: Uuid = uuid!("55e1f68f-e20f-4fab-a6f3-3e38b3dbd6ad");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: Some(ArgType::Eager),
    };
}
impl Uid for Float {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Float {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(&Self::ARITY))
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
            Some(ValueTerm::Float(_)) => Ok(value),
            Some(ValueTerm::Int(value)) => {
                Ok(factory.create_value_term(ValueTerm::Float(*value as f64)))
            }
            _ => Err(format!(
                "Invalid float conversion: Expected Int or Float, received {}",
                value,
            )),
        }
    }
}
