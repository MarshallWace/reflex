// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::iter::once;

use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    FunctionArity, HeapAllocator, RefType, SignalType, StringTermType, StringValue, Uid, Uuid,
};

pub struct Effect;
impl Effect {
    pub const UUID: Uuid = uuid!("5bf0a450-0055-474f-ae42-ddb25e2d4d4d");
    const ARITY: FunctionArity<3, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Effect {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Effect {
    fn arity(&self) -> Option<Arity> {
        Some(Self::arity())
    }
    fn should_parallelize(&self, _args: &[T]) -> bool {
        false
    }
    fn apply(
        &self,
        mut args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let signal_type = args.next().unwrap();
        let payload = args.next().unwrap();
        let token = args.next().unwrap();
        if let Some(signal_type) = factory.match_string_term(&signal_type) {
            Ok(factory.create_effect_term(allocator.create_signal(
                SignalType::Custom(String::from(signal_type.value().as_deref().as_str())),
                payload,
                token,
            )))
        } else {
            Err(format!(
                "Expected (String, ...), received ({})",
                once(signal_type)
                    .chain(args)
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<_>>()
                    .join(", "),
            ))
        }
    }
}
