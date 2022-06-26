// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::iter::once;

use crate::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    FunctionArity, HeapAllocator, SignalType, StringValue, Uid, Uuid,
};

pub struct Effect {}
impl Effect {
    pub(crate) const UUID: Uuid = uuid!("5bf0a450-0055-474f-ae42-ddb25e2d4d4d");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: Some(ArgType::Strict),
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
        if let Some(signal_type) = factory.match_string_term(&signal_type) {
            let signal = allocator.create_signal(
                SignalType::Custom(String::from(signal_type.value.as_str())),
                allocator.create_list(args),
            );
            Ok(factory.create_dynamic_variable_term(
                signal.id(),
                factory.create_signal_term(allocator.create_signal_list(once(signal))),
            ))
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
