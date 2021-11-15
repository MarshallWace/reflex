// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::once;

use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    FunctionArity, HeapAllocator, SignalType, Uid, Uuid,
};
use reflex_handlers::SIGNAL_TYPE_SCAN;

pub struct Scan {}
impl Scan {
    pub(crate) const UUID: Uuid = uuid!("9c9f5a15-45a7-484d-a910-c6f114a8bced");
    const ARITY: FunctionArity<3, 0> = FunctionArity {
        required: [ArgType::Lazy, ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
}
impl Uid for Scan {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Scan {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(&Self::ARITY))
    }
    fn apply(
        &self,
        args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
        let target = args.next().unwrap();
        let seed = args.next().unwrap();
        let iteratee = args.next().unwrap();
        if !is_pure_expression(&seed) {
            Err(format!("Scan seed must be a pure expression"))
        } else if !is_pure_expression(&iteratee) {
            Err(format!("Scan iteratee must be a pure expression"))
        } else {
            let signal = allocator.create_signal(
                SignalType::Custom(String::from(SIGNAL_TYPE_SCAN)),
                allocator.create_triple(target, seed, iteratee),
            );
            Ok(factory.create_dynamic_variable_term(
                signal.id(),
                factory.create_signal_term(allocator.create_signal_list(once(signal))),
            ))
        }
    }
}

fn is_pure_expression<T: Expression>(expression: &T) -> bool {
    expression.capture_depth() == 0 && !expression.has_dynamic_dependencies(true)
}
