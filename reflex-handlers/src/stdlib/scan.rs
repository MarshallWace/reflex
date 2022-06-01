// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::once;

use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    FunctionArity, HeapAllocator, SignalType, Uid, Uuid,
};

use crate::actor::scan::EFFECT_TYPE_SCAN;

pub struct Scan {}
impl Scan {
    pub(crate) const UUID: Uuid = uuid!("9c9f5a15-45a7-484d-a910-c6f114a8bced");
    const ARITY: FunctionArity<4, 0> = FunctionArity {
        required: [
            ArgType::Strict,
            ArgType::Lazy,
            ArgType::Strict,
            ArgType::Strict,
        ],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Scan {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Scan {
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
        let mut args = args.into_iter();
        let name = args.next().unwrap();
        let target = args.next().unwrap();
        let seed = args.next().unwrap();
        let iteratee = args.next().unwrap();
        match factory
            .match_value_term(&name)
            .and_then(|value| value.match_string())
        {
            None => Err(format!(
                "Invalid scan name: Expected String, received {}",
                name
            )),
            Some(_) => {
                if !is_pure_expression(&seed) {
                    Err(format!("Scan seed must be a pure expression"))
                } else if !is_pure_expression(&iteratee) {
                    Err(format!("Scan iteratee must be a pure expression"))
                } else {
                    let signal = allocator.create_signal(
                        SignalType::Custom(String::from(EFFECT_TYPE_SCAN)),
                        allocator.create_list([name, target, seed, iteratee]),
                    );
                    Ok(factory.create_dynamic_variable_term(
                        signal.id(),
                        factory.create_signal_term(allocator.create_signal_list(once(signal))),
                    ))
                }
            }
        }
    }
}

fn is_pure_expression<T: Expression>(expression: &T) -> bool {
    expression.capture_depth() == 0 && !expression.has_dynamic_dependencies(true)
}
