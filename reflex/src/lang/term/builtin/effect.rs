// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::once;

use crate::{
    core::{
        Applicable, Arity, EvaluationCache, Expression, ExpressionFactory, HeapAllocator,
        SignalType, StringValue, VarArgs,
    },
    lang::ValueTerm,
};

pub struct Effect {}
impl<T: Expression> Applicable<T> for Effect {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(1, 0, Some(VarArgs::Eager)))
    }
    fn apply(
        &self,
        args: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
        if args.len() < 1 {
            return Err(format!(
                "Expected 1 or more arguments, received {}",
                args.len()
            ));
        }
        let signal_type = args.next().unwrap();
        if let Some(ValueTerm::String(signal_type)) = factory.match_value_term(&signal_type) {
            let signal = allocator.create_signal(
                SignalType::Custom(String::from(signal_type.as_str())),
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