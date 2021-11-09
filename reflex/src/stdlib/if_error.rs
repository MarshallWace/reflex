// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{
        uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
        FunctionArity, HeapAllocator, SignalType, Uid, Uuid,
    },
    lang::ValueTerm,
};

pub struct IfError {}
impl IfError {
    const UUID: Uuid = uuid!("ca015984-8f33-49c5-b821-aca8fb122ee8");
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Eager, ArgType::Lazy],
        optional: [],
        variadic: None,
    };
}
impl Uid for IfError {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for IfError {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(&Self::ARITY))
    }
    fn apply(
        &self,
        mut args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let target = args.next().unwrap();
        let handler = args.next().unwrap();
        if let Some(signal) = factory.match_signal_term(&target) {
            let (error_signals, other_signals) = signal.signals().into_iter().fold(
                (Vec::new(), Vec::new()),
                |(mut error_signals, mut other_signals), signal| {
                    if *signal.signal_type() == SignalType::Error {
                        error_signals.push(signal);
                    } else {
                        other_signals.push(signal);
                    }
                    (error_signals, other_signals)
                },
            );
            if error_signals.is_empty() {
                Ok(target)
            } else if !other_signals.is_empty() {
                Ok(factory.create_signal_term(
                    allocator.create_signal_list(
                        other_signals
                            .into_iter()
                            .map(|signal| allocator.clone_signal(signal)),
                    ),
                ))
            } else {
                Ok(factory.create_application_term(
                    handler,
                    allocator.create_unit_list(factory.create_vector_term(allocator.create_list(
                        error_signals.into_iter().map(|signal| {
                            signal
                                .args()
                                .iter()
                                .next()
                                .cloned()
                                .unwrap_or_else(|| factory.create_value_term(ValueTerm::Null))
                        }),
                    ))),
                ))
            }
        } else {
            Ok(target)
        }
    }
}