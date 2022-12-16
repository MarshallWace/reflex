// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{
    uuid, Applicable, ArgType, Arity, ConditionListType, ConditionType, EvaluationCache,
    Expression, ExpressionFactory, FunctionArity, HeapAllocator, RefType, SignalTermType,
    SignalType, Uid, Uuid,
};

pub struct IfError;
impl IfError {
    pub const UUID: Uuid = uuid!("ca015984-8f33-49c5-b821-aca8fb122ee8");
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Eager, ArgType::Lazy],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for IfError {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for IfError {
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
        let target = args.next().unwrap();
        let handler = args.next().unwrap();
        if let Some(signal) = factory.match_signal_term(&target) {
            let (error_signals, other_signals) = signal.signals().as_deref().iter().fold(
                (Vec::new(), Vec::new()),
                |(mut error_signals, mut other_signals), signal| {
                    if signal.as_deref().signal_type() == SignalType::Error {
                        error_signals.push(signal.as_deref());
                    } else {
                        other_signals.push(signal);
                    }
                    (error_signals, other_signals)
                },
            );
            if error_signals.is_empty() {
                Ok(target.clone())
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
                    allocator.create_unit_list(
                        factory.create_list_term(
                            allocator.create_list(
                                error_signals
                                    .into_iter()
                                    .map(|signal| signal.payload())
                                    .map(|item| item.as_deref())
                                    .cloned(),
                            ),
                        ),
                    ),
                ))
            }
        } else {
            Ok(target)
        }
    }
}
