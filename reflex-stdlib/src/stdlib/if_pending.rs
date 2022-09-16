// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{
    uuid, Applicable, ArgType, Arity, ConditionListType, ConditionType, EvaluationCache,
    Expression, ExpressionFactory, FunctionArity, HeapAllocator, RefType, SignalTermType,
    SignalType, Uid, Uuid,
};

pub struct IfPending {}
impl IfPending {
    pub(crate) const UUID: Uuid = uuid!("ae41033f-ae13-4e46-810b-1a90d62aa306");
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Eager, ArgType::Lazy],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for IfPending {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for IfPending {
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
        let fallback = args.next().unwrap();
        if let Some(signal) = factory.match_signal_term(&target) {
            let non_pending_signals = signal
                .signals()
                .as_deref()
                .iter()
                .filter(|signal| signal.as_deref().signal_type() != SignalType::Pending)
                .collect::<Vec<_>>();
            if non_pending_signals.len() == signal.signals().as_deref().len() {
                Ok(target.clone())
            } else if !non_pending_signals.is_empty() {
                Ok(factory.create_signal_term(
                    allocator.create_signal_list(
                        non_pending_signals
                            .into_iter()
                            .map(|signal| allocator.clone_signal(signal)),
                    ),
                ))
            } else {
                Ok(fallback)
            }
        } else {
            Ok(target)
        }
    }
}
