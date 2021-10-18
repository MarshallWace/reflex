// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::core::{
    Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory, FunctionArity,
    HeapAllocator, SignalType,
};

pub struct IfPending {}
impl IfPending {
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Eager, ArgType::Lazy],
        optional: [],
        variadic: None,
    };
}
impl<T: Expression> Applicable<T> for IfPending {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(&Self::ARITY))
    }
    fn apply(
        &self,
        args: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
        let target = args.next().unwrap();
        let fallback = args.next().unwrap();
        if let Some(signal) = factory.match_signal_term(&target) {
            let non_pending_signals = signal
                .signals()
                .into_iter()
                .filter(|signal| *signal.signal_type() != SignalType::Pending)
                .collect::<Vec<_>>();
            if non_pending_signals.len() == signal.signals().len() {
                Ok(target)
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
