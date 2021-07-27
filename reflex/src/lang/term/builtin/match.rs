// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{
        Applicable, Arity, EvaluationCache, Expression, ExpressionFactory, HeapAllocator,
        StructFieldOffset,
    },
    lang::ValueTerm,
};

pub struct Match {}
impl<T: Expression> Applicable<T> for Match {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(2, 0, None))
    }
    fn apply(
        &self,
        args: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
        if args.len() != 2 {
            return Err(format!("Expected 2 arguments, received {}", args.len()));
        }
        let target = args.next().unwrap();
        let matcher = args.next().unwrap();
        let result = match (
            match_enum(&target, factory),
            factory.match_tuple_term(&matcher),
        ) {
            (Some((discriminant, enum_args)), Some(matcher)) => Some({
                match matcher.fields().get(discriminant) {
                    Some(handler) => Ok(factory.create_application_term(
                        handler.clone(),
                        allocator.create_list(enum_args.into_iter().cloned()),
                    )),
                    None => Err(format!(
                        "Unhandled enum index: {} for matcher {}",
                        discriminant, matcher
                    )),
                }
            }),
            _ => None,
        };
        match result {
            Some(result) => result,
            None => Err(format!(
                "Invalid pattern match: Expected (<enum>, <struct>), received ({}, {})",
                target, matcher,
            )),
        }
    }
}

pub(crate) fn match_enum<'a, T: Expression>(
    target: &'a T,
    factory: &'a impl ExpressionFactory<T>,
) -> Option<(
    StructFieldOffset,
    impl IntoIterator<Item = &'a T, IntoIter = impl ExactSizeIterator<Item = &'a T>>,
)> {
    match factory.match_tuple_term(target) {
        Some(target) => {
            target.get(0).and_then(
                |discriminant| match factory.match_value_term(discriminant) {
                    Some(ValueTerm::Int(value)) if *value >= 0 => {
                        Some((*value as usize, target.fields().iter().skip(1)))
                    }
                    _ => None,
                },
            )
        }
        _ => None,
    }
}
