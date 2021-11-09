// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{
        uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
        FunctionArity, HeapAllocator, StructFieldOffset, Uid, Uuid,
    },
    lang::ValueTerm,
};

pub struct Match {}
impl Match {
    const UUID: Uuid = uuid!("5ce9c4e5-68ff-49c8-acb7-83d9213ca3fd");
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
}
impl Uid for Match {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Match {
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