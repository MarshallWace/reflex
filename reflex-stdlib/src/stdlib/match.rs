// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    ExpressionListType, FunctionArity, HeapAllocator, IntTermType, ListTermType, RefType, Uid,
    Uuid,
};

pub struct Match {}
impl Match {
    pub(crate) const UUID: Uuid = uuid!("5ce9c4e5-68ff-49c8-acb7-83d9213ca3fd");
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Match {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Match {
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
        let matcher = args.next().unwrap();
        let result = match (
            match_enum(&target, factory),
            factory.match_list_term(&matcher),
        ) {
            (Some((discriminant, enum_args)), Some(matcher_term)) => Some({
                match matcher_term
                    .items()
                    .as_deref()
                    .get(discriminant)
                    .map(|item| item.as_deref())
                {
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
    usize,
    impl IntoIterator<Item = &'a T, IntoIter = impl ExactSizeIterator<Item = &'a T>>,
)> {
    match factory.match_list_term(target) {
        Some(target) => target
            .items()
            .as_deref()
            .get(0)
            .map(|item| item.as_deref())
            .and_then(|discriminant| match factory.match_int_term(discriminant) {
                Some(term) if term.value() >= 0 => Some((
                    term.value() as usize,
                    target
                        .items()
                        .as_deref()
                        .iter()
                        .map(|item| item.as_deref())
                        .skip(1),
                )),
                _ => None,
            }),
        _ => None,
    }
}
