// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{
    as_integer, uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    ExpressionListType, FloatTermType, FunctionArity, HeapAllocator, IntTermType, ListTermType,
    RefType, Uid, Uuid,
};

pub struct Slice;
impl Slice {
    pub const UUID: Uuid = uuid!("03f6c061-0058-4ead-b72f-baf79eba31f1");
    const ARITY: FunctionArity<3, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Slice {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Slice {
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
        let start_index = args.next().unwrap();
        let end_index = args.next().unwrap();
        let bounds = {
            let start_index = parse_integer_argument(&start_index, factory);
            let end_index = parse_integer_argument(&end_index, factory);
            match (start_index, end_index) {
                (Some(start_index), Some(end_index)) => {
                    let start_index = start_index.max(0) as usize;
                    let end_index = end_index.max(start_index as i32) as usize;
                    Some((start_index, end_index))
                }
                _ => None,
            }
        };
        match (factory.match_list_term(&target), bounds) {
            (Some(target), Some((start_index, end_index))) => Ok(factory.create_list_term(
                allocator.create_list(
                    target
                        .items()
                        .as_deref()
                        .iter()
                        .skip(start_index)
                        .take(end_index - start_index),
                ),
            )),
            _ => Err(format!(
                "Expected (List, Int, Int), received ({}, {}, {})",
                target, start_index, end_index,
            )),
        }
    }
}

fn parse_integer_argument<T: Expression>(
    term: &T,
    factory: &impl ExpressionFactory<T>,
) -> Option<i32> {
    match factory.match_int_term(term) {
        Some(term) => Some(term.value()),
        _ => match factory.match_float_term(term) {
            Some(term) => as_integer(term.value()),
            _ => None,
        },
    }
}
