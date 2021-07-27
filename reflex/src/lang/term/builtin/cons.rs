// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{Applicable, Arity, EvaluationCache, Expression, ExpressionFactory, HeapAllocator},
    lang::ValueTerm,
};

pub struct Cons {}
impl<T: Expression> Applicable<T> for Cons {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(0, 2, None))
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
        let head = args.next().unwrap();
        let tail = args.next().unwrap();
        let enum_variant = factory.create_value_term(ValueTerm::Int(1));
        Ok(factory.create_tuple_term(allocator.create_triple(enum_variant, head, tail)))
    }
}

pub(crate) fn match_cons_cell<'a, T: Expression>(
    target: &'a T,
    factory: &'a impl ExpressionFactory<T>,
) -> Option<(&'a T, &'a T)> {
    match_triple(target, factory).and_then(|(enum_discriminant, head, tail)| {
        match factory.match_value_term(enum_discriminant) {
            Some(ValueTerm::Int(value)) if *value == 1 => Some((head, tail)),
            _ => None,
        }
    })
}

fn match_triple<'a, T: Expression>(
    target: &'a T,
    factory: &'a impl ExpressionFactory<T>,
) -> Option<(&'a T, &'a T, &'a T)> {
    match factory.match_tuple_term(target) {
        Some(target) if target.size() == 3 => {
            let mut fields = target.fields().iter();
            let first = fields.next().unwrap();
            let second = fields.next().unwrap();
            let third = fields.next().unwrap();
            Some((first, second, third))
        }
        _ => None,
    }
}
