// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{
        uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
        FunctionArity, HeapAllocator, Uid, Uuid,
    },
    lang::ValueTerm,
};

pub struct Cons {}
impl Cons {
    const UUID: Uuid = uuid!("6dafb566-229c-441d-85e4-951a9e2b5a60");
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Lazy, ArgType::Lazy],
        optional: [],
        variadic: None,
    };
}
impl Uid for Cons {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Cons {
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