// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    ExpressionListType, FunctionArity, HeapAllocator, IntTermType, ListTermType, RefType, Uid,
    Uuid,
};

pub struct Cons;
impl Cons {
    pub const UUID: Uuid = uuid!("6dafb566-229c-441d-85e4-951a9e2b5a60");
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Lazy, ArgType::Lazy],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Cons {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Cons {
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
        if args.len() != 2 {
            return Err(format!("Expected 2 arguments, received {}", args.len()));
        }
        let head = args.next().unwrap();
        let tail = args.next().unwrap();
        let enum_variant = factory.create_int_term(1);
        Ok(factory.create_list_term(allocator.create_triple(enum_variant, head, tail)))
    }
}

pub(crate) fn match_cons_cell<'a, T: Expression>(
    target: &'a T,
    factory: &'a impl ExpressionFactory<T>,
) -> Option<(&'a T, &'a T)> {
    match_triple(target, factory).and_then(|(enum_discriminant, head, tail)| {
        match factory.match_int_term(enum_discriminant) {
            Some(term) if term.value() == 1 => Some((head, tail)),
            _ => None,
        }
    })
}

fn match_triple<'a, T: Expression>(
    target: &'a T,
    factory: &'a impl ExpressionFactory<T>,
) -> Option<(&'a T, &'a T, &'a T)> {
    match factory.match_list_term(target) {
        Some(term) if term.items().as_deref().len() == 3 => {
            let mut values = term.items().as_deref().iter().map(|item| item.as_deref());
            let first = values.next().unwrap();
            let second = values.next().unwrap();
            let third = values.next().unwrap();
            Some((first, second, third))
        }
        _ => None,
    }
}
