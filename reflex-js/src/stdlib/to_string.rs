// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::ops::Deref;

use reflex::core::{
    uuid, Applicable, ArgType, Arity, BooleanTermType, EvaluationCache, Expression,
    ExpressionFactory, FloatTermType, FunctionArity, HeapAllocator, IntTermType, RefType,
    StringTermType, StringValue, SymbolTermType, Uid, Uuid,
};

pub struct ToString;
impl ToString {
    pub const UUID: Uuid = uuid!("7f651286-8d00-4854-a956-0a54dfe662d0");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for ToString {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for ToString {
    fn arity(&self) -> Option<Arity> {
        Some(Self::arity())
    }
    fn should_parallelize(&self, _args: &[T]) -> bool {
        false
    }
    fn apply(
        &self,
        args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
        let operand = args.next().unwrap();
        if let Some(_) = factory.match_string_term(&operand) {
            Ok(operand)
        } else if let Some(value) = format_value(&operand, factory) {
            Ok(factory.create_string_term(allocator.create_string(value)))
        } else {
            Err(format!("Expected printable value, received {}", operand))
        }
    }
}

pub fn format_value<T: Expression>(
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Option<String> {
    if let Some(_) = factory.match_nil_term(value) {
        Some(String::from("null"))
    } else if let Some(term) = factory.match_boolean_term(value) {
        Some(format!("{}", term.value()))
    } else if let Some(term) = factory.match_int_term(value) {
        Some(format!("{}", term.value()))
    } else if let Some(term) = factory.match_float_term(value) {
        Some(format!("{}", term.value()))
    } else if let Some(term) = factory.match_string_term(value) {
        Some(String::from(term.value().as_deref().as_str().deref()))
    } else if let Some(term) = factory.match_symbol_term(value) {
        Some(format!("{}", term.id()))
    } else {
        None
    }
}
