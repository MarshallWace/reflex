// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use reflex::{
    core::{
        uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
        FunctionArity, HeapAllocator, StringValue, Uid, Uuid,
    },
    lang::ValueTerm,
};

pub struct ToString {}
impl ToString {
    pub(crate) const UUID: Uuid = uuid!("7f651286-8d00-4854-a956-0a54dfe662d0");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: Some(ArgType::Lazy),
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
        match factory.match_value_term(&operand) {
            Some(ValueTerm::String(_)) => Ok(operand),
            Some(value) => Ok(factory.create_value_term(ValueTerm::String(
                allocator.create_string(format_value(value)),
            ))),
            _ => Err(format!("Expected printable value, received {}", operand)),
        }
    }
}

pub fn format_value<TString: StringValue>(value: &ValueTerm<TString>) -> String {
    match value {
        ValueTerm::Null => String::from("null"),
        ValueTerm::Boolean(value) => format!("{}", value),
        ValueTerm::Int(value) => format!("{}", value),
        ValueTerm::Float(value) => format!("{}", value),
        ValueTerm::String(value) => String::from(value.as_str()),
        ValueTerm::Symbol(_) => format!("{}", value),
        ValueTerm::Hash(value) => format!("{:016x}", value),
    }
}
