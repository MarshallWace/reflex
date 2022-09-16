// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use chrono::{DateTime, NaiveDateTime};
use reflex::core::{
    create_record, uuid, Applicable, ArgType, Arity, EvaluationCache, Expression,
    ExpressionFactory, FloatTermType, FunctionArity, HeapAllocator, IntTermType, RefType,
    StringTermType, StringValue, Uid, Uuid,
};
use reflex_stdlib::Stdlib;

pub struct DateConstructor {}
impl DateConstructor {
    pub(crate) const UUID: Uuid = uuid!("c63f7c30-b28c-42dd-aabc-3a228cca40e2");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for DateConstructor {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for DateConstructor
where
    T::Builtin: From<Stdlib>,
{
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
        let value = args.next().unwrap();
        let timestamp = if let Some(term) = factory.match_int_term(&value) {
            Some(term.value() as i64)
        } else if let Some(term) = factory.match_float_term(&value) {
            Some(term.value().trunc() as i64)
        } else if let Some(term) = factory.match_string_term(&value) {
            parse_string_timestamp(term.value().as_deref().as_str())
        } else {
            None
        };
        if let Some(timestamp) = timestamp {
            Ok(create_record(
                [(
                    factory.create_string_term(allocator.create_static_string("getTime")),
                    factory.create_lambda_term(0, factory.create_float_term(timestamp as f64)),
                )],
                factory,
                allocator,
            ))
        } else {
            Err(format!(
                "Invalid Date constructor: Expected Int or Float or ISO-8601 String, received {}",
                value
            ))
        }
    }
}

fn parse_string_timestamp(timestamp: &str) -> Option<i64> {
    None.or_else(|| {
        DateTime::parse_from_rfc3339(timestamp)
            .ok()
            .map(|date| date.timestamp_millis())
    })
    .or_else(|| {
        NaiveDateTime::parse_from_str(timestamp, "%Y-%m-%dT%H:%M:%S%.f")
            .ok()
            .map(|date| date.timestamp_millis())
    })
}
