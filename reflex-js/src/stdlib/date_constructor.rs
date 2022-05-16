// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use chrono::{DateTime, NaiveDateTime};
use reflex::{
    core::{
        uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
        FunctionArity, HeapAllocator, StringValue, Uid, Uuid,
    },
    lang::{create_struct, ValueTerm},
    stdlib::Stdlib,
};

pub struct DateConstructor {}
impl DateConstructor {
    pub(crate) const UUID: Uuid = uuid!("c63f7c30-b28c-42dd-aabc-3a228cca40e2");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
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
        Some(Arity::from(&Self::ARITY))
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
        let timestamp = if let Some(value) = factory.match_value_term(&value) {
            match value {
                ValueTerm::Int(value) => Some(*value as i64),
                ValueTerm::Float(value) => Some(value.trunc() as i64),
                ValueTerm::String(value) => parse_string_timestamp(value.as_str()),
                _ => None,
            }
        } else {
            None
        };
        if let Some(timestamp) = timestamp {
            Ok(create_struct(
                [(
                    String::from("getTime"),
                    factory.create_lambda_term(
                        0,
                        factory.create_value_term(ValueTerm::Float(timestamp as f64)),
                    ),
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
