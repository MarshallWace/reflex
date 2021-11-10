// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use reflex::{
    core::{
        uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
        FunctionArity, HeapAllocator, Uid, Uuid,
    },
    lang::create_struct,
};

pub struct GraphQlResolver {}
impl GraphQlResolver {
    const UUID: Uuid = uuid!("34f0156e-4a64-40f5-bd01-e297b9086ee7");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
}
impl Uid for GraphQlResolver {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for GraphQlResolver {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(&Self::ARITY))
    }
    fn apply(
        &self,
        args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
        let root = args.next().unwrap();
        if let Some((Some(query), Some(mutation), Some(subscription))) =
            factory.match_struct_term(&root).map(|value| {
                (
                    value.get("query"),
                    value.get("mutation"),
                    value.get("subscription"),
                )
            })
        {
            let query = parse_operation_root(query, "query", factory)?;
            let mutation = parse_operation_root(mutation, "mutation", factory)?;
            let subscription = parse_operation_root(subscription, "subscription", factory)?;
            Ok(create_struct(
                [
                    (String::from("query"), query),
                    (String::from("mutation"), mutation),
                    (String::from("subscription"), subscription),
                ],
                factory,
                allocator,
            ))
        } else {
            Err(format!("Invalid GraphQL resolver definition: Expected {{ query, subscription, mutation }}, received {}", root))
        }
    }
}

fn parse_operation_root<T: Expression>(
    definition: &T,
    operation_type: &str,
    factory: &impl ExpressionFactory<T>,
) -> Result<T, String> {
    let result = if let Some(constructor) = factory.match_lambda_term(definition) {
        match constructor.num_args() {
            0 => Some(factory.create_lambda_term(1, constructor.body().clone())),
            1 => Some(definition.clone()),
            _ => None,
        }
    } else if let Some(_) = factory.match_struct_term(definition) {
        Some(factory.create_lambda_term(1, definition.clone()))
    } else if let Some(_) = factory
        .match_value_term(definition)
        .and_then(|value| value.match_null())
    {
        Some(factory.create_lambda_term(1, definition.clone()))
    } else {
        None
    };
    result.ok_or_else(|| format!("Invalid GraphQL {} resolver definition: Expected <function:1> or <struct> or <null>, received {}", operation_type, definition))
}
