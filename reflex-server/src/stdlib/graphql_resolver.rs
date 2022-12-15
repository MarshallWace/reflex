// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    FunctionArity, HeapAllocator, RecordTermType, Uid, Uuid,
};

pub struct GraphQlResolver;
impl GraphQlResolver {
    pub const UUID: Uuid = uuid!("34f0156e-4a64-40f5-bd01-e297b9086ee7");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for GraphQlResolver {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression + Applicable<T>> Applicable<T> for GraphQlResolver {
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
        let root = args.next().unwrap();
        if let Some(_) = factory.match_record_term(&root).filter(|value| {
            value
                .get(&factory.create_string_term(allocator.create_static_string("query")))
                .is_some()
                && value
                    .get(&factory.create_string_term(allocator.create_static_string("mutation")))
                    .is_some()
                && value
                    .get(
                        &factory.create_string_term(allocator.create_static_string("subscription")),
                    )
                    .is_some()
        }) {
            Ok(factory.create_lambda_term(1, root))
        } else if let Some(arity) = root.arity() {
            match arity.required().len() {
                0 => Ok(
                    if arity.optional().len() > 0 || arity.variadic().is_some() {
                        root
                    } else {
                        factory.create_lambda_term(1, root)
                    },
                ),
                1 => Ok(root),
                _ => Err(format!(
                    "Invalid GraphQL resolver factory: Expected <function:1>, received {}",
                    root
                )),
            }
        } else {
            Err(format!("Invalid GraphQL resolver definition: Expected {{ query, subscription, mutation }} or <function:1>, received {}", root))
        }
    }
}
