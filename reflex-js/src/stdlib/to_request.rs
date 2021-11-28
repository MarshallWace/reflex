// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use reflex::{
    core::{
        uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
        FunctionArity, HeapAllocator, StructPrototype, Uid, Uuid,
    },
    lang::ValueTerm,
};

pub struct ToRequest {}
impl ToRequest {
    pub(crate) const UUID: Uuid = uuid!("29d89369-0b7b-41df-aa14-47ea708a8fa6");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
}
impl Uid for ToRequest {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for ToRequest {
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
        let target = args.next().unwrap();
        let result = match factory.match_value_term(&target) {
            Some(ValueTerm::String(_)) => {
                let url = target.clone();
                let method = factory
                    .create_value_term(ValueTerm::String(allocator.create_static_string("GET")));
                let headers = factory.create_struct_term(
                    allocator.create_struct_prototype(Vec::new()),
                    allocator.create_empty_list(),
                );
                let body = factory.create_value_term(ValueTerm::Null);
                Some(factory.create_struct_term(
                    http_request_prototype(allocator),
                    allocator.create_list(vec![url, method, headers, body]),
                ))
            }
            _ => http_request_prototype(allocator).parse_struct(&target, factory, allocator),
        };
        match result {
            Some(result) => Ok(result),
            None => Err(format!(
                "Invalid Request constructor init object: {}",
                target
            )),
        }
    }
}

pub(crate) fn http_request_prototype<T: Expression>(
    allocator: &impl HeapAllocator<T>,
) -> StructPrototype {
    allocator.create_struct_prototype(vec![
        String::from("url"),
        String::from("method"),
        String::from("headers"),
        String::from("body"),
    ])
}
