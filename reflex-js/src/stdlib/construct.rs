// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use reflex::core::{
    parse_record_values, uuid, Applicable, ArgType, Arity, ConstructorTermType, EvaluationCache,
    Expression, ExpressionFactory, FunctionArity, HeapAllocator, RefType, Uid, Uuid,
};

pub struct Construct {}
impl Construct {
    pub(crate) const UUID: Uuid = uuid!("5a68dc6b-71fa-407f-8039-bcd87323f2bf");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: Some(ArgType::Lazy),
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Construct {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Construct {
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
        let target = args.next().unwrap();
        let result = if let Some(constructor) = factory.match_constructor_term(&target) {
            let prototype = constructor.prototype();
            let properties = args.next().unwrap();
            let reordered_values =
                parse_record_values(prototype.as_deref(), &properties, factory, allocator)
                    .ok_or_else(|| {
                        format!("Invalid constructor call: {} {}", target, properties)
                    })?;
            match reordered_values {
                None => properties,
                Some(values) => {
                    factory.create_record_term(allocator.clone_struct_prototype(prototype), values)
                }
            }
        } else {
            factory.create_application_term(target, allocator.create_list(args))
        };
        Ok(result)
    }
}
