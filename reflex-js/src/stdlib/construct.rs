// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    FunctionArity, HeapAllocator, Uid, Uuid,
};

pub struct Construct {}
impl Construct {
    const UUID: Uuid = uuid!("5a68dc6b-71fa-407f-8039-bcd87323f2bf");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: Some(ArgType::Lazy),
    };
}
impl Uid for Construct {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Construct {
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
        let target = args.next().unwrap();
        if let Some(constructor) = factory.match_constructor_term(&target) {
            let prototype = constructor.prototype();
            let properties = args.next().unwrap();
            prototype
                .parse_struct(&properties, factory, allocator)
                .ok_or_else(|| format!("Invalid constructor call: {} {}", constructor, properties))
        } else {
            Ok(factory.create_application_term(target, allocator.create_list(args)))
        }
    }
}