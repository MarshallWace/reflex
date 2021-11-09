// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    FunctionArity, HeapAllocator, Uid, Uuid,
};

pub struct StructTypeFactory {}
impl StructTypeFactory {
    const UUID: Uuid = uuid!("69fdfb4f-7a4a-4414-8140-ededbdc9368c");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
}
impl Uid for StructTypeFactory {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for StructTypeFactory {
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
        let shape = args.next().unwrap();
        if let Some(shape) = factory.match_struct_term(&shape) {
            Ok(
                factory
                    .create_constructor_term(allocator.clone_struct_prototype(shape.prototype())),
            )
        } else {
            Err(format!(
                "Invalid shape definition: Expected <struct>, received {}",
                shape
            ))
        }
    }
}