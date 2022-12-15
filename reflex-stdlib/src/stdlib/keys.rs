// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    ExpressionListType, FunctionArity, HashmapTermType, HeapAllocator, ListTermType,
    RecordTermType, RefType, StructPrototypeType, Uid, Uuid,
};

pub struct Keys;
impl Keys {
    pub const UUID: Uuid = uuid!("05812cb5-1a05-49c0-be1a-cc715f620a77");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Keys {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Keys {
    fn arity(&self) -> Option<Arity> {
        Some(Self::arity())
    }
    fn should_parallelize(&self, _args: &[T]) -> bool {
        false
    }
    fn apply(
        &self,
        mut args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let target = args.next().unwrap();
        let result = if let Some(target) = factory.match_record_term(&target) {
            Some(
                factory.create_list_term(
                    allocator.create_list(
                        target
                            .prototype()
                            .as_deref()
                            .keys()
                            .as_deref()
                            .iter()
                            .map(|item| item.as_deref())
                            .cloned(),
                    ),
                ),
            )
        } else if let Some(target) = factory.match_list_term(&target) {
            Some(
                factory.create_list_term(
                    allocator.create_list(
                        target
                            .items()
                            .as_deref()
                            .iter()
                            .map(|item| item.as_deref())
                            .enumerate()
                            .map(|(index, _)| factory.create_int_term(index as i32)),
                    ),
                ),
            )
        } else if let Some(target) = factory.match_hashmap_term(&target) {
            Some(factory.create_list_term(
                allocator.create_list(target.keys().map(|item| item.as_deref()).cloned()),
            ))
        } else {
            None
        };
        match result {
            Some(result) => Ok(result),
            None => Err(format!("Unable to enumerate keys for {}", target)),
        }
    }
}
