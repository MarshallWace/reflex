// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    FunctionArity, GraphNode, HashmapTermType, HashsetTermType, HeapAllocator, ListTermType,
    RecordTermType, Uid, Uuid,
};

use crate::Stdlib;

pub struct ResolveShallow {}
impl ResolveShallow {
    pub(crate) const UUID: Uuid = uuid!("475ca53b-e249-418d-8310-a9d54ae7ac0c");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for ResolveShallow {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for ResolveShallow
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
        mut args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let target = args.next().unwrap();
        if let Some(value) = factory.match_record_term(&target) {
            if value.values().is_atomic() {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(Stdlib::CollectRecord),
                    allocator.clone_list(value.values()),
                ))
            }
        } else if let Some(value) = factory.match_list_term(&target) {
            if value.items().is_atomic() {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(Stdlib::CollectList),
                    allocator.clone_list(value.items()),
                ))
            }
        } else if let Some(value) = factory.match_hashset_term(&target) {
            if value.values().all(|value| value.is_atomic()) {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(Stdlib::CollectHashSet),
                    allocator.create_list(value.values().cloned()),
                ))
            }
        } else if let Some(value) = factory.match_hashmap_term(&target) {
            let keys_are_atomic = value.keys().all(|key| key.is_atomic());
            let values_are_atomic = value.values().all(|value| value.is_atomic());
            if keys_are_atomic && values_are_atomic {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(Stdlib::ConstructHashMap),
                    allocator.create_pair(
                        if keys_are_atomic {
                            factory.create_list_term(allocator.create_list(value.keys().cloned()))
                        } else {
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::CollectList),
                                allocator.create_list(value.keys().cloned()),
                            )
                        },
                        if values_are_atomic {
                            factory.create_list_term(allocator.create_list(value.values().cloned()))
                        } else {
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::CollectList),
                                allocator.create_list(value.values().cloned()),
                            )
                        },
                    ),
                ))
            }
        } else {
            Ok(target)
        }
    }
}
