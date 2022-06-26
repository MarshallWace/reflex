// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use crate::{
    core::{
        uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
        FunctionArity, GraphNode, HeapAllocator, Uid, Uuid,
    },
    stdlib::Stdlib,
};

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
        if let Some(value) = factory.match_tuple_term(&target) {
            if value.is_atomic() {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(Stdlib::CollectTuple),
                    allocator.clone_list(value.fields()),
                ))
            }
        } else if let Some(value) = factory.match_record_term(&target) {
            if value.is_atomic() {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(Stdlib::CollectRecord),
                    allocator.clone_list(value.values()),
                ))
            }
        } else if let Some(value) = factory.match_vector_term(&target) {
            if value.is_atomic() {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(Stdlib::CollectVector),
                    allocator.clone_list(value.items()),
                ))
            }
        } else if let Some(value) = factory.match_hashset_term(&target) {
            if value.is_atomic() {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(Stdlib::CollectHashSet),
                    allocator.clone_list(value.values()),
                ))
            }
        } else if let Some(value) = factory.match_hashmap_term(&target) {
            if value.is_atomic() {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(Stdlib::ConstructHashMap),
                    allocator.create_pair(
                        if value.keys().is_atomic() {
                            factory.create_vector_term(allocator.clone_list(value.keys()))
                        } else {
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::CollectVector),
                                allocator.clone_list(value.keys()),
                            )
                        },
                        if value.values().is_atomic() {
                            factory.create_vector_term(allocator.clone_list(value.values()))
                        } else {
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::CollectVector),
                                allocator.clone_list(value.values()),
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
