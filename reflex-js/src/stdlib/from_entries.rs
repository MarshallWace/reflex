// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    ExpressionListType, FunctionArity, HeapAllocator, ListTermType, RefType, Uid, Uuid,
};
use reflex_stdlib::{CollectList, ConstructRecord, Get};

pub struct FromEntries;
impl FromEntries {
    pub const UUID: Uuid = uuid!("0dff2d00-33c8-49cd-b350-423bd9389645");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for FromEntries {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for FromEntries
where
    T::Builtin: From<CollectList> + From<ConstructRecord> + From<Get>,
{
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
        if let Some(_) = factory.match_record_term(&target) {
            Ok(target)
        } else if let Some(entries) = factory.match_list_term(&target) {
            let entries = entries
                .items()
                .as_deref()
                .iter()
                .map(|entry| {
                    let entry = entry.as_deref();
                    match get_indexed_field(entry, 0, factory, allocator).and_then(|key| {
                        get_indexed_field(entry, 1, factory, allocator).map(|value| (key, value))
                    }) {
                        Some((key, value)) => Ok((key, value)),
                        None => Err(format!(
                            "Object.fromEntries(): Expected [key, value] pair, received {}",
                            entry,
                        )),
                    }
                })
                .collect::<Result<Vec<_>, _>>();
            match entries {
                Err(error) => Err(error),
                Ok(entries) => {
                    let (keys, values): (Vec<_>, Vec<_>) = entries.into_iter().unzip();
                    let is_static_prototype = keys.iter().all(|key| key.is_static());
                    Ok(if is_static_prototype {
                        let prototype =
                            allocator.create_struct_prototype(allocator.create_list(keys));
                        factory.create_record_term(prototype, allocator.create_list(values))
                    } else {
                        factory.create_application_term(
                            factory.create_builtin_term(ConstructRecord),
                            allocator.create_pair(
                                factory.create_application_term(
                                    factory.create_builtin_term(CollectList),
                                    allocator.create_list(keys),
                                ),
                                factory.create_list_term(allocator.create_list(values)),
                            ),
                        )
                    })
                }
            }
        } else {
            Err(format!(
                "Object.fromEntries(): Expected list of [key, value] pairs, received {}",
                target
            ))
        }
    }
}

fn get_indexed_field<T: Expression>(
    target: &T,
    index: usize,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Option<T>
where
    T::Builtin: From<Get>,
{
    if let Some(target) = factory.match_list_term(target) {
        target
            .items()
            .as_deref()
            .get(index)
            .map(|value| value.as_deref().clone())
    } else if target.is_static() {
        None
    } else {
        Some(factory.create_application_term(
            factory.create_builtin_term(Get),
            allocator.create_pair(target.clone(), factory.create_int_term(index as i32)),
        ))
    }
}
