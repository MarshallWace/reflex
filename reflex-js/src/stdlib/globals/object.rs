// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::any::TypeId;

use reflex::{
    core::{
        Arity, Expression, ExpressionFactory, ExpressionList, HeapAllocator, NativeAllocator,
        StringValue,
    },
    hash::{hash_object, HashId},
    lang::{create_struct, BuiltinTerm, NativeFunction, ValueTerm},
};

pub fn global_object<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    create_struct(
        vec![(
            String::from("fromEntries"),
            factory.create_native_function_term(from_entries()),
        )],
        factory,
        allocator,
    )
}

pub fn from_entries<T: Expression>() -> NativeFunction<T> {
    NativeFunction::new(
        FromEntries::uid(),
        FromEntries::name(),
        FromEntries::arity(),
        FromEntries::apply,
    )
}
struct FromEntries {}
impl FromEntries {
    fn uid() -> HashId {
        hash_object(&TypeId::of::<Self>())
    }
    fn name() -> Option<&'static str> {
        Some("FromEntries")
    }
    fn arity() -> Arity {
        Arity::from(1, 0, None)
    }
    fn apply<T: Expression>(
        args: ExpressionList<T>,
        factory: &dyn ExpressionFactory<T>,
        allocator: &dyn NativeAllocator<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
        let target = args.next().unwrap();
        if let Some(_) = factory.match_struct_term(&target) {
            Ok(target)
        } else if let Some(entries) = factory.match_vector_term(&target) {
            let entries = entries
                .items()
                .iter()
                .map(|entry| {
                    match get_indexed_field(entry, 0, &factory, &allocator).and_then(|key| {
                        get_indexed_field(entry, 1, &factory, &allocator).map(|value| (key, value))
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
                    match get_static_prototype_keys(keys.iter(), &factory) {
                        Err(error) => Err(error),
                        Ok(Some(keys)) => {
                            let prototype = allocator.create_struct_prototype(
                                keys.into_iter()
                                    .map(|key| String::from(key.as_str()))
                                    .collect(),
                            );
                            Ok(
                                factory
                                    .create_struct_term(prototype, allocator.create_list(values)),
                            )
                        }
                        Ok(None) => Ok(factory.create_application_term(
                            factory.create_builtin_term(BuiltinTerm::ConstructStruct),
                            allocator.create_pair(
                                factory.create_application_term(
                                    factory.create_builtin_term(BuiltinTerm::CollectVector),
                                    allocator.create_list(keys),
                                ),
                                factory.create_vector_term(allocator.create_list(values)),
                            ),
                        )),
                    }
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
) -> Option<T> {
    if let Some(target) = factory.match_tuple_term(target) {
        target.get(index).cloned()
    } else if let Some(target) = factory.match_vector_term(target) {
        target.items().get(index).cloned()
    } else if target.is_static() {
        None
    } else {
        Some(factory.create_application_term(
            factory.create_builtin_term(BuiltinTerm::Get),
            allocator.create_pair(
                target.clone(),
                factory.create_value_term(ValueTerm::Int(index as i32)),
            ),
        ))
    }
}

fn get_static_prototype_keys<'a, T: Expression + 'a>(
    items: impl IntoIterator<Item = &'a T>,
    factory: &impl ExpressionFactory<T>,
) -> Result<Option<Vec<&'a T::String>>, String> {
    items
        .into_iter()
        .map(|item| match factory.match_value_term(item) {
            Some(ValueTerm::String(value)) => Ok(Some(value)),
            _ => {
                if !item.is_static() {
                    Ok(None)
                } else {
                    Err(format!(
                        "Invalid object key: Expected String, received {}",
                        item
                    ))
                }
            }
        })
        .collect::<Result<Option<Vec<_>>, _>>()
}
