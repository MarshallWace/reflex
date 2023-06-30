// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::ops::Deref;

use reflex::core::{
    as_integer, uuid, Applicable, ArgType, Arity, Builtin, EvaluationCache, Expression,
    ExpressionFactory, ExpressionListType, FloatTermType, FunctionArity, HeapAllocator,
    IntTermType, ListTermType, RecordTermType, RefType, StringTermType, StringValue, Uid, Uuid,
};

use reflex_stdlib::stdlib;

pub trait AccessorBuiltin:
    Builtin
    + From<stdlib::Apply>
    + From<stdlib::Concat>
    + From<stdlib::Contains>
    + From<stdlib::Filter>
    + From<stdlib::Flatten>
    + From<stdlib::Get>
    + From<stdlib::Insert>
    + From<stdlib::Keys>
    + From<stdlib::Length>
    + From<stdlib::Map>
    + From<stdlib::Multiply>
    + From<stdlib::Push>
    + From<stdlib::PushFront>
    + From<stdlib::Reduce>
    + From<stdlib::Replace>
    + From<stdlib::ResolveList>
    + From<stdlib::Slice>
    + From<stdlib::Split>
    + From<stdlib::Subtract>
    + From<crate::stdlib::ToString>
    + From<stdlib::Values>
    + From<stdlib::Zip>
{
}
impl<T> AccessorBuiltin for T where
    T: Builtin
        + From<stdlib::Apply>
        + From<stdlib::Concat>
        + From<stdlib::Contains>
        + From<stdlib::Filter>
        + From<stdlib::Flatten>
        + From<stdlib::Get>
        + From<stdlib::Insert>
        + From<stdlib::Keys>
        + From<stdlib::Length>
        + From<stdlib::Map>
        + From<stdlib::Multiply>
        + From<stdlib::Push>
        + From<stdlib::PushFront>
        + From<stdlib::Reduce>
        + From<stdlib::Replace>
        + From<stdlib::ResolveList>
        + From<stdlib::Slice>
        + From<stdlib::Split>
        + From<stdlib::Subtract>
        + From<crate::stdlib::ToString>
        + From<stdlib::Values>
        + From<stdlib::Zip>
{
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct Accessor;

impl Accessor {
    pub const UUID: Uuid = uuid!("c6ea1ed6-9361-4ba7-813e-1bbe09331f8d");
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Accessor {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Accessor
where
    T::Builtin: AccessorBuiltin,
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
        let key = args.next().unwrap();
        let result = if let Some(term) = factory.match_record_term(&target) {
            get_record_property(term, &target, &key, factory, allocator)
        } else if let Some(term) = factory.match_list_term(&target) {
            get_list_property(term, &target, &key, factory, allocator)
        } else if let Some(term) = factory.match_string_term(&target) {
            get_string_property(term, &target, &key, factory, allocator)
        } else if let Some(term) = factory.match_hashmap_term(&target) {
            get_hashmap_property(term, &target, &key, factory, allocator)
        } else if let Some(term) = factory.match_hashset_term(&target) {
            get_hashset_property(term, &target, &key, factory, allocator)
        } else {
            None
        };
        match result {
            Some(result) => Ok(result),
            _ => Err(format!("Invalid field accessor: {}[{}]", target, key)),
        }
    }
}

fn get_record_property<T: Expression, TFactory: ExpressionFactory<T>>(
    term: &T::RecordTerm,
    _target: &T,
    key: &T,
    _factory: &TFactory,
    _allocator: &impl HeapAllocator<T>,
) -> Option<T> {
    term.get(key).map(|value| value.as_deref().clone())
}

fn get_list_property<T: Expression, TFactory: ExpressionFactory<T>>(
    term: &T::ListTerm,
    target: &T,
    key: &T,
    factory: &TFactory,
    allocator: &impl HeapAllocator<T>,
) -> Option<T>
where
    T::Builtin: Builtin
        + From<stdlib::Apply>
        + From<stdlib::Concat>
        + From<stdlib::Filter>
        + From<stdlib::Flatten>
        + From<stdlib::Get>
        + From<stdlib::Keys>
        + From<stdlib::Length>
        + From<stdlib::Map>
        + From<stdlib::Multiply>
        + From<stdlib::Push>
        + From<stdlib::PushFront>
        + From<stdlib::Reduce>
        + From<stdlib::ResolveList>
        + From<stdlib::Slice>
        + From<stdlib::Subtract>
        + From<crate::stdlib::ToString>
        + From<stdlib::Values>,
{
    if let Some(key) = factory.match_int_term(key) {
        let index = key.value();
        if index >= 0 {
            term.items()
                .as_deref()
                .get(index as usize)
                .map(|item| item.as_deref().clone())
        } else {
            None
        }
    } else if let Some(key) = factory.match_float_term(key) {
        let index = as_integer(key.value());
        match index {
            Some(index) if index >= 0 => term
                .items()
                .as_deref()
                .get(index as usize)
                .map(|item| item.as_deref().clone()),
            _ => None,
        }
    } else if let Some(key) = factory.match_string_term(key) {
        let key = key.value();
        let key = key.as_deref().as_str();
        let key = key.deref();
        get_list_field(target, key, factory, allocator)
    } else {
        None
    }
}

fn get_string_property<T: Expression, TFactory: ExpressionFactory<T>>(
    term: &T::StringTerm,
    target: &T,
    key: &T,
    factory: &TFactory,
    allocator: &impl HeapAllocator<T>,
) -> Option<T>
where
    T::Builtin:
        From<stdlib::Length> + From<stdlib::Replace> + From<stdlib::Slice> + From<stdlib::Split>,
{
    if let Some(key) = factory.match_int_term(key) {
        let index = key.value();
        if index >= 0 {
            let value = get_string_char(term.value().as_deref().as_str().deref(), index as usize);
            Some(factory.create_string_term(allocator.create_string(value)))
        } else {
            None
        }
    } else if let Some(key) = factory.match_float_term(key) {
        let index = as_integer(key.value());
        match index {
            Some(index) if index >= 0 => {
                let value =
                    get_string_char(term.value().as_deref().as_str().deref(), index as usize);
                Some(factory.create_string_term(allocator.create_string(value)))
            }
            _ => None,
        }
    } else if let Some(key) = factory.match_string_term(key) {
        let key = key.value();
        let key = key.as_deref().as_str();
        let key = key.deref();
        get_string_field(target, key, factory, allocator)
    } else {
        None
    }
}

fn get_hashmap_property<T: Expression, TFactory: ExpressionFactory<T>>(
    _term: &T::HashmapTerm,
    target: &T,
    key: &T,
    factory: &TFactory,
    allocator: &impl HeapAllocator<T>,
) -> Option<T>
where
    T::Builtin: From<stdlib::Get>
        + From<stdlib::Contains>
        + From<stdlib::Keys>
        + From<stdlib::Insert>
        + From<stdlib::Length>
        + From<stdlib::Values>
        + From<stdlib::Zip>,
{
    if let Some(key) = factory.match_string_term(key) {
        let key = key.value();
        let key = key.as_deref().as_str();
        let key = key.deref();
        get_hashmap_field(target, key, factory, allocator)
    } else {
        None
    }
}

fn get_hashset_property<T: Expression, TFactory: ExpressionFactory<T>>(
    _term: &T::HashsetTerm,
    target: &T,
    key: &T,
    factory: &TFactory,
    allocator: &impl HeapAllocator<T>,
) -> Option<T>
where
    T::Builtin: From<stdlib::Contains>
        + From<stdlib::Length>
        + From<stdlib::Push>
        + From<stdlib::Values>
        + From<stdlib::Zip>,
{
    if let Some(key) = factory.match_string_term(key) {
        let key = key.value();
        let key = key.as_deref().as_str();
        let key = key.deref();
        get_hashset_field(target, key, factory, allocator)
    } else {
        None
    }
}

fn get_list_field<T: Expression, TFactory: ExpressionFactory<T>>(
    target: &T,
    key: &str,
    factory: &TFactory,
    allocator: &impl HeapAllocator<T>,
) -> Option<T>
where
    T::Builtin: Builtin
        + From<stdlib::Apply>
        + From<stdlib::Concat>
        + From<stdlib::Filter>
        + From<stdlib::Flatten>
        + From<stdlib::Get>
        + From<stdlib::Keys>
        + From<stdlib::Length>
        + From<stdlib::Map>
        + From<stdlib::Multiply>
        + From<stdlib::Push>
        + From<stdlib::PushFront>
        + From<stdlib::Reduce>
        + From<stdlib::ResolveList>
        + From<stdlib::Slice>
        + From<stdlib::Subtract>
        + From<crate::stdlib::ToString>
        + From<stdlib::Values>,
{
    match key {
        "entries" => Some(factory.create_partial_application_term(
            factory.create_builtin_term(stdlib::Values),
            allocator.create_unit_list(target.clone()),
        )),
        "filter" => Some(factory.create_partial_application_term(
            factory.create_builtin_term(stdlib::Filter),
            allocator.create_unit_list(target.clone()),
        )),
        // TODO: Create stdlib method for Array.prototype.join
        "join" => Some(get_array_join_method(target, factory, allocator)),
        "keys" => Some(factory.create_partial_application_term(
            factory.create_builtin_term(stdlib::Keys),
            allocator.create_unit_list(target.clone()),
        )),
        "length" => Some(factory.create_application_term(
            factory.create_builtin_term(stdlib::Length),
            allocator.create_unit_list(target.clone()),
        )),
        "map" => Some(factory.create_partial_application_term(
            factory.create_builtin_term(stdlib::Map),
            allocator.create_unit_list(target.clone()),
        )),
        "push" => Some(factory.create_partial_application_term(
            factory.create_builtin_term(stdlib::Push),
            allocator.create_unit_list(target.clone()),
        )),
        "reduce" => Some(factory.create_partial_application_term(
            factory.create_builtin_term(stdlib::Reduce),
            allocator.create_unit_list(target.clone()),
        )),
        "slice" => Some(factory.create_partial_application_term(
            factory.create_builtin_term(stdlib::Slice),
            allocator.create_unit_list(target.clone()),
        )),
        "unshift" => Some(factory.create_partial_application_term(
            factory.create_builtin_term(stdlib::PushFront),
            allocator.create_unit_list(target.clone()),
        )),
        "values" => Some(factory.create_partial_application_term(
            factory.create_builtin_term(stdlib::Values),
            allocator.create_unit_list(target.clone()),
        )),
        _ => None,
    }
}

fn get_array_join_method<T: Expression, TFactory: ExpressionFactory<T>>(
    target: &T,
    factory: &TFactory,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: Builtin
        + From<stdlib::Apply>
        + From<stdlib::Concat>
        + From<stdlib::Flatten>
        + From<stdlib::Length>
        + From<stdlib::Map>
        + From<stdlib::Multiply>
        + From<stdlib::ResolveList>
        + From<stdlib::Slice>
        + From<stdlib::Subtract>
        + From<crate::stdlib::ToString>,
{
    factory.create_partial_application_term(
        factory.create_lambda_term(
            2,
            factory.create_application_term(
                factory.create_builtin_term(stdlib::Apply),
                allocator.create_pair(
                    factory.create_builtin_term(stdlib::Concat),
                    factory.create_application_term(
                        factory.create_builtin_term(stdlib::Slice),
                        allocator.create_triple(
                            factory.create_application_term(
                                factory.create_builtin_term(stdlib::Flatten),
                                allocator.create_unit_list(factory.create_application_term(
                                    factory.create_builtin_term(stdlib::ResolveList),
                                    allocator.create_unit_list(factory.create_application_term(
                                        factory.create_builtin_term(stdlib::Map),
                                        allocator.create_pair(
                                            factory.create_variable_term(1),
                                            factory.create_partial_application_term(
                                                factory.create_lambda_term(
                                                    2,
                                                    factory.create_list_term(
                                                        allocator.create_pair(
                                                            factory.create_application_term(
                                                                factory.create_builtin_term(
                                                                    crate::stdlib::ToString,
                                                                ),
                                                                allocator.create_unit_list(
                                                                    factory.create_variable_term(0),
                                                                ),
                                                            ),
                                                            factory.create_variable_term(1),
                                                        ),
                                                    ),
                                                ),
                                                allocator.create_unit_list(
                                                    factory.create_variable_term(0),
                                                ),
                                            ),
                                        ),
                                    )),
                                )),
                            ),
                            factory.create_int_term(0),
                            factory.create_application_term(
                                factory.create_builtin_term(stdlib::Subtract),
                                allocator.create_pair(
                                    factory.create_application_term(
                                        factory.create_builtin_term(stdlib::Multiply),
                                        allocator.create_pair(
                                            factory.create_application_term(
                                                factory.create_builtin_term(stdlib::Length),
                                                allocator.create_pair(
                                                    factory.create_variable_term(1),
                                                    factory.create_int_term(2),
                                                ),
                                            ),
                                            factory.create_int_term(2),
                                        ),
                                    ),
                                    factory.create_int_term(1),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        ),
        allocator.create_unit_list(target.clone()),
    )
}

fn get_string_field<T: Expression, TFactory: ExpressionFactory<T>>(
    target: &T,
    method: &str,
    factory: &TFactory,
    allocator: &impl HeapAllocator<T>,
) -> Option<T>
where
    T::Builtin:
        From<stdlib::Length> + From<stdlib::Replace> + From<stdlib::Slice> + From<stdlib::Split>,
{
    match method {
        "length" => Some(factory.create_application_term(
            factory.create_builtin_term(stdlib::Length),
            allocator.create_unit_list(target.clone()),
        )),
        "replace" => Some(factory.create_partial_application_term(
            factory.create_builtin_term(stdlib::Replace),
            allocator.create_unit_list(target.clone()),
        )),
        "slice" => Some(factory.create_partial_application_term(
            factory.create_builtin_term(stdlib::Slice),
            allocator.create_unit_list(target.clone()),
        )),
        "split" => Some(factory.create_partial_application_term(
            factory.create_builtin_term(stdlib::Split),
            allocator.create_unit_list(target.clone()),
        )),
        _ => None,
    }
}

fn get_hashmap_field<T: Expression, TFactory: ExpressionFactory<T>>(
    target: &T,
    key: &str,
    factory: &TFactory,
    allocator: &impl HeapAllocator<T>,
) -> Option<T>
where
    T::Builtin: From<stdlib::Contains>
        + From<stdlib::Get>
        + From<stdlib::Keys>
        + From<stdlib::Insert>
        + From<stdlib::Length>
        + From<stdlib::Values>
        + From<stdlib::Zip>,
{
    match key {
        "entries" => Some(factory.create_partial_application_term(
            factory.create_lambda_term(
                1,
                factory.create_application_term(
                    factory.create_builtin_term(stdlib::Zip),
                    allocator.create_pair(
                        factory.create_application_term(
                            factory.create_builtin_term(stdlib::Keys),
                            allocator.create_unit_list(factory.create_variable_term(0)),
                        ),
                        factory.create_application_term(
                            factory.create_builtin_term(stdlib::Values),
                            allocator.create_unit_list(factory.create_variable_term(0)),
                        ),
                    ),
                ),
            ),
            allocator.create_unit_list(target.clone()),
        )),
        "get" => Some(factory.create_partial_application_term(
            factory.create_builtin_term(stdlib::Get),
            allocator.create_unit_list(target.clone()),
        )),
        "has" => Some(factory.create_partial_application_term(
            factory.create_builtin_term(stdlib::Contains),
            allocator.create_unit_list(target.clone()),
        )),
        "keys" => Some(factory.create_partial_application_term(
            factory.create_builtin_term(stdlib::Keys),
            allocator.create_unit_list(target.clone()),
        )),
        "set" => Some(factory.create_partial_application_term(
            factory.create_builtin_term(stdlib::Insert),
            allocator.create_unit_list(target.clone()),
        )),
        "size" => Some(factory.create_partial_application_term(
            factory.create_builtin_term(stdlib::Length),
            allocator.create_unit_list(target.clone()),
        )),
        "values" => Some(factory.create_partial_application_term(
            factory.create_builtin_term(stdlib::Values),
            allocator.create_unit_list(target.clone()),
        )),
        _ => None,
    }
}

fn get_hashset_field<T: Expression, TFactory: ExpressionFactory<T>>(
    target: &T,
    key: &str,
    factory: &TFactory,
    allocator: &impl HeapAllocator<T>,
) -> Option<T>
where
    T::Builtin: From<stdlib::Contains>
        + From<stdlib::Length>
        + From<stdlib::Push>
        + From<stdlib::Values>
        + From<stdlib::Zip>,
{
    match key {
        "add" => Some(factory.create_partial_application_term(
            factory.create_builtin_term(stdlib::Push),
            allocator.create_unit_list(target.clone()),
        )),
        "entries" => Some(factory.create_partial_application_term(
            factory.create_lambda_term(
                1,
                factory.create_application_term(
                    factory.create_builtin_term(stdlib::Zip),
                    allocator.create_pair(
                        factory.create_variable_term(0),
                        factory.create_variable_term(0),
                    ),
                ),
            ),
            allocator.create_unit_list(factory.create_application_term(
                factory.create_builtin_term(stdlib::Values),
                allocator.create_unit_list(target.clone()),
            )),
        )),
        "has" => Some(factory.create_partial_application_term(
            factory.create_builtin_term(stdlib::Contains),
            allocator.create_unit_list(target.clone()),
        )),
        "size" => Some(factory.create_partial_application_term(
            factory.create_builtin_term(stdlib::Length),
            allocator.create_unit_list(target.clone()),
        )),
        "values" => Some(factory.create_partial_application_term(
            factory.create_builtin_term(stdlib::Values),
            allocator.create_unit_list(target.clone()),
        )),
        _ => None,
    }
}

fn get_string_char(value: &str, index: usize) -> String {
    value
        .as_bytes()
        .get(index as usize)
        .copied()
        .map(char::from)
        .map(String::from)
        .unwrap_or_default()
}
