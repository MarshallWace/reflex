// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::iter::once;

use reflex::core::{
    uuid, Applicable, ArgType, Arity, Builtin, EvaluationCache, Expression, ExpressionFactory,
    FunctionArity, HeapAllocator, RefType, StringTermType, StringValue, Uid, Uuid,
};
use reflex_stdlib::{
    Contains, Entries, Filter, Flatten, Get, Insert, Keys, Map, Push, PushFront, Reduce, Replace,
    ResolveShallow, Sequence, Slice, Split, Values,
};

pub struct Dispatch;
impl Dispatch {
    pub const UUID: Uuid = uuid!("f37c66ee-cd73-496d-8420-1ce83ab924ad");
    const ARITY: FunctionArity<3, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict, ArgType::Lazy],
        optional: [],
        variadic: Some(ArgType::Lazy),
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Dispatch {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Dispatch
where
    T::Builtin: Builtin
        + From<Contains>
        + From<Entries>
        + From<Filter>
        + From<Flatten>
        + From<Get>
        + From<Insert>
        + From<Keys>
        + From<Map>
        + From<Push>
        + From<PushFront>
        + From<Reduce>
        + From<Replace>
        + From<ResolveShallow>
        + From<Sequence>
        + From<Slice>
        + From<Split>
        + From<Values>,
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
        let method_name = args.next().unwrap();
        let fallback = args.next().unwrap();
        let builtin_method = match factory.match_string_term(&method_name) {
            Some(method_name) => get_builtin_field(
                Some(&target),
                method_name.value().as_deref().as_str(),
                factory,
                allocator,
            ),
            _ => None,
        };
        match builtin_method {
            Some(method) => Ok(factory.create_application_term(
                method,
                allocator.create_sized_list(1 + args.len(), once(target).chain(args)),
            )),
            None => Ok(fallback),
        }
    }
}

pub(crate) fn get_builtin_field<T: Expression>(
    target: Option<&T>,
    method: &str,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Option<T>
where
    T::Builtin: Builtin
        + From<Contains>
        + From<Entries>
        + From<Filter>
        + From<Flatten>
        + From<Get>
        + From<Insert>
        + From<Keys>
        + From<Map>
        + From<Push>
        + From<PushFront>
        + From<Reduce>
        + From<Replace>
        + From<ResolveShallow>
        + From<Sequence>
        + From<Slice>
        + From<Split>
        + From<Values>,
{
    None.or_else(|| {
        if target.is_none()
            || target
                .and_then(|target| factory.match_string_term(target))
                .is_some()
        {
            get_builtin_string_field(method, factory)
        } else {
            None
        }
    })
    .or_else(|| {
        if target.is_none()
            || target
                .map(|target| factory.match_list_term(target))
                .is_some()
        {
            get_builtin_list_field(method, factory, allocator)
        } else {
            None
        }
    })
    .or_else(|| {
        if target.is_none()
            || target
                .map(|target| factory.match_hashmap_term(target))
                .is_some()
        {
            get_builtin_hashmap_field(method, factory, allocator)
        } else {
            None
        }
    })
    .or_else(|| {
        if target.is_none()
            || target
                .map(|target| factory.match_hashset_term(target))
                .is_some()
        {
            get_builtin_hashset_field(method, factory)
        } else {
            None
        }
    })
}

fn get_builtin_string_field<T: Expression>(
    method: &str,
    factory: &impl ExpressionFactory<T>,
) -> Option<T>
where
    T::Builtin: From<Replace> + From<Split>,
{
    match method {
        "replace" => Some(factory.create_builtin_term(Replace)),
        "split" => Some(factory.create_builtin_term(Split)),
        _ => None,
    }
}

fn get_builtin_list_field<T: Expression>(
    method: &str,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Option<T>
where
    T::Builtin: Builtin
        + From<Entries>
        + From<Filter>
        + From<Keys>
        + From<Map>
        + From<Flatten>
        + From<ResolveShallow>
        + From<Push>
        + From<Reduce>
        + From<Slice>
        + From<PushFront>
        + From<Values>,
{
    match method {
        "entries" => Some(factory.create_builtin_term(Entries)),
        "filter" => Some(factory.create_builtin_term(Filter)),
        "keys" => Some(factory.create_builtin_term(Keys)),
        "map" => Some(factory.create_builtin_term(Map)),
        "flatMap" => Some(factory.create_lambda_term(
            2,
            factory.create_application_term(
                factory.create_builtin_term(Flatten),
                allocator.create_unit_list(factory.create_application_term(
                    factory.create_builtin_term(ResolveShallow),
                    allocator.create_unit_list(factory.create_application_term(
                        factory.create_builtin_term(Map),
                        allocator.create_pair(
                            factory.create_variable_term(1),
                            factory.create_variable_term(0),
                        ),
                    )),
                )),
            ),
        )),
        "push" => Some(factory.create_builtin_term(Push)),
        "reduce" => Some(factory.create_builtin_term(Reduce)),
        "slice" => Some(factory.create_builtin_term(Slice)),
        "unshift" => Some(factory.create_builtin_term(PushFront)),
        "values" => Some(factory.create_builtin_term(Values)),
        _ => None,
    }
}

fn get_builtin_hashmap_field<T: Expression>(
    method: &str,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Option<T>
where
    T::Builtin: Builtin
        + From<Contains>
        + From<Entries>
        + From<Get>
        + From<Insert>
        + From<Keys>
        + From<Sequence>
        + From<Values>,
{
    match method {
        "entries" => Some(factory.create_builtin_term(Entries)),
        "get" => Some(factory.create_builtin_term(Get)),
        "has" => Some(factory.create_builtin_term(Contains)),
        "keys" => Some(factory.create_builtin_term(Keys)),
        "set" => Some({
            // Ensure value is resolved before inserting into underlying hashmap
            factory.create_lambda_term(
                3,
                factory.create_application_term(
                    factory.create_builtin_term(Sequence),
                    allocator.create_pair(
                        factory.create_variable_term(0),
                        factory.create_partial_application_term(
                            factory.create_builtin_term(Insert),
                            allocator.create_pair(
                                factory.create_variable_term(2),
                                factory.create_variable_term(1),
                            ),
                        ),
                    ),
                ),
            )
        }),
        "values" => Some(factory.create_builtin_term(Values)),
        _ => None,
    }
}

fn get_builtin_hashset_field<T: Expression>(
    method: &str,
    factory: &impl ExpressionFactory<T>,
) -> Option<T>
where
    T::Builtin: From<Push> + From<Entries> + From<Contains> + From<Values>,
{
    match method {
        "add" => Some(factory.create_builtin_term(Push)),
        "entries" => Some(factory.create_builtin_term(Entries)),
        "has" => Some(factory.create_builtin_term(Contains)),
        "values" => Some(factory.create_builtin_term(Values)),
        _ => None,
    }
}
