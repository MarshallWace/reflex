// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::once;

use reflex::{
    core::{
        uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
        FunctionArity, HeapAllocator, StringValue, Uid, Uuid,
    },
    lang::ValueTerm,
    stdlib::Stdlib,
};

pub struct Dispatch {}
impl Dispatch {
    const UUID: Uuid = uuid!("f37c66ee-cd73-496d-8420-1ce83ab924ad");
    const ARITY: FunctionArity<3, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict, ArgType::Lazy],
        optional: [],
        variadic: Some(ArgType::Lazy),
    };
}
impl Uid for Dispatch {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Dispatch
where
    T::Builtin: From<Stdlib>,
{
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
        let method_name = args.next().unwrap();
        let fallback = args.next().unwrap();
        let builtin_method = match factory.match_value_term(&method_name) {
            Some(ValueTerm::String(method_name)) => {
                get_builtin_field(Some(&target), method_name.as_str(), factory)
            }
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
) -> Option<T>
where
    T::Builtin: From<Stdlib>,
{
    None.or_else(|| {
        if target.is_none()
            || target
                .map(|target| match_string_value_term(target, factory))
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
                .map(|target| factory.match_vector_term(target))
                .is_some()
        {
            get_builtin_vector_field(method, factory)
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
            get_builtin_hashmap_field(method, factory)
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
    T::Builtin: From<Stdlib>,
{
    match method {
        "replace" => Some(factory.create_builtin_term(Stdlib::Replace)),
        "split" => Some(factory.create_builtin_term(Stdlib::Split)),
        _ => None,
    }
}

fn get_builtin_vector_field<T: Expression>(
    method: &str,
    factory: &impl ExpressionFactory<T>,
) -> Option<T>
where
    T::Builtin: From<Stdlib>,
{
    match method {
        "entries" => Some(factory.create_builtin_term(Stdlib::Entries)),
        "filter" => Some(factory.create_builtin_term(Stdlib::Filter)),
        "keys" => Some(factory.create_builtin_term(Stdlib::Keys)),
        "map" => Some(factory.create_builtin_term(Stdlib::Map)),
        "push" => Some(factory.create_builtin_term(Stdlib::Push)),
        "reduce" => Some(factory.create_builtin_term(Stdlib::Reduce)),
        "slice" => Some(factory.create_builtin_term(Stdlib::Slice)),
        "unshift" => Some(factory.create_builtin_term(Stdlib::PushFront)),
        "values" => Some(factory.create_builtin_term(Stdlib::Values)),
        _ => None,
    }
}

fn get_builtin_hashmap_field<T: Expression>(
    method: &str,
    factory: &impl ExpressionFactory<T>,
) -> Option<T>
where
    T::Builtin: From<Stdlib>,
{
    match method {
        "entries" => Some(factory.create_builtin_term(Stdlib::Entries)),
        "get" => Some(factory.create_builtin_term(Stdlib::Get)),
        "has" => Some(factory.create_builtin_term(Stdlib::Contains)),
        "keys" => Some(factory.create_builtin_term(Stdlib::Keys)),
        "set" => Some(factory.create_builtin_term(Stdlib::Insert)),
        "values" => Some(factory.create_builtin_term(Stdlib::Values)),
        _ => None,
    }
}

fn get_builtin_hashset_field<T: Expression>(
    method: &str,
    factory: &impl ExpressionFactory<T>,
) -> Option<T>
where
    T::Builtin: From<Stdlib>,
{
    match method {
        "add" => Some(factory.create_builtin_term(Stdlib::Push)),
        "entries" => Some(factory.create_builtin_term(Stdlib::Entries)),
        "has" => Some(factory.create_builtin_term(Stdlib::Contains)),
        "values" => Some(factory.create_builtin_term(Stdlib::Values)),
        _ => None,
    }
}

fn match_string_value_term<'a, T: Expression>(
    target: &'a T,
    factory: &impl ExpressionFactory<T>,
) -> Option<&'a T::String> {
    factory
        .match_value_term(target)
        .and_then(|target| target.match_string())
}