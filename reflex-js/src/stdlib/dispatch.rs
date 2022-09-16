// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::iter::once;

use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    FunctionArity, HeapAllocator, RefType, StringTermType, StringValue, Uid, Uuid,
};
use reflex_stdlib::Stdlib;

pub struct Dispatch {}
impl Dispatch {
    pub(crate) const UUID: Uuid = uuid!("f37c66ee-cd73-496d-8420-1ce83ab924ad");
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
    T::Builtin: From<Stdlib>,
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
    T::Builtin: From<Stdlib>,
{
    match method {
        "replace" => Some(factory.create_builtin_term(Stdlib::Replace)),
        "split" => Some(factory.create_builtin_term(Stdlib::Split)),
        _ => None,
    }
}

fn get_builtin_list_field<T: Expression>(
    method: &str,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Option<T>
where
    T::Builtin: From<Stdlib>,
{
    match method {
        "entries" => Some(factory.create_builtin_term(Stdlib::Entries)),
        "filter" => Some(factory.create_builtin_term(Stdlib::Filter)),
        "keys" => Some(factory.create_builtin_term(Stdlib::Keys)),
        "map" => Some(factory.create_builtin_term(Stdlib::Map)),
        "flatMap" => Some(factory.create_lambda_term(
            2,
            factory.create_application_term(
                factory.create_builtin_term(Stdlib::Flatten),
                allocator.create_unit_list(factory.create_application_term(
                    factory.create_builtin_term(Stdlib::ResolveShallow),
                    allocator.create_unit_list(factory.create_application_term(
                        factory.create_builtin_term(Stdlib::Map),
                        allocator.create_pair(
                            factory.create_variable_term(1),
                            factory.create_variable_term(0),
                        ),
                    )),
                )),
            ),
        )),
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
    allocator: &impl HeapAllocator<T>,
) -> Option<T>
where
    T::Builtin: From<Stdlib>,
{
    match method {
        "entries" => Some(factory.create_builtin_term(Stdlib::Entries)),
        "get" => Some(factory.create_builtin_term(Stdlib::Get)),
        "has" => Some(factory.create_builtin_term(Stdlib::Contains)),
        "keys" => Some(factory.create_builtin_term(Stdlib::Keys)),
        "set" => Some({
            // Ensure value is resolved before inserting into underlying hashmap
            factory.create_lambda_term(
                3,
                factory.create_application_term(
                    factory.create_builtin_term(Stdlib::Sequence),
                    allocator.create_pair(
                        factory.create_variable_term(0),
                        factory.create_partial_application_term(
                            factory.create_builtin_term(Stdlib::Insert),
                            allocator.create_pair(
                                factory.create_variable_term(2),
                                factory.create_variable_term(1),
                            ),
                        ),
                    ),
                ),
            )
        }),
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
