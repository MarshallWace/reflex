// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::once;

use futures_util::future::{AbortHandle, Abortable};
use reflex::{
    core::{
        Expression, ExpressionFactory, ExpressionList, HeapAllocator, Signal, SignalType,
        StateToken, StringValue,
    },
    lang::ValueTerm,
};
use reflex_runtime::{
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, RuntimeEffect,
    SignalHandlerResult, SignalHelpers, StateUpdate,
};

use crate::{utils::fetch, SignalResult};

pub fn http_fetch_handler<T: AsyncExpression>(
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
) -> impl Fn(&str, &[&Signal<T>], &SignalHelpers<T>) -> SignalHandlerResult<T> {
    let factory = factory.clone();
    let allocator = allocator.clone();
    move |signal_type: &str, signals: &[&Signal<T>], _helpers: &SignalHelpers<T>| {
        if signal_type != "reflex::http::fetch" {
            return None;
        }
        Some(
            signals
                .iter()
                .map(|signal| {
                    handle_http_fetch_signal(signal.id(), signal.args(), &factory, &allocator)
                })
                .collect(),
        )
    }
}

fn handle_http_fetch_signal<T: AsyncExpression>(
    signal_id: StateToken,
    args: &ExpressionList<T>,
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
) -> Result<SignalResult<T>, String> {
    let mut args = args.into_iter();
    if args.len() != 4 {
        return Err(format!(
            "Invalid fetch signal: Expected 4 arguments, received {}",
            args.len()
        ));
    }
    let url = parse_string_arg(args.next().unwrap(), factory);
    let method = parse_string_arg(args.next().unwrap(), factory);
    let headers = parse_key_values_arg(args.next().unwrap(), factory);
    let body = parse_optional_string_arg(args.next().unwrap(), factory);
    match (method, url, headers, body) {
        (Some(method), Some(url), Some(headers), Some(body)) => Ok((
            create_pending_signal(factory, allocator),
            Some({
                let (task, dispose) = {
                    let factory = factory.clone();
                    let allocator = allocator.clone();
                    let (abort_handle, abort_registration) = AbortHandle::new_pair();
                    let task = async move {
                        let value = match fetch(&method, &url, headers, body).await {
                            Ok((status, data)) => factory.create_tuple_term(allocator.create_pair(
                                factory.create_value_term(ValueTerm::Int(status as i32)),
                                factory.create_value_term(ValueTerm::String(
                                    allocator.create_string(data),
                                )),
                            )),
                            Err(error) => factory.create_signal_term(allocator.create_signal_list(
                                once(allocator.create_signal(
                                    SignalType::Error,
                                    allocator.create_unit_list(factory.create_value_term(
                                        ValueTerm::String(allocator.create_string(error)),
                                    )),
                                )),
                            )),
                        };
                        StateUpdate::value(signal_id, value)
                    };
                    let dispose = async move {
                        abort_handle.abort();
                    };
                    (Abortable::new(task, abort_registration), dispose)
                };
                RuntimeEffect::deferred(
                    Box::pin({
                        let factory = factory.clone();
                        let allocator = allocator.clone();
                        async move {
                            let result = task.await;
                            match result {
                                Ok(result) => result,
                                Err(_) => StateUpdate::value(
                                    signal_id,
                                    create_pending_signal(&factory, &allocator),
                                ),
                            }
                        }
                    }),
                    dispose,
                )
            }),
        )),
        _ => Err(String::from("Invalid fetch signal arguments")),
    }
}

fn create_pending_signal<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_signal_term(allocator.create_signal_list(once(
        allocator.create_signal(SignalType::Pending, allocator.create_empty_list()),
    )))
}

fn parse_string_arg<T: Expression>(
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Option<String> {
    match factory.match_value_term(value) {
        Some(ValueTerm::String(value)) => Some(String::from(value.as_str())),
        _ => None,
    }
}

fn parse_optional_string_arg<T: Expression>(
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Option<Option<String>> {
    match factory.match_value_term(value) {
        Some(ValueTerm::String(value)) => Some(Some(String::from(value.as_str()))),
        Some(ValueTerm::Null) => Some(None),
        _ => None,
    }
}

fn parse_key_values_arg<T: Expression>(
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Option<Vec<(String, String)>> {
    if let Some(value) = factory.match_struct_term(value) {
        value
            .entries()
            .into_iter()
            .map(|(key, value)| match factory.match_value_term(value) {
                Some(ValueTerm::String(value)) => {
                    Some((String::from(key.as_str()), String::from(value.as_str())))
                }
                _ => None,
            })
            .collect::<Option<Vec<_>>>()
    } else {
        None
    }
}
