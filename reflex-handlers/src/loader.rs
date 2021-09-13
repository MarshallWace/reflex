// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{
        hash_map::{DefaultHasher, Entry},
        HashMap,
    },
    hash::Hasher,
    iter::once,
};

use futures_util::{future, stream, StreamExt};
use reflex::{
    compiler::Compile,
    core::{
        Applicable, Expression, ExpressionFactory, HeapAllocator, Reducible, Rewritable, Signal,
        SignalId, SignalType, StateToken,
    },
    hash::{hash_object, HashId},
    lang::{BuiltinTerm, ValueTerm, WithCompiledBuiltins},
};
use reflex_runtime::{
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, RuntimeEffect,
    SignalHandlerResult, SignalHelpers, StateUpdate, Stream,
};

pub(crate) const SIGNAL_TYPE_LOAD: &'static str = "reflex::loader::load";

const HASH_NAMESPACE_LOADER: &'static str = "reflex::loader";

pub fn load_signal_handler<
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
>(
    factory: &(impl AsyncExpressionFactory<T> + WithCompiledBuiltins),
    allocator: &impl AsyncHeapAllocator<T>,
) -> impl Fn(&str, &[&Signal<T>], &SignalHelpers<T>) -> SignalHandlerResult<T>
where
    T::String: Send + Sync,
{
    let factory = factory.clone();
    let allocator = allocator.clone();
    move |signal_type: &str, signals: &[&Signal<T>], helpers: &SignalHelpers<T>| {
        if signal_type != SIGNAL_TYPE_LOAD {
            return None;
        }
        let parsed_signals = signals
            .iter()
            .map(|signal| (*signal, parse_load_signal(signal, &factory)))
            .collect::<Vec<_>>();

        let mut keys_by_loader = parsed_signals
            .iter()
            .filter_map(|(signal, result)| match result {
                Err(_) => None,
                Ok((loader, keys)) => Some((*signal, *loader, *keys)),
            })
            .fold(
                HashMap::<
                    &T,
                    (
                        HashId,
                        Vec<StateToken>,
                        Vec<T>,
                        HashMap<&T, usize>,
                        &Signal<T>,
                    ),
                >::new(),
                |mut results, (signal, loader, keys)| {
                    match results.entry(loader) {
                        Entry::Occupied(mut entry) => {
                            let (
                                _,
                                existing_signals,
                                existing_keys,
                                loader_key_lookup,
                                last_signal,
                            ) = entry.get_mut();
                            let num_existing_keys = existing_keys.len();
                            existing_keys.extend(keys.iter().cloned());
                            existing_signals.push(signal.id());
                            loader_key_lookup.extend(
                                keys.iter()
                                    .enumerate()
                                    .map(|(index, key)| (key, num_existing_keys + index)),
                            );
                            *last_signal = signal;
                        }
                        Entry::Vacant(entry) => {
                            entry.insert((
                                0,
                                vec![signal.id()],
                                keys.iter().cloned().collect(),
                                keys.iter()
                                    .enumerate()
                                    .map(|(index, key)| (key, index))
                                    .collect(),
                                signal,
                            ));
                        }
                    };
                    results
                },
            );
        for (loader, entry) in keys_by_loader.iter_mut() {
            let (_, _, keys, _, _) = entry;
            entry.0 = get_loader_cache_key(*loader, keys)
        }
        Some(
            parsed_signals
                .into_iter()
                .map(|(signal, parsed_signal)| match parsed_signal {
                    Err(error) => Err(error),
                    Ok((loader, keys)) => match keys_by_loader.get(loader) {
                        None => Err(format!("Unhandled signal: {}", signal)),
                        Some((cache_key, _, _, loader_key_lookup, last_signal)) => {
                            let cache = factory.create_dynamic_variable_term(
                                *cache_key,
                                create_pending(&factory, &allocator),
                            );
                            let cache_entries = {
                                let factory = factory
                                    .with_compiled_builtins(helpers.builtins(), helpers.plugins());
                                factory.create_vector_term(allocator.create_list(keys.iter().map(
                                    |key| {
                                        create_property_accessor(
                                            cache.clone(),
                                            factory.create_value_term(ValueTerm::Int(
                                                *(loader_key_lookup.get(key).unwrap()) as i32,
                                            )),
                                            &factory,
                                            &allocator,
                                        )
                                    },
                                )))
                            };
                            let effect = if signal == *last_signal {
                                let (cache_key, signal_ids, loader_keys, _, _) =
                                    keys_by_loader.remove(loader).unwrap();
                                let loader = loader.clone();
                                println!(
                                    "[Loader] {}: Subscribing to {} keys",
                                    hash_object(&loader),
                                    loader_keys.len()
                                );
                                let factory = factory.clone();
                                let allocator = allocator.clone();
                                // TODO: Dispose dataloader subscriptions
                                let dispose = future::ready(());
                                Some(RuntimeEffect::stream(
                                    load_batch(
                                        &loader,
                                        &loader_keys,
                                        signal_ids,
                                        helpers,
                                        &factory,
                                        &allocator,
                                    )
                                    .map(move |results| {
                                        let results = match results {
                                            Ok(results) => results,
                                            Err(errors) => {
                                                let error = create_combined_error_expression(
                                                    errors, &factory, &allocator,
                                                );
                                                loader_keys.iter().map(|_| error.clone()).collect()
                                            }
                                        };
                                        println!(
                                            "[Loader] {}: Received {} results",
                                            hash_object(&loader),
                                            results.len(),
                                        );
                                        StateUpdate::value(
                                            cache_key,
                                            factory
                                                .create_vector_term(allocator.create_list(results)),
                                        )
                                    }),
                                    dispose,
                                ))
                            } else {
                                None
                            };
                            Ok((cache_entries, effect))
                        }
                    },
                })
                .collect(),
        )
    }
}

fn get_loader_cache_key<T: Expression>(loader: &T, keys: &[T]) -> StateToken {
    generate_hash(HASH_NAMESPACE_LOADER, once(loader).chain(keys))
}

fn load_batch<T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>>(
    loader: &T,
    keys: &[T],
    signal_ids: impl IntoIterator<Item = SignalId>,
    helpers: &SignalHelpers<T>,
    factory: &(impl AsyncExpressionFactory<T> + WithCompiledBuiltins),
    allocator: &impl AsyncHeapAllocator<T>,
) -> impl Stream<Item = Result<Vec<T>, Vec<String>>>
where
    T::String: Send + Sync,
{
    let keys = keys.iter().cloned().collect::<Vec<_>>();
    let expression = factory.create_application_term(
        loader.clone(),
        allocator.create_unit_list(
            factory.create_vector_term(allocator.create_list(keys.iter().cloned())),
        ),
    );
    match helpers
        .clone()
        .watch_expression(expression, signal_ids, factory, allocator)
    {
        Err(error) => stream::iter(once(Err(vec![error]))).left_stream(),
        Ok(results) => results
            .map({
                let factory = factory.clone();
                move |result| {
                    result.and_then(|result| {
                        if let Some(results) = factory.match_hashmap_term(&result) {
                            keys.iter()
                                .map(|key| {
                                    results
                                        .get(key)
                                        .cloned()
                                        .ok_or_else(|| format!("Missing value for item {}", key))
                                })
                                .collect::<Result<Vec<_>, _>>()
                        } else if let Some(results) = factory.match_vector_term(&result) {
                            if results.items().len() != keys.len() {
                                Err(format!(
                                    "Expected {} items, received {}",
                                    keys.len(),
                                    results.items().len()
                                ))
                            } else {
                                Ok(results.items().iter().cloned().collect::<Vec<_>>())
                            }
                        } else if let Some(_) = factory.match_signal_term(&result) {
                            Ok(keys.iter().cloned().collect::<Vec<_>>())
                        } else {
                            Err(format!("Expected HashMap or Vector, received {}", result))
                        }
                        .map_err(|err| vec![err])
                    })
                }
            })
            .right_stream(),
    }
}

fn parse_load_signal<'a, T: Expression + Applicable<T>>(
    signal: &'a Signal<T>,
    factory: &impl ExpressionFactory<T>,
) -> Result<(&'a T, &'a [T]), String> {
    let args = signal.args();
    if args.len() != 2 {
        return Err(format!(
            "Invalid load signal: Expected 2 arguments, received {}",
            args.len()
        ));
    }
    let mut args = args.into_iter();
    let loader = args.next().unwrap();
    let keys = args.next().unwrap();
    match_loader_expression(loader).and_then(|loader| {
        if let Some(keys) = factory.match_vector_term(keys) {
            Ok((loader, keys.items().as_slice()))
        } else {
            Err(format!(
                "Invalid load signal keys: Expected Vector, received {}",
                keys
            ))
        }
    })
}

fn match_loader_expression<'a, T: Expression + Applicable<T>>(
    loader: &'a T,
) -> Result<&'a T, String> {
    match loader.arity() {
        Some(arity)
            if (arity.required() == 1) || (arity.required() < 1 && arity.variadic().is_some()) =>
        {
            Ok(loader)
        }
        _ => Err(format!(
            "Invalid load signal factory: Expected <function:1>, received {}",
            loader
        )),
    }
}

fn create_property_accessor<T: Expression>(
    target: T,
    key: T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_application_term(
        factory.create_builtin_term(BuiltinTerm::Get),
        allocator.create_pair(target, key),
    )
}

fn create_combined_error_expression<T: Expression>(
    messages: Vec<String>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_signal_term(
        allocator.create_signal_list(
            messages
                .into_iter()
                .map(|message| create_error_signal(message, factory, allocator)),
        ),
    )
}

fn create_error_signal<T: Expression>(
    message: String,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Signal<T> {
    allocator.create_signal(
        SignalType::Error,
        allocator.create_unit_list(
            factory.create_value_term(ValueTerm::String(allocator.create_string(message))),
        ),
    )
}

fn create_pending<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_signal_term(allocator.create_signal_list(once(
        allocator.create_signal(SignalType::Pending, allocator.create_empty_list()),
    )))
}

fn generate_hash<'a, T: std::hash::Hash + 'a>(
    namespace: &str,
    args: impl IntoIterator<Item = &'a T>,
) -> HashId {
    let mut hasher = DefaultHasher::new();
    hasher.write(namespace.as_bytes());
    for arg in args {
        arg.hash(&mut hasher);
    }
    hasher.finish()
}
