// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
pub mod graphql;
pub mod http;

use std::collections::HashMap;

use reflex::core::SerializedTerm;
use reflex_runtime::SignalResult;

pub(crate) mod utils {
    mod fetch;
    pub(crate) use fetch::fetch;
}

pub fn builtin_signal_handler(
) -> impl Fn(&str, &[SerializedTerm]) -> Option<Result<SignalResult, String>> + Send + Sync + 'static
{
    create_signal_handler(vec![
        (
            "reflex::http::fetch",
            http::handle_http_fetch as fn(&[SerializedTerm]) -> Result<SignalResult, String>,
        ),
        (
            "reflex::graphql::execute",
            graphql::handle_graphql_execute
                as fn(&[SerializedTerm]) -> Result<SignalResult, String>,
        ),
    ])
}

pub fn create_signal_handler(
    handlers: impl IntoIterator<
        Item = (
            &'static str,
            fn(&[SerializedTerm]) -> Result<SignalResult, String>,
        ),
    >,
) -> impl Fn(&str, &[SerializedTerm]) -> Option<Result<SignalResult, String>> + Send + Sync + 'static
{
    let handlers = handlers.into_iter().collect::<HashMap<_, _>>();
    move |signal_type, args| match handlers.get(signal_type) {
        Some(handler) => Some(handler(args)),
        None => None,
    }
}

pub fn debug_signal_handler<THandler>(
    handler: THandler,
) -> impl Fn(&str, &[SerializedTerm]) -> Option<Result<SignalResult, String>> + Send + Sync + 'static
where
    THandler:
        Fn(&str, &[SerializedTerm]) -> Option<Result<SignalResult, String>> + Send + Sync + 'static,
{
    move |signal_type, args| {
        println!(
            "{}{}",
            signal_type,
            if args.is_empty() {
                String::from("")
            } else {
                args.iter()
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<_>>()
                    .join(" ")
            }
        );
        handler(signal_type, args)
    }
}
