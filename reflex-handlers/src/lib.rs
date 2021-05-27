// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
pub mod graphql;
pub mod http;

use reflex::core::SerializedTerm;
use reflex_runtime::{SignalHandler, SignalResult};

pub(crate) mod utils {
    mod fetch;
    pub(crate) use fetch::fetch;
}

pub fn builtin_signal_handlers() -> impl IntoIterator<Item = (&'static str, SignalHandler)> {
    vec![
        create_signal_handler("reflex::http::fetch", http::handle_http_fetch),
        create_signal_handler("reflex::graphql::execute", graphql::handle_graphql_execute),
    ]
}

pub fn create_signal_handler(
    signal_type: &'static str,
    handler: impl Fn(Option<&[SerializedTerm]>) -> Result<SignalResult, String> + Send + Sync + 'static,
) -> (&'static str, SignalHandler) {
    (signal_type, Box::new(handler))
}
