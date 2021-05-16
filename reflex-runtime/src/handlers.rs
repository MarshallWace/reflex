// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::once;

use crate::{create_signal_handler, handlers::http::handle_http_fetch, SignalHandler};

mod http;

pub fn builtin_signal_handlers() -> impl IntoIterator<Item = (&'static str, SignalHandler)> {
    once(create_signal_handler(
        "reflex::http::fetch",
        handle_http_fetch,
    ))
}
