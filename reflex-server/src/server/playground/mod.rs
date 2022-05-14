// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::server::utils::create_http_response;
use http::{header, HeaderValue, Request, Response, StatusCode};
use std::borrow::Cow;

const PLAYGROUND_TEMPLATE: &'static [u8] = include_bytes!("static/index.html");

pub(crate) async fn handle_playground_http_request<T: From<String> + Default>(
    _req: Request<T>,
) -> Response<T> {
    let body = match String::from_utf8_lossy(PLAYGROUND_TEMPLATE) {
        Cow::Borrowed(value) => String::from(value),
        Cow::Owned(value) => value,
    };
    create_http_response(
        StatusCode::OK,
        vec![(header::CONTENT_TYPE, HeaderValue::from_static("text/html"))],
        Some(body),
    )
}
