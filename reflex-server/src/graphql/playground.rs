// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::create_http_response;
use hyper::{header, Body, Request, Response, StatusCode};
use std::{borrow::Cow, convert::Infallible};

const PLAYGROUND_TEMPLATE: &'static [u8] = include_bytes!("playground/index.html");

pub(crate) async fn handle_playground_http_request(
    _req: Request<Body>,
) -> Result<Response<Body>, Infallible> {
    let body = match String::from_utf8_lossy(PLAYGROUND_TEMPLATE) {
        Cow::Borrowed(value) => String::from(value),
        Cow::Owned(value) => value,
    };
    Ok(create_http_response(
        StatusCode::OK,
        vec![(header::CONTENT_TYPE, String::from("text/html"))],
        Some(body),
    ))
}
