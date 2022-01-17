// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::once;

use bytes::Bytes;
use http::{
    header::{self, HeaderName},
    HeaderValue, Request, Response, StatusCode,
};
use reflex_json::JsonValue;

pub(crate) fn clone_request_wrapper<T>(request: &Request<T>) -> Request<()> {
    let mut result = Request::new(());
    *result.method_mut() = request.method().clone();
    *result.uri_mut() = request.uri().clone();
    *result.version_mut() = request.version();
    let headers = result.headers_mut();
    for (key, value) in request.headers() {
        headers.append(key.clone(), value.clone());
    }
    result
}

pub(crate) fn clone_response_wrapper<T>(response: &Response<T>) -> Response<()> {
    let mut result = Response::new(());
    *result.status_mut() = response.status();
    *result.version_mut() = response.version();
    let headers = result.headers_mut();
    for (key, value) in response.headers() {
        headers.append(key.clone(), value.clone());
    }
    result
}

pub(crate) fn create_json_http_response(
    status: StatusCode,
    headers: impl IntoIterator<Item = (HeaderName, HeaderValue)>,
    body: &JsonValue,
) -> Response<Bytes> {
    create_http_response(
        status,
        headers
            .into_iter()
            .chain(once(create_content_type_header("application/json"))),
        Some(body.to_string()),
    )
}

fn create_content_type_header(value: &'static str) -> (HeaderName, HeaderValue) {
    (header::CONTENT_TYPE, HeaderValue::from_static(value))
}

pub(crate) fn create_http_response(
    status: StatusCode,
    headers: impl IntoIterator<Item = (HeaderName, HeaderValue)>,
    body: Option<String>,
) -> Response<Bytes> {
    let body = match body {
        Some(contents) => Bytes::from(contents),
        None => Bytes::default(),
    };
    let mut response = Response::new(body);
    *response.status_mut() = status;
    let response_headers = response.headers_mut();
    for (key, value) in headers.into_iter() {
        response_headers.insert(key, value);
    }
    response
}

pub(crate) fn get_cors_headers<T>(
    req: &Request<T>,
) -> impl IntoIterator<Item = (HeaderName, HeaderValue)> {
    once((
        header::ACCESS_CONTROL_ALLOW_METHODS,
        HeaderValue::from_static("OPTIONS, GET, POST"),
    ))
    .chain(once((
        header::ACCESS_CONTROL_ALLOW_CREDENTIALS,
        HeaderValue::from_static("true"),
    )))
    .chain(once((
        header::ACCESS_CONTROL_ALLOW_ORIGIN,
        req.headers()
            .get(header::ORIGIN)
            .cloned()
            .unwrap_or_else(|| HeaderValue::from_static("*")),
    )))
    .chain(once((
        header::ACCESS_CONTROL_ALLOW_HEADERS,
        req.headers()
            .get(header::ACCESS_CONTROL_REQUEST_HEADERS)
            .cloned()
            .unwrap_or_else(|| HeaderValue::from_static("*")),
    )))
}
