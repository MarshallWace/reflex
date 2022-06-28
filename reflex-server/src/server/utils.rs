// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::once;

use http::{
    header::{self, HeaderName},
    HeaderMap, HeaderValue, Request, Response, StatusCode,
};
use reflex_json::JsonValue;

pub fn clone_http_request<T: Clone>(request: &Request<T>) -> Request<T> {
    clone_http_request_wrapper(request).map(|_| request.body().clone())
}

pub fn clone_http_request_wrapper<T>(request: &Request<T>) -> Request<()> {
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

pub fn clone_http_response<T: Clone>(response: &Response<T>) -> Response<T> {
    clone_http_response_wrapper(response).map(|_| response.body().clone())
}

pub fn clone_http_response_wrapper<T>(response: &Response<T>) -> Response<()> {
    let mut result = Response::new(());
    *result.status_mut() = response.status();
    *result.version_mut() = response.version();
    let headers = result.headers_mut();
    for (key, value) in response.headers() {
        headers.append(key.clone(), value.clone());
    }
    result
}

pub fn create_json_http_response<T: From<String> + Default>(
    status: StatusCode,
    headers: impl IntoIterator<Item = (HeaderName, HeaderValue)>,
    body: &JsonValue,
) -> Response<T> {
    create_http_response(
        status,
        headers
            .into_iter()
            .chain(once(create_content_type_header("application/json"))),
        Some(body.to_string()),
    )
}

pub fn create_html_http_response<T: From<String> + Default>(
    status: StatusCode,
    headers: impl IntoIterator<Item = (HeaderName, HeaderValue)>,
    body: impl Into<String>,
) -> Response<T> {
    create_http_response(
        status,
        headers
            .into_iter()
            .chain(once(create_content_type_header("text/html"))),
        Some(body.into()),
    )
}

pub fn create_accepted_http_response<T: Default + From<String>>(
    status_code: StatusCode,
    headers: impl IntoIterator<Item = (HeaderName, HeaderValue)>,
    body: Option<String>,
    request_headers: &HeaderMap,
) -> Response<T> {
    let body = match body {
        Some(body) => Some(body),
        None if !status_code.is_success() => Some(format!("{}", status_code)),
        _ => None,
    };
    if is_json_request(request_headers) {
        create_json_http_response(
            status_code,
            headers,
            &match body {
                Some(body) => JsonValue::String(body),
                None => JsonValue::Null,
            },
        )
    } else {
        create_http_response(
            status_code,
            once((
                http::header::CONTENT_TYPE,
                HeaderValue::from_static("text/plain"),
            )),
            body,
        )
    }
}

pub(crate) fn is_json_request(headers: &HeaderMap) -> bool {
    headers
        .get(http::header::ACCEPT)
        .map(|header_value| {
            find_subsequence_index(header_value.as_ref(), b"application/json").is_some()
        })
        .unwrap_or(false)
}

fn find_subsequence_index<T>(haystack: &[T], needle: &[T]) -> Option<usize>
where
    for<'a> &'a [T]: PartialEq,
{
    haystack
        .windows(needle.len())
        .position(|window| window == needle)
}

pub(crate) fn create_content_type_header(value: &'static str) -> (HeaderName, HeaderValue) {
    (header::CONTENT_TYPE, HeaderValue::from_static(value))
}

pub(crate) fn create_http_response<T>(
    status: StatusCode,
    headers: impl IntoIterator<Item = (HeaderName, HeaderValue)>,
    body: Option<String>,
) -> Response<T>
where
    T: From<String> + Default,
{
    let body = match body {
        Some(contents) => contents.into(),
        None => Default::default(),
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
