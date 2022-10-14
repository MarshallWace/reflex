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

#[derive(Clone, Debug)]
pub enum EitherTracer<L, R> {
    Left(L),
    Right(R),
}
impl<L: opentelemetry::trace::Tracer, R: opentelemetry::trace::Tracer> opentelemetry::trace::Tracer
    for EitherTracer<L, R>
where
    L::Span: Send + Sync + 'static,
    R::Span: Send + Sync + 'static,
{
    type Span = EitherSpan<L::Span, R::Span>;
    fn start_with_context<T>(&self, name: T, parent_cx: &opentelemetry::Context) -> Self::Span
    where
        T: Into<std::borrow::Cow<'static, str>>,
    {
        match self {
            Self::Left(inner) => EitherSpan::Left(inner.start_with_context(name, parent_cx)),
            Self::Right(inner) => EitherSpan::Right(inner.start_with_context(name, parent_cx)),
        }
    }
    fn span_builder<T>(&self, name: T) -> opentelemetry::trace::SpanBuilder
    where
        T: Into<std::borrow::Cow<'static, str>>,
    {
        match self {
            Self::Left(inner) => inner.span_builder(name),
            Self::Right(inner) => inner.span_builder(name),
        }
    }
    fn build_with_context(
        &self,
        builder: opentelemetry::trace::SpanBuilder,
        parent_cx: &opentelemetry::Context,
    ) -> Self::Span {
        match self {
            Self::Left(inner) => EitherSpan::Left(inner.build_with_context(builder, parent_cx)),
            Self::Right(inner) => EitherSpan::Right(inner.build_with_context(builder, parent_cx)),
        }
    }
    fn start<T>(&self, name: T) -> Self::Span
    where
        T: Into<std::borrow::Cow<'static, str>>,
    {
        match self {
            Self::Left(inner) => EitherSpan::Left(inner.start(name)),
            Self::Right(inner) => EitherSpan::Right(inner.start(name)),
        }
    }
    fn build(&self, builder: opentelemetry::trace::SpanBuilder) -> Self::Span {
        match self {
            Self::Left(inner) => EitherSpan::Left(inner.build(builder)),
            Self::Right(inner) => EitherSpan::Right(inner.build(builder)),
        }
    }
    fn in_span<T, F>(&self, name: &'static str, f: F) -> T
    where
        F: FnOnce(opentelemetry::Context) -> T,
        Self::Span: Send + Sync + 'static,
    {
        match self {
            Self::Left(inner) => inner.in_span(name, f),
            Self::Right(inner) => inner.in_span(name, f),
        }
    }
    fn with_span<T, F>(&self, span: Self::Span, f: F) -> T
    where
        F: FnOnce(opentelemetry::Context) -> T,
        Self::Span: Send + Sync + 'static,
    {
        match (self, span) {
            (Self::Left(inner), EitherSpan::Left(span)) => inner.with_span(span, f),
            (Self::Right(inner), EitherSpan::Right(span)) => inner.with_span(span, f),
            _ => panic!("Invalid span type"),
        }
    }
}

pub enum EitherSpan<L, R> {
    Left(L),
    Right(R),
}
impl<L, R> opentelemetry::trace::Span for EitherSpan<L, R>
where
    L: opentelemetry::trace::Span,
    R: opentelemetry::trace::Span,
{
    fn add_event_with_timestamp<T>(
        &mut self,
        name: T,
        timestamp: std::time::SystemTime,
        attributes: Vec<opentelemetry::KeyValue>,
    ) where
        T: Into<std::borrow::Cow<'static, str>>,
    {
        match self {
            Self::Left(inner) => inner.add_event_with_timestamp(name, timestamp, attributes),
            Self::Right(inner) => inner.add_event_with_timestamp(name, timestamp, attributes),
        }
    }
    fn span_context(&self) -> &opentelemetry::trace::SpanContext {
        match self {
            Self::Left(inner) => inner.span_context(),
            Self::Right(inner) => inner.span_context(),
        }
    }
    fn is_recording(&self) -> bool {
        match self {
            Self::Left(inner) => inner.is_recording(),
            Self::Right(inner) => inner.is_recording(),
        }
    }
    fn set_attribute(&mut self, attribute: opentelemetry::KeyValue) {
        match self {
            Self::Left(inner) => inner.set_attribute(attribute),
            Self::Right(inner) => inner.set_attribute(attribute),
        }
    }
    fn set_status(&mut self, code: opentelemetry::trace::StatusCode, message: String) {
        match self {
            Self::Left(inner) => inner.set_status(code, message),
            Self::Right(inner) => inner.set_status(code, message),
        }
    }
    fn update_name<T>(&mut self, new_name: T)
    where
        T: Into<std::borrow::Cow<'static, str>>,
    {
        match self {
            Self::Left(inner) => inner.update_name(new_name),
            Self::Right(inner) => inner.update_name(new_name),
        }
    }
    fn end_with_timestamp(&mut self, timestamp: std::time::SystemTime) {
        match self {
            Self::Left(inner) => inner.end_with_timestamp(timestamp),
            Self::Right(inner) => inner.end_with_timestamp(timestamp),
        }
    }
    fn add_event<T>(&mut self, name: T, attributes: Vec<opentelemetry::KeyValue>)
    where
        T: Into<std::borrow::Cow<'static, str>>,
    {
        match self {
            Self::Left(inner) => inner.add_event(name, attributes),
            Self::Right(inner) => inner.add_event(name, attributes),
        }
    }
    fn record_exception(&mut self, err: &dyn std::error::Error) {
        match self {
            Self::Left(inner) => inner.record_exception(err),
            Self::Right(inner) => inner.record_exception(err),
        }
    }
    fn record_exception_with_stacktrace<T>(&mut self, err: &dyn std::error::Error, stacktrace: T)
    where
        T: Into<std::borrow::Cow<'static, str>>,
    {
        match self {
            Self::Left(inner) => inner.record_exception_with_stacktrace(err, stacktrace),
            Self::Right(inner) => inner.record_exception_with_stacktrace(err, stacktrace),
        }
    }
    fn end(&mut self) {
        match self {
            Self::Left(inner) => inner.end(),
            Self::Right(inner) => inner.end(),
        }
    }
}
