// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use http::{
    request::{Builder as RequestBuilder, Parts as RequestHead},
    response::{Builder as ResponseBuilder, Parts as ResponseHead},
    HeaderMap, HeaderValue, Method, Request, Response, Uri,
};
use reflex_handlers::utils::serialize::SerializedBytes;
use serde::{Deserialize, Serialize};

use crate::server::utils::{clone_http_request_wrapper, clone_http_response_wrapper};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SerializedRequest {
    head: SerializedRequestHead,
    body: SerializedBytes,
}
impl<'a, T> From<&'a Request<T>> for SerializedRequest
where
    &'a T: Into<SerializedBytes>,
{
    fn from(value: &'a Request<T>) -> Self {
        let (head, _) = clone_http_request_wrapper(value).into_parts();
        Self {
            head: (&head).into(),
            body: value.body().into(),
        }
    }
}
impl<T> From<SerializedRequest> for Request<T>
where
    T: From<SerializedBytes>,
{
    fn from(value: SerializedRequest) -> Self {
        let SerializedRequest { head, body } = value;
        Self::from_parts(head.into(), body.into())
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SerializedResponse {
    head: SerializedResponseHead,
    body: SerializedBytes,
}
impl<'a, T> From<&'a Response<T>> for SerializedResponse
where
    &'a T: Into<SerializedBytes>,
{
    fn from(value: &'a Response<T>) -> Self {
        let (head, _) = clone_http_response_wrapper(value).into_parts();
        Self {
            head: (&head).into(),
            body: value.body().into(),
        }
    }
}
impl<T> From<SerializedResponse> for Response<T>
where
    T: From<SerializedBytes>,
{
    fn from(value: SerializedResponse) -> Self {
        let SerializedResponse { head, body } = value;
        Self::from_parts(head.into(), body.into())
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SerializedRequestHead {
    #[serde(with = "http_serde::method")]
    pub method: Method,
    #[serde(with = "http_serde::uri")]
    pub uri: Uri,
    #[serde(with = "http_serde::header_map")]
    pub headers: HeaderMap<HeaderValue>,
}
impl<'a> From<&'a RequestHead> for SerializedRequestHead {
    fn from(value: &'a RequestHead) -> Self {
        let RequestHead {
            method,
            uri,
            headers,
            version: _,
            extensions: _,
            ..
        } = value;
        Self {
            method: method.clone(),
            uri: uri.clone(),
            headers: headers.clone(),
        }
    }
}
impl From<SerializedRequestHead> for RequestHead {
    fn from(value: SerializedRequestHead) -> Self {
        let SerializedRequestHead {
            method,
            uri,
            headers,
        } = value;
        let mut request = RequestBuilder::new().method(method).uri(uri);
        if let Some(request_headers) = request.headers_mut() {
            request_headers.extend(headers)
        }
        let (head, _) = request.body(()).unwrap().into_parts();
        head
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SerializedResponseHead {
    pub status: u16,
    #[serde(with = "http_serde::header_map")]
    pub headers: HeaderMap<HeaderValue>,
}
impl<'a> From<&'a ResponseHead> for SerializedResponseHead {
    fn from(value: &'a ResponseHead) -> Self {
        let ResponseHead {
            status,
            headers,
            version: _,
            extensions: _,
            ..
        } = value;
        Self {
            status: status.as_u16(),
            headers: headers.clone(),
        }
    }
}
impl From<SerializedResponseHead> for ResponseHead {
    fn from(value: SerializedResponseHead) -> Self {
        let SerializedResponseHead { status, headers } = value;
        let mut response = ResponseBuilder::new().status(status);
        if let Some(response_headers) = response.headers_mut() {
            response_headers.extend(headers)
        }
        let (head, _) = response.body(()).unwrap().into_parts();
        head
    }
}
