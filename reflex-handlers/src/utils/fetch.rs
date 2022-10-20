// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::str::FromStr;

use bytes::Bytes;
use http::{
    header::HeaderName, method::InvalidMethod, uri::InvalidUri, HeaderValue, Method, StatusCode,
};
use hyper::{Body, Request, Uri};

#[derive(Eq, PartialEq, Clone, Hash, Debug)]
pub struct FetchRequest {
    pub url: String,
    pub method: String,
    pub headers: Vec<(HeaderName, HeaderValue)>,
    pub body: Option<Bytes>,
}

#[derive(Debug)]
pub enum FetchError {
    InvalidUri(InvalidUri, String),
    InvalidMethod(InvalidMethod, String),
    InvalidRequestBody(http::Error),
    NetworkError(hyper::Error),
    InvalidResponseBody(hyper::Error),
}
impl std::error::Error for FetchError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            FetchError::InvalidUri(err, _) => err.source(),
            FetchError::InvalidMethod(err, _) => err.source(),
            FetchError::InvalidRequestBody(err) => err.source(),
            FetchError::NetworkError(err) => err.source(),
            FetchError::InvalidResponseBody(err) => err.source(),
        }
    }
}
impl std::fmt::Display for FetchError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidUri(_, url) => write!(f, "Invalid HTTP URL: {}", url),
            Self::InvalidMethod(_, method) => write!(f, "Invalid HTTP method: {}", method),
            Self::InvalidRequestBody(err) => write!(f, "Invalid HTTP request body: {}", err),
            Self::NetworkError(err) => write!(f, "HTTP network error: {}", err),
            Self::InvalidResponseBody(err) => {
                write!(f, "Invalid HTTP response body: {}", err)
            }
        }
    }
}

pub fn parse_fetch_request(request: &FetchRequest) -> Result<Request<Body>, FetchError> {
    let url = request
        .url
        .parse::<Uri>()
        .map_err(|err| FetchError::InvalidUri(err, request.url.clone()))?;
    let method = Method::from_str(request.method.as_str())
        .map_err(|err| FetchError::InvalidMethod(err, request.method.clone()))?;
    let http_request = Request::builder().method(method).uri(url);
    let http_request = request
        .headers
        .iter()
        .fold(http_request, |http_request, (key, value)| {
            http_request.header(key.clone(), value.clone())
        });
    let body = Body::from(request.body.clone().unwrap_or(Bytes::new()));
    http_request
        .body(body)
        .map_err(FetchError::InvalidRequestBody)
}

pub async fn fetch<T>(
    client: hyper::Client<T, hyper::Body>,
    request: http::Request<Body>,
) -> Result<(StatusCode, Bytes), FetchError>
where
    T: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    let result = client
        .request(request)
        .await
        .map_err(FetchError::NetworkError)?;
    let status = result.status();
    let response = hyper::body::to_bytes(result.into_body())
        .await
        .map_err(FetchError::InvalidResponseBody)?;
    Ok((status, response))
}
