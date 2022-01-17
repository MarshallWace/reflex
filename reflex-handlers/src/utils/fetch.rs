// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::str::FromStr;

use bytes::Bytes;
use futures::Future;
use http::{
    header::HeaderName, method::InvalidMethod, uri::InvalidUri, HeaderValue, Method, StatusCode,
};
use hyper::{Body, Request, Uri};
use hyper_tls::HttpsConnector;

#[derive(Eq, PartialEq, Hash, Debug)]
pub(crate) struct FetchRequest {
    pub url: String,
    pub method: String,
    pub headers: Vec<(HeaderName, HeaderValue)>,
    pub body: Option<Bytes>,
}

#[derive(Debug)]
pub(crate) enum FetchError {
    InvalidUri(InvalidUri, String),
    InvalidMethod(InvalidMethod, String),
    InvalidRequestBody(http::Error),
    NetworkError(hyper::Error),
    InvalidResponseBody(hyper::Error),
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

pub(crate) fn fetch(
    request: &FetchRequest,
) -> Result<impl Future<Output = Result<(StatusCode, Bytes), FetchError>>, FetchError> {
    let https = HttpsConnector::new();
    let client = hyper::Client::builder().build::<_, hyper::Body>(https);
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
    let http_request = http_request
        .body(body)
        .map_err(FetchError::InvalidRequestBody)?;
    Ok(async move {
        let result = client
            .request(http_request)
            .await
            .map_err(FetchError::NetworkError)?;
        let status = result.status();
        let response = hyper::body::to_bytes(result.into_body())
            .await
            .map_err(FetchError::InvalidResponseBody)?;
        Ok((status, response))
    })
}
