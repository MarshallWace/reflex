// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{create_http_response, get_cors_headers};
use hyper::{header, Body, Request, Response, StatusCode};
use reflex::{
    cache::SubstitutionCache,
    core::{ApplicationTerm, Expression, Term},
    hash::hash_object,
    serialize::{serialize, SerializedObjectTerm},
};
use reflex_graphql::{
    create_introspection_query_response, deserialize_graphql_operation,
    wrap_graphql_error_response, wrap_graphql_success_response,
};
use reflex_json::stringify;
use reflex_runtime::{Runtime, StreamExt, SubscriptionResult};
use std::{convert::Infallible, iter::once, sync::Arc};

pub(crate) async fn handle_graphql_http_request(
    req: Request<Body>,
    store: Arc<Runtime>,
    root: Expression,
) -> Result<Response<Body>, Infallible> {
    let cors_headers = get_cors_headers(&req);
    let request_etag = parse_request_etag(&req);
    let response = match parse_graphql_request(req, &root).await {
        Err(response) => Ok(response),
        Ok(query) => {
            let query = query
                .optimize(&mut SubstitutionCache::new())
                .unwrap_or(query);
            match store.subscribe(query).await {
                Err(error) => Err(error),
                Ok(mut results) => match results.next().await {
                    None => Err(String::from("Empty result stream")),
                    Some(result) => match results.unsubscribe().await {
                        Ok(_) => Ok(format_http_response(result)),
                        Err(error) => Err(error),
                    },
                },
            }
        }
    }
    .unwrap_or_else(|error| HttpResult::error(StatusCode::INTERNAL_SERVER_ERROR, error));
    let response_etag = parse_response_etag(&response);
    let is_matching_etag = match (&request_etag, &response_etag) {
        (Some(request_etag), Some(response_etag)) => request_etag == response_etag,
        _ => false,
    };
    if is_matching_etag {
        return Ok(create_http_response(
            StatusCode::NOT_MODIFIED,
            cors_headers,
            None,
        ));
    }
    let payload = match response.result {
        Ok(result) => serialize(result.value()).map(|result| wrap_graphql_success_response(result)),
        Err(errors) => Ok(wrap_graphql_error_response(errors)),
    };
    let (status, body) = match payload.and_then(stringify) {
        Ok(body) => (response.status, body),
        Err(error) => (
            StatusCode::NOT_ACCEPTABLE,
            stringify(wrap_graphql_error_response(vec![format!("{}", error)])).unwrap(),
        ),
    };
    let headers = cors_headers.into_iter().chain(once((
        header::CONTENT_TYPE,
        String::from("application/json"),
    )));
    let response = match response_etag {
        Some(etag) => {
            let headers = headers.chain(once((header::ETAG, etag)));
            create_http_response(status, headers, Some(body))
        }
        None => create_http_response(status, headers, Some(body)),
    };
    Ok::<Response<Body>, Infallible>(response)
}

fn parse_request_etag(req: &Request<Body>) -> Option<String> {
    req.headers()
        .get(header::IF_NONE_MATCH)
        .and_then(|header| header.to_str().map(|value| String::from(value)).ok())
}

fn parse_response_etag(response: &HttpResult) -> Option<String> {
    match &response.result {
        Err(_) => None,
        Ok(result) => Some(format!("\"{:x}\"", hash_object(&result))),
    }
}

async fn parse_graphql_request(
    req: Request<Body>,
    root: &Expression,
) -> Result<Expression, HttpResult> {
    let content_type = match req.headers().get(header::CONTENT_TYPE) {
        Some(value) => match value.to_str() {
            Ok(value) => Ok(String::from(value)),
            Err(_) => Err(String::from("Invalid Content-Type header")),
        },
        None => Err(String::from("Missing Content-Type header")),
    };
    let body = match hyper::body::to_bytes(req.into_body()).await {
        Ok(body) => match String::from_utf8(body.into_iter().collect()) {
            Ok(body) => Some(body),
            Err(_) => None,
        },
        Err(_) => None,
    }
    .ok_or_else(|| String::from("Invalid request body"));
    let result = content_type.and_then(|content_type| match content_type.as_str() {
        "application/graphql" => body.and_then(|body| {
            let variables = SerializedObjectTerm::new(Vec::new());
            parse_request_body_graphql(&body, &variables, &root)
        }),
        "application/json" => body.and_then(|body| parse_request_body_graphql_json(&body, &root)),
        _ => Err(String::from("Unsupported Content-Type header")),
    });
    result.or_else(|error| Err(HttpResult::error(StatusCode::BAD_REQUEST, error)))
}

fn parse_request_body_graphql(
    body: &str,
    variables: &SerializedObjectTerm,
    root: &Expression,
) -> Result<Expression, String> {
    let variables = variables
        .entries()
        .iter()
        .map(|(key, value)| (key.as_str(), value.deserialize()));
    let query = reflex_graphql::parse(body, variables)?;
    Ok(Expression::new(Term::Application(ApplicationTerm::new(
        query,
        vec![Expression::clone(root)],
    ))))
}

fn parse_request_body_graphql_json(body: &str, root: &Expression) -> Result<Expression, String> {
    match deserialize_graphql_operation(&body) {
        Err(error) => Err(error),
        Ok(message) => match message.operation_name() {
            Some("IntrospectionQuery") => Ok(create_introspection_query_response()),
            _ => parse_request_body_graphql(message.query(), message.variables(), &root),
        },
    }
}

struct HttpResult {
    status: StatusCode,
    result: Result<Expression, Vec<String>>,
}
impl HttpResult {
    fn success(status: StatusCode, result: Expression) -> Self {
        Self {
            status,
            result: Ok(result),
        }
    }
    fn error(status: StatusCode, message: String) -> Self {
        Self {
            status,
            result: Err(vec![message]),
        }
    }
    fn errors(status: StatusCode, messages: impl IntoIterator<Item = String>) -> Self {
        Self {
            status,
            result: Err(messages.into_iter().collect()),
        }
    }
}

fn format_http_response(result: SubscriptionResult) -> HttpResult {
    match result {
        Ok(result) => HttpResult::success(StatusCode::OK, result),
        Err(errors) => HttpResult::errors(StatusCode::OK, errors),
    }
}
