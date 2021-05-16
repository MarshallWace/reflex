// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{convert::Infallible, future::Future, sync::Arc};

use hyper::{
    header::HeaderValue,
    service::{service_fn, Service},
    Body, Request, Response, StatusCode,
};
use reflex::{
    core::{Expression, Term},
    query::{query, QueryShape},
};
use reflex_js::stdlib::{json_stringify, json_stringify_string};
use reflex_runtime::{Runtime, SubscriptionResult};
mod utils;
use utils::graphql::{parse_graphql_query, QueryTransform};

pub fn graphql_http_request_handler(
    store: Arc<Runtime>,
    root: Expression,
) -> impl Service<
    Request<Body>,
    Response = Response<Body>,
    Error = Infallible,
    Future = impl Future<Output = Result<Response<Body>, Infallible>> + Send + Sync,
> {
    service_fn(move |req| {
        let store = Arc::clone(&store);
        let root = Expression::clone(&root);
        async move {
            let response = match parse_graphql_request(req).await {
                Err(response) => Ok(response),
                Ok((shape, transform)) => {
                    let expression = query(Expression::clone(&root), &shape);
                    match store.subscribe(expression).await {
                        Err(error) => Err(error),
                        Ok(mut results) => match results.next().await {
                            None => Err(String::from("Empty result stream")),
                            Some(result) => match results.unsubscribe().await {
                                Ok(_) => Ok(format_http_response(result, transform)),
                                Err(error) => Err(error),
                            },
                        },
                    }
                }
            };
            let response = response.unwrap_or_else(|error| {
                HttpResult::error(StatusCode::INTERNAL_SERVER_ERROR, error)
            });
            Ok::<Response<Body>, Infallible>({
                let (status, body) = match response.result {
                    Err(errors) => (response.status, wrap_graphql_error_response(errors)),
                    Ok(result) => match json_stringify(result.value()) {
                        Ok(body) => (response.status, wrap_graphql_success_response(body)),
                        Err(error) => (
                            StatusCode::NOT_ACCEPTABLE,
                            wrap_graphql_error_response(vec![format!(
                                "Unable to serialize result: {}",
                                error
                            )]),
                        ),
                    },
                };
                let mut response = Response::new(Body::from(body));
                *response.status_mut() = status;
                response
                    .headers_mut()
                    .insert("Content-Type", HeaderValue::from_static("application/json"));
                response
            })
        }
    })
}

fn wrap_graphql_success_response(json_output: String) -> String {
    format!("{{\"data\":{}}}", json_output)
}

fn wrap_graphql_error_response(errors: impl IntoIterator<Item = String>) -> String {
    format!(
        "{{\"errors\":[{}]}}",
        errors
            .into_iter()
            .map(|message| format!("{{\"message\":{}}}", json_stringify_string(&message)))
            .collect::<Vec<_>>()
            .join(",")
    )
}

async fn parse_graphql_request(
    req: Request<Body>,
) -> Result<(QueryShape, QueryTransform), HttpResult> {
    let body = match hyper::body::to_bytes(req.into_body()).await {
        Ok(body) => match String::from_utf8(body.into_iter().collect()) {
            Ok(body) => match parse_graphql_query(&body) {
                Ok((shape, transform)) => Ok((shape, transform)),
                Err(error) => Err(Some(error)),
            },
            Err(_) => Err(None),
        },
        Err(_) => Err(None),
    };
    body.or_else(|error| {
        Err(HttpResult::error(
            StatusCode::BAD_REQUEST,
            error.unwrap_or_else(|| String::from("Bad request")),
        ))
    })
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

fn format_http_response(result: SubscriptionResult, transform: QueryTransform) -> HttpResult {
    match result {
        Ok(result) => match result.value() {
            Term::Value(result) => match transform(result) {
                Ok(result) => HttpResult::success(StatusCode::OK, result),
                Err(error) => {
                    HttpResult::error(StatusCode::INTERNAL_SERVER_ERROR, format!("{}", error))
                }
            },
            result => HttpResult::error(
                StatusCode::NOT_ACCEPTABLE,
                format!("Invalid result type: {}", result),
            ),
        },
        Err(errors) => HttpResult::errors(StatusCode::OK, errors),
    }
}
