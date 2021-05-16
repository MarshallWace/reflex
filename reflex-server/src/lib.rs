// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{convert::Infallible, net::SocketAddr, sync::Arc};

use hyper::{
    header::HeaderValue,
    service::{make_service_fn, service_fn},
    Body, Request, Response, Server, StatusCode,
};
use reflex::{
    core::{Expression, Term},
    query::{query, QueryShape},
};
use reflex_js::stdlib::json_stringify;
use reflex_runtime::{Runtime, SignalHandler, SubscriptionResult};
mod utils;
use utils::graphql::{parse_graphql_query, QueryTransform};

pub async fn run(
    root: Expression,
    signal_handlers: impl IntoIterator<Item = (&'static str, SignalHandler)>,
    address: &SocketAddr,
) -> Result<(), String> {
    // TODO: Establish sensible defaults for channel buffer sizes
    let command_buffer_size = 32;
    let result_buffer_size = 32;

    let runtime = Runtime::new(signal_handlers, command_buffer_size, result_buffer_size);
    let runtime = Arc::new(runtime);

    let server = Server::bind(&address).serve(make_service_fn(|_conn| {
        let runtime = Arc::clone(&runtime);
        let root = Expression::clone(&root);
        async move {
            Ok::<_, Infallible>(service_fn(move |req| {
                let runtime = Arc::clone(&runtime);
                let root = Expression::clone(&root);
                async move {
                    let response = match parse_graphql_request(req).await {
                        Err(response) => Ok(response),
                        Ok((shape, transform)) => {
                            let expression = query(Expression::clone(&root), &shape);
                            match runtime.subscribe(expression).await {
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
                    let response = match response {
                        Ok(response) => response,
                        Err(error) => HttpResponse::new(StatusCode::INTERNAL_SERVER_ERROR, error),
                    };
                    Ok::<Response<Body>, Infallible>({
                        let status = response.status;
                        let mut response = Response::new(Body::from(response.body));
                        if status == 200 {
                            response.headers_mut().insert(
                                "Content-Type",
                                HeaderValue::from_static("application/json"),
                            );
                        }
                        *response.status_mut() = status;
                        response
                    })
                }
            }))
        }
    }));
    match server.await {
        Err(error) => Err(format!("{}", error)),
        Ok(()) => Ok(()),
    }
}

async fn parse_graphql_request(
    req: Request<Body>,
) -> Result<(QueryShape, QueryTransform), HttpResponse> {
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
        Err(HttpResponse::new(
            StatusCode::BAD_REQUEST,
            error.unwrap_or_else(|| String::from("Bad request")),
        ))
    })
}

struct HttpResponse {
    status: StatusCode,
    body: String,
}
impl HttpResponse {
    fn new(status: StatusCode, body: String) -> Self {
        Self { status, body }
    }
}

fn format_http_response(result: SubscriptionResult, transform: QueryTransform) -> HttpResponse {
    match result {
        Ok(result) => match result.value() {
            Term::Value(result) => match transform(result) {
                Ok(result) => match json_stringify(result.value()) {
                    Ok(output) => HttpResponse::new(StatusCode::OK, output),
                    Err(error) => HttpResponse::new(
                        StatusCode::BAD_REQUEST,
                        format!("Unable to serialize result: {}", error),
                    ),
                },
                Err(error) => {
                    HttpResponse::new(StatusCode::INTERNAL_SERVER_ERROR, format!("{}", error))
                }
            },
            result => HttpResponse::new(
                StatusCode::BAD_REQUEST,
                format!("Invalid result type: {}", result),
            ),
        },
        Err(errors) => HttpResponse::new(
            StatusCode::INTERNAL_SERVER_ERROR,
            format!(
                "{}",
                errors
                    .iter()
                    .map(|error| format!("Error: {}", error))
                    .collect::<Vec<_>>()
                    .join("\n")
            ),
        ),
    }
}
