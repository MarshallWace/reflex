// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{convert::Infallible, net::SocketAddr, sync::Arc};

use hyper::{
    service::{make_service_fn, service_fn},
    Body, Request, Response, Server, StatusCode,
};
use reflex::core::Expression;
use reflex_js::{parse, stdlib::json_stringify, Env};
use reflex_runtime::{Runtime, SignalHandler, SubscriptionResult};

pub async fn run(
    env: Env,
    signal_handlers: impl IntoIterator<Item = (&'static str, SignalHandler)>,
    address: &SocketAddr,
) -> Result<(), String> {
    let env = Arc::new(env);

    // TODO: Establish sensible defaults for channel buffer sizes
    let command_buffer_size = 32;
    let result_buffer_size = 32;

    let runtime = Runtime::new(signal_handlers, command_buffer_size, result_buffer_size);
    let runtime = Arc::new(runtime);

    let server = Server::bind(&address).serve(make_service_fn(|_conn| {
        let runtime = Arc::clone(&runtime);
        let env = Arc::clone(&env);
        async move {
            Ok::<_, Infallible>(service_fn(move |req| {
                let runtime = Arc::clone(&runtime);
                let env = Arc::clone(&env);
                async move {
                    let response = match parse_request(req).await {
                        Err(response) => Ok(response),
                        Ok(source) => match parse_expression(source, &env) {
                            Err(response) => Ok(response),
                            Ok(expression) => match runtime.subscribe(expression).await {
                                Err(error) => Err(error),
                                Ok(mut results) => match results.next().await {
                                    None => Err(String::from("Empty result stream")),
                                    Some(result) => match results.unsubscribe().await {
                                        Ok(_) => Ok(format_http_response(result)),
                                        Err(error) => Err(error),
                                    },
                                },
                            },
                        },
                    };
                    let response = match response {
                        Ok(response) => response,
                        Err(error) => HttpResponse::new(StatusCode::INTERNAL_SERVER_ERROR, error),
                    };
                    Ok::<Response<Body>, Infallible>({
                        let status = response.status;
                        let mut response = Response::new(Body::from(response.body));
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

async fn parse_request(req: Request<Body>) -> Result<String, HttpResponse> {
    let body = match hyper::body::to_bytes(req.into_body()).await {
        Ok(body) => match String::from_utf8(body.into_iter().collect()) {
            Ok(body) => Ok(body),
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

fn parse_expression(source: String, env: &Env) -> Result<Expression, HttpResponse> {
    match parse(&source, &env) {
        Err(error) => Err(HttpResponse::new(StatusCode::BAD_REQUEST, error)),
        Ok(expression) => Ok(expression),
    }
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

fn format_http_response(result: SubscriptionResult) -> HttpResponse {
    match result {
        Ok(result) => match json_stringify(result.value()) {
            Ok(output) => HttpResponse::new(StatusCode::OK, output),
            Err(error) => HttpResponse::new(
                StatusCode::BAD_REQUEST,
                format!("Invalid result: {}", error),
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
