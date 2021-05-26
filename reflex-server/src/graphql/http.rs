// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use super::protocol::deserialize_graphql_request;
use crate::{
    create_http_response,
    query::query,
    utils::graphql::{parse_graphql_query, QueryTransform},
    wrap_graphql_error_response, wrap_graphql_success_response,
};
use hyper::{header, Body, Request, Response, StatusCode};
use reflex::{
    core::{Expression, SerializedTerm, Signal, SignalTerm, Term},
    serialize,
    stdlib::{signal::SignalType, value::StringValue},
};
use reflex_js::stdlib::json_stringify;
use reflex_runtime::{Runtime, SubscriptionResult};
use std::{convert::Infallible, sync::Arc};

pub(crate) async fn handle_graphql_http_request(
    req: Request<Body>,
    store: Arc<Runtime>,
    root: Expression,
) -> Result<Response<Body>, Infallible> {
    let response = match parse_graphql_request(req, &root).await {
        Err(response) => Ok(response),
        Ok((expression, transform)) => match store.subscribe(expression).await {
            Err(error) => Err(error),
            Ok(mut results) => match results.next().await {
                None => Err(String::from("Empty result stream")),
                Some(result) => match results.unsubscribe().await {
                    Ok(_) => Ok(format_http_response(result, transform)),
                    Err(error) => Err(error),
                },
            },
        },
    };
    let response = response
        .unwrap_or_else(|error| HttpResult::error(StatusCode::INTERNAL_SERVER_ERROR, error));
    let (status, body) = match response.result {
        Err(errors) => (response.status, wrap_graphql_error_response(errors)),
        Ok(result) => match json_stringify(result.value()) {
            Ok(body) => (response.status, wrap_graphql_success_response(body)),
            Err(error) => (
                StatusCode::NOT_ACCEPTABLE,
                wrap_graphql_error_response(vec![format!("Unable to serialize result: {}", error)]),
            ),
        },
    };
    Ok::<Response<Body>, Infallible>(create_http_response(
        status,
        vec![(header::CONTENT_TYPE, "application/json")],
        Some(body),
    ))
}

async fn parse_graphql_request(
    req: Request<Body>,
    root: &Expression,
) -> Result<(Expression, QueryTransform), HttpResult> {
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
        "application/graphql" => body.and_then(|body| parse_graphql_query_expression(&root, &body)),
        "application/json" => body
            .and_then(|body| deserialize_graphql_request(&body))
            .and_then(|message| match message.operation_name() {
                Some("IntrospectionQuery") => {
                    let expression = create_introspection_query_response();
                    let transform: QueryTransform =
                        Box::new(|value: &SerializedTerm| Ok(value.deserialize()));
                    Ok((expression, transform))
                }
                _ => parse_graphql_query_expression(&root, message.query()),
            }),
        _ => Err(String::from("Unsupported Content-Type header")),
    });
    result.or_else(|error| Err(HttpResult::error(StatusCode::BAD_REQUEST, error)))
}

fn parse_graphql_query_expression(
    root: &Expression,
    source: &str,
) -> Result<(Expression, QueryTransform), String> {
    match parse_graphql_query(source) {
        Ok((shape, transform)) => {
            let expression = query(Expression::clone(root), &shape);
            Ok((expression, transform))
        }
        Err(error) => Err(error),
    }
}

fn create_introspection_query_response() -> Expression {
    Expression::new(Term::Signal(SignalTerm::new(Signal::new(
        SignalType::Error,
        vec![SerializedTerm::string(StringValue::from(
            "Introspection query not yet implemented",
        ))],
    ))))
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
        Ok(result) => match serialize(result.value()) {
            Ok(result) => match transform(&result) {
                Ok(result) => HttpResult::success(StatusCode::OK, result),
                Err(error) => {
                    HttpResult::error(StatusCode::INTERNAL_SERVER_ERROR, format!("{}", error))
                }
            },
            Err(error) => HttpResult::error(
                StatusCode::NOT_ACCEPTABLE,
                format!("Invalid GraphQL result: {}", error),
            ),
        },
        Err(errors) => HttpResult::errors(StatusCode::OK, errors),
    }
}
