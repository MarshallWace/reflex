// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    convert::Infallible,
    hash::Hash,
    iter::{empty, once},
    sync::Arc,
};

use hyper::{header, Body, Request, Response, StatusCode};
use reflex::{
    compiler::{Compile, CompilerOptions, Program},
    core::{
        Applicable, Expression, ExpressionFactory, HeapAllocator, Reducible, Rewritable,
        StringValue,
    },
    hash::hash_object,
    stdlib::Stdlib,
};
use reflex_graphql::{
    create_graphql_error_response, create_graphql_success_response,
    create_introspection_query_response, create_json_error_object, deserialize_graphql_operation,
    parse_graphql_operation, sanitize_signal_errors, stdlib::Stdlib as GraphQlStdlib,
    GraphQlOperationPayload, GraphQlQueryTransform,
};
use reflex_json::{json_object, JsonValue};
use reflex_runtime::{
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, Runtime, StreamExt,
};

use crate::{
    create_http_response, get_cors_headers, graphql::compile_graphql_query,
    GraphQlHttpQueryTransform,
};

pub(crate) async fn handle_graphql_http_request<
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
>(
    req: Request<Body>,
    runtime: Arc<Runtime<T>>,
    program: &Program,
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
    compiler_options: CompilerOptions,
    transform: Arc<impl GraphQlHttpQueryTransform + Send + Sync + 'static>,
) -> Result<Response<Body>, Infallible>
where
    T::String: StringValue + Send + Sync,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
{
    let cors_headers = get_cors_headers(&req);
    let transform = match transform.factory(req.headers(), None) {
        Err((status, error)) => return Ok(create_http_response(status, cors_headers, Some(error))),
        Ok(transform) => transform,
    };
    let request_etag = parse_request_etag(&req);
    let root = factory.create_static_variable_term(0);
    let response = match parse_graphql_request(req, &root, factory, allocator, &transform).await {
        Err(response) => Ok(response),
        Ok(query) => {
            match compile_graphql_query(query, &program, &compiler_options, factory, allocator) {
                Err(error) => Err(error),
                Ok((program, entry_point)) => match runtime.subscribe(program, entry_point).await {
                    Err(error) => Err(error),
                    Ok(mut results) => match results.next().await {
                        None => Err(String::from("Empty result stream")),
                        Some(result) => match results.unsubscribe().await {
                            Ok(_) => Ok(format_http_response(result, factory)),
                            Err(error) => Err(error),
                        },
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
    let (status, body) = match response.result {
        Err(errors) => {
            let payload = create_graphql_error_response(errors);
            (response.status, payload.to_string())
        }
        Ok(result) => {
            let payload = create_graphql_success_response(result, factory, allocator);
            match reflex_json::stringify(&payload) {
                Ok(body) => (response.status, body),
                Err(error) => (
                    StatusCode::NOT_ACCEPTABLE,
                    create_graphql_error_response(vec![create_json_error_object(error)])
                        .to_string(),
                ),
            }
        }
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

fn parse_response_etag<T: Hash>(response: &HttpResult<T>) -> Option<String> {
    match &response.result {
        Err(_) => None,
        Ok(result) => Some(format!("\"{:x}\"", hash_object(result))),
    }
}

async fn parse_graphql_request<T: Expression>(
    req: Request<Body>,
    root: &T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    transform: &impl GraphQlQueryTransform,
) -> Result<T, HttpResult<T>>
where
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
{
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
            let operation = GraphQlOperationPayload::new(body, None, Some(empty()), Some(empty()));
            parse_graphql_operation(&operation, root, factory, allocator, transform)
        }),
        "application/json" => body.and_then(|body| {
            parse_request_body_graphql_json(&body, root, factory, allocator, transform)
        }),
        _ => Err(String::from("Unsupported Content-Type header")),
    });
    result.or_else(|error| Err(HttpResult::error(StatusCode::BAD_REQUEST, error)))
}

fn parse_request_body_graphql_json<T: Expression>(
    body: &str,
    root: &T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    transform: &impl GraphQlQueryTransform,
) -> Result<T, String>
where
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
{
    match deserialize_graphql_operation(&body) {
        Err(error) => Err(error),
        Ok(operation) => match operation.operation_name() {
            Some("IntrospectionQuery") => {
                Ok(create_introspection_query_response(factory, allocator))
            }
            _ => parse_graphql_operation(&operation, root, factory, allocator, transform),
        },
    }
}

struct HttpResult<T> {
    status: StatusCode,
    result: Result<T, Vec<JsonValue>>,
}
impl<T> HttpResult<T> {
    fn success(status: StatusCode, result: T) -> Self {
        Self {
            status,
            result: Ok(result),
        }
    }
    fn error(status: StatusCode, error: String) -> Self {
        Self {
            status,
            result: Err(vec![json_object(once((
                String::from("message"),
                JsonValue::String(error),
            )))]),
        }
    }
    fn errors(status: StatusCode, errors: impl IntoIterator<Item = JsonValue>) -> Self {
        Self {
            status,
            result: Err(errors.into_iter().collect()),
        }
    }
}

fn format_http_response<T: Expression>(
    result: T,
    factory: &impl ExpressionFactory<T>,
) -> HttpResult<T> {
    match factory.match_signal_term(&result) {
        Some(signal) => HttpResult::errors(StatusCode::OK, sanitize_signal_errors(signal)),
        None => HttpResult::success(StatusCode::OK, result),
    }
}
