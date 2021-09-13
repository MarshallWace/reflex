// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{create_http_response, get_cors_headers};
use hyper::{header, Body, Request, Response, StatusCode};
use reflex::{
    compiler::{Compile, Compiler, CompilerMode, CompilerOptions, Instruction, InstructionPointer},
    core::{
        Applicable, Expression, ExpressionFactory, HeapAllocator, Reducible, Rewritable,
        StringValue,
    },
    hash::hash_object,
    lang::{create_struct, ValueTerm},
};
use reflex_graphql::{
    create_introspection_query_response, deserialize_graphql_operation, parse_graphql_operation,
    wrap_graphql_error_response, wrap_graphql_success_response, GraphQlOperationPayload,
};
use reflex_runtime::{
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, Runtime, StreamExt,
    SubscriptionResult,
};
use std::{
    convert::Infallible,
    hash::Hash,
    iter::{empty, once},
    sync::Arc,
};

pub(crate) async fn handle_graphql_http_request<
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
>(
    req: Request<Body>,
    store: Arc<Runtime<T>>,
    root: &T,
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
    compiler_options: CompilerOptions,
) -> Result<Response<Body>, Infallible>
where
    T::String: StringValue + Send + Sync,
{
    let cors_headers = get_cors_headers(&req);
    let request_etag = parse_request_etag(&req);
    let response = match parse_graphql_request(req, root, factory, allocator).await {
        Err(response) => Ok(response),
        Ok(query) => {
            let mut prelude = store.program().clone();
            prelude.push(Instruction::PushFunction {
                target: InstructionPointer::default(),
            });
            match Compiler::new(compiler_options, Some(prelude)).compile(
                &query,
                CompilerMode::Expression,
                false,
                empty(),
                factory,
                allocator,
            ) {
                Err(error) => Err(error),
                Ok(compiled) => {
                    // TODO: Error if runtime expression depends on unrecognized native functions
                    let (program, _, _) = compiled.into_parts();
                    let entry_point = InstructionPointer::new(store.program().len());
                    match store
                        .subscribe(program, entry_point, factory, allocator)
                        .await
                    {
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
        Ok(result) => wrap_graphql_success_response(result, factory, allocator),
        Err(errors) => wrap_graphql_error_response(
            errors
                .into_iter()
                .map(|error| create_graphql_error_object(error, factory, allocator)),
            factory,
            allocator,
        ),
    };
    let (status, body) = match reflex_json::stringify(&payload) {
        Ok(body) => (response.status, body),
        Err(error) => (
            StatusCode::NOT_ACCEPTABLE,
            reflex_json::stringify(&wrap_graphql_error_response(
                vec![create_graphql_error_object(error, factory, allocator)],
                factory,
                allocator,
            ))
            .unwrap(),
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
) -> Result<T, HttpResult<T>> {
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
            parse_graphql_operation(&operation, root, factory, allocator)
        }),
        "application/json" => {
            body.and_then(|body| parse_request_body_graphql_json(&body, root, factory, allocator))
        }
        _ => Err(String::from("Unsupported Content-Type header")),
    });
    result.or_else(|error| Err(HttpResult::error(StatusCode::BAD_REQUEST, error)))
}

fn parse_request_body_graphql_json<T: Expression>(
    body: &str,
    root: &T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, String> {
    match deserialize_graphql_operation(&body) {
        Err(error) => Err(error),
        Ok(operation) => match operation.operation_name() {
            Some("IntrospectionQuery") => {
                Ok(create_introspection_query_response(factory, allocator))
            }
            _ => parse_graphql_operation(&operation, root, factory, allocator),
        },
    }
}

struct HttpResult<T> {
    status: StatusCode,
    result: Result<T, Vec<String>>,
}
impl<T> HttpResult<T> {
    fn success(status: StatusCode, result: T) -> Self {
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

fn format_http_response<T>(result: SubscriptionResult<T>) -> HttpResult<T> {
    match result {
        Ok(result) => HttpResult::success(StatusCode::OK, result),
        Err(errors) => HttpResult::errors(StatusCode::OK, errors),
    }
}

fn create_graphql_error_object<T: Expression>(
    message: String,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    create_struct(
        once((
            String::from("message"),
            factory.create_value_term(ValueTerm::String(allocator.create_string(message))),
        )),
        factory,
        allocator,
    )
}
