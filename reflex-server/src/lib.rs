// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{convert::Infallible, future::Future, iter::once, sync::Arc};

use hyper::{
    header::{self, HeaderName, HeaderValue, ACCESS_CONTROL_ALLOW_CREDENTIALS},
    service::{service_fn, Service},
    Body, HeaderMap, Method, Request, Response, StatusCode,
};
use reflex::{
    compiler::{Compile, CompilerOptions, Program},
    core::{Applicable, Reducible, Rewritable, StringValue},
    stdlib::Stdlib,
};
use reflex_graphql::{
    stdlib::Stdlib as GraphQlStdlib, AsyncGraphQlQueryTransform, NoopGraphQlQueryTransform,
};
use reflex_json::JsonValue;
use reflex_runtime::{AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, Runtime};

pub mod builtins;
pub mod cli;

mod graphql;
use graphql::{
    http::handle_graphql_http_request, playground::handle_playground_http_request,
    websocket::handle_graphql_ws_request,
};

pub type RequestHeaders = HeaderMap<HeaderValue>;

pub trait GraphQlHttpQueryTransform: Send + Sync + 'static {
    type T: AsyncGraphQlQueryTransform;
    fn factory(
        &self,
        headers: &RequestHeaders,
        connection_params: Option<&JsonValue>,
    ) -> Result<Self::T, (StatusCode, String)>;
}
impl<T, T2> GraphQlHttpQueryTransform for T
where
    T: Fn(&RequestHeaders, Option<&JsonValue>) -> Result<T2, (StatusCode, String)>
        + Send
        + Sync
        + 'static,
    T2: AsyncGraphQlQueryTransform,
{
    type T = T2;
    fn factory(
        &self,
        headers: &RequestHeaders,
        connection_params: Option<&JsonValue>,
    ) -> Result<Self::T, (StatusCode, String)> {
        self(headers, connection_params)
    }
}

pub struct NoopGraphQlHttpQueryTransform {}
impl Default for NoopGraphQlHttpQueryTransform {
    fn default() -> Self {
        Self {}
    }
}
impl GraphQlHttpQueryTransform for NoopGraphQlHttpQueryTransform {
    type T = NoopGraphQlQueryTransform;
    fn factory(
        &self,
        _headers: &RequestHeaders,
        _connection_params: Option<&JsonValue>,
    ) -> Result<Self::T, (StatusCode, String)> {
        Ok(NoopGraphQlQueryTransform::default())
    }
}

pub fn graphql_service<
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
>(
    runtime: Arc<Runtime<T>>,
    program: Arc<Program>,
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
    compiler_options: CompilerOptions,
    transform: Arc<impl GraphQlHttpQueryTransform + Send + Sync + 'static>,
) -> impl Service<
    Request<Body>,
    Response = Response<Body>,
    Error = Infallible,
    Future = impl Future<Output = Result<Response<Body>, Infallible>> + Send + Sync,
>
where
    T::String: StringValue + Send + Sync,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
{
    service_fn({
        let factory = factory.clone();
        let allocator = allocator.clone();
        move |req| {
            let runtime = Arc::clone(&runtime);
            let program = Arc::clone(&program);
            let transform = Arc::clone(&transform);
            let factory = factory.clone();
            let allocator = allocator.clone();
            async move {
                match req.method() {
                    &Method::POST => {
                        handle_graphql_http_request(
                            req,
                            runtime,
                            &program,
                            &factory,
                            &allocator,
                            compiler_options,
                            transform,
                        )
                        .await
                    }
                    &Method::GET => {
                        if req.headers().contains_key(header::UPGRADE) {
                            handle_graphql_upgrade_request(
                                req,
                                runtime,
                                program,
                                &factory,
                                &allocator,
                                compiler_options,
                                transform,
                            )
                            .await
                        } else {
                            handle_playground_http_request(req).await
                        }
                    }
                    &Method::OPTIONS => handle_cors_preflight_request(req),
                    _ => Ok(method_not_allowed()),
                }
            }
        }
    })
}

fn handle_cors_preflight_request(req: Request<Body>) -> Result<Response<Body>, Infallible> {
    Ok(create_http_response(
        StatusCode::NO_CONTENT,
        get_cors_headers(&req),
        None,
    ))
}

async fn handle_graphql_upgrade_request<
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
>(
    req: Request<Body>,
    runtime: Arc<Runtime<T>>,
    program: Arc<Program>,
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
    compiler_options: CompilerOptions,
    transform: Arc<impl GraphQlHttpQueryTransform + Send + Sync + 'static>,
) -> Result<Response<Body>, Infallible>
where
    T::String: StringValue + Send + Sync,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
{
    Ok(if hyper_tungstenite::is_upgrade_request(&req) {
        match handle_graphql_ws_request(
            req,
            runtime,
            program,
            factory,
            allocator,
            compiler_options,
            transform,
        )
        .await
        {
            Ok(response) => response,
            Err(_) => create_invalid_websocket_upgrade_response(),
        }
    } else {
        create_invalid_websocket_upgrade_response()
    })
}

fn create_invalid_websocket_upgrade_response() -> Response<Body> {
    create_http_response(
        StatusCode::UPGRADE_REQUIRED,
        vec![
            (header::CONNECTION, String::from("upgrade")),
            (header::UPGRADE, String::from("websocket")),
            (header::CONTENT_TYPE, String::from("text/plain")),
        ],
        Some(String::from("Invalid protocol upgrade request")),
    )
}

fn get_cors_headers(req: &Request<Body>) -> impl IntoIterator<Item = (HeaderName, String)> {
    once((
        header::ACCESS_CONTROL_ALLOW_METHODS,
        String::from("OPTIONS, GET, POST"),
    ))
    .chain(once((
        ACCESS_CONTROL_ALLOW_CREDENTIALS,
        String::from("true"),
    )))
    .chain(once((
        header::ACCESS_CONTROL_ALLOW_ORIGIN,
        req.headers()
            .get(header::ORIGIN)
            .and_then(|header| header.to_str().ok().map(String::from))
            .unwrap_or_else(|| String::from("*")),
    )))
    .chain(once((
        header::ACCESS_CONTROL_ALLOW_HEADERS,
        req.headers()
            .get(header::ACCESS_CONTROL_REQUEST_HEADERS)
            .and_then(|header| header.to_str().ok().map(String::from))
            .unwrap_or_else(|| String::from("*")),
    )))
}

fn create_http_response(
    status: StatusCode,
    headers: impl IntoIterator<Item = (HeaderName, String)>,
    body: Option<String>,
) -> Response<Body> {
    let body = match body {
        Some(contents) => Body::from(contents),
        None => Body::empty(),
    };
    let mut res = Response::new(body);
    *res.status_mut() = status;
    for (key, value) in headers.into_iter() {
        if let Ok(value) = HeaderValue::from_str(&value) {
            res.headers_mut().insert(key, value);
        }
    }
    res
}

fn create_http_error_response(status: StatusCode, message: String) -> Response<Body> {
    create_http_response(
        status,
        vec![(header::CONTENT_TYPE, String::from("text/plain"))],
        Some(message),
    )
}

fn method_not_allowed() -> Response<Body> {
    create_http_error_response(
        StatusCode::METHOD_NOT_ALLOWED,
        String::from("Method not allowed"),
    )
}
