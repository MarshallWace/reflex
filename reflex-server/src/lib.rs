// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use graphql::{
    http::handle_graphql_http_request, playground::handle_playground_http_request,
    websocket::handle_graphql_ws_request,
};
use std::{convert::Infallible, future::Future, iter::once, sync::Arc};

use hyper::{
    header::{self, HeaderName, HeaderValue, ACCESS_CONTROL_ALLOW_CREDENTIALS},
    service::{service_fn, Service},
    Body, Method, Request, Response, StatusCode,
};

use reflex::{
    compiler::{Compile, CompilerOptions},
    core::{Applicable, Reducible, Rewritable, StringValue},
    interpreter::Execute,
};
use reflex_runtime::{AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, Runtime};

pub mod cli;

mod graphql {
    pub(crate) mod http;
    pub(crate) mod playground;
    pub(crate) mod websocket;
}

pub fn graphql_service<
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T> + Execute<T>,
>(
    store: Arc<Runtime<T>>,
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
    compiler_options: CompilerOptions,
) -> impl Service<
    Request<Body>,
    Response = Response<Body>,
    Error = Infallible,
    Future = impl Future<Output = Result<Response<Body>, Infallible>> + Send + Sync,
>
where
    T::String: StringValue + Send + Sync,
{
    service_fn({
        let factory = factory.clone();
        let allocator = allocator.clone();
        move |req| {
            let store = Arc::clone(&store);
            let factory = factory.clone();
            let allocator = allocator.clone();
            let root = factory.create_application_term(
                factory.create_static_variable_term(0),
                allocator.create_empty_list(),
            );
            async move {
                match req.method() {
                    &Method::POST => {
                        handle_graphql_http_request(
                            req,
                            store,
                            &root,
                            &factory,
                            &allocator,
                            compiler_options,
                        )
                        .await
                    }
                    &Method::GET => {
                        if req.headers().contains_key(header::UPGRADE) {
                            handle_graphql_upgrade_request(
                                req,
                                store,
                                &root,
                                &factory,
                                &allocator,
                                compiler_options,
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
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T> + Execute<T>,
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
    Ok(if hyper_tungstenite::is_upgrade_request(&req) {
        match handle_graphql_ws_request(req, store, root, factory, allocator, compiler_options)
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

fn method_not_allowed() -> Response<Body> {
    create_http_response(
        StatusCode::METHOD_NOT_ALLOWED,
        vec![(header::CONTENT_TYPE, String::from("text/plain"))],
        Some(String::from("Method not allowed")),
    )
}
