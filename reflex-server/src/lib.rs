// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use graphql::{
    http::handle_graphql_http_request, playground::handle_playground_http_request,
    websocket::handle_graphql_ws_request,
};
use std::{convert::Infallible, future::Future, sync::Arc};

use hyper::{
    header::{self, HeaderName, HeaderValue},
    service::{service_fn, Service},
    Body, Method, Request, Response, StatusCode,
};

use reflex::core::Expression;
use reflex_js::stdlib::json_stringify_string;
use reflex_runtime::Runtime;

pub mod loaders {
    pub mod graphql;
}
mod query;
mod utils;
mod graphql {
    pub(crate) mod http;
    pub(crate) mod playground;
    pub(crate) mod protocol;
    pub(crate) mod websocket;
}

pub fn graphql_service(
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
            match req.method() {
                &Method::GET => {
                    if req.headers().contains_key(header::UPGRADE) {
                        handle_graphql_upgrade_request(req, store, root).await
                    } else {
                        handle_playground_http_request(req).await
                    }
                }
                &Method::POST => handle_graphql_http_request(req, store, root).await,
                _ => Ok(method_not_allowed()),
            }
        }
    })
}

async fn handle_graphql_upgrade_request(
    req: Request<Body>,
    store: Arc<Runtime>,
    root: Expression,
) -> Result<Response<Body>, Infallible> {
    Ok(if hyper_tungstenite::is_upgrade_request(&req) {
        match handle_graphql_ws_request(req, store, root).await {
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
            (header::CONNECTION, "upgrade"),
            (header::UPGRADE, "websocket"),
            (header::CONTENT_TYPE, "text/plain"),
        ],
        Some(String::from("Invalid protocol upgrade request")),
    )
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

fn create_http_response(
    status: StatusCode,
    headers: impl IntoIterator<Item = (HeaderName, &'static str)>,
    body: Option<String>,
) -> Response<Body> {
    let body = match body {
        Some(contents) => Body::from(contents),
        None => Body::empty(),
    };
    let mut res = Response::new(body);
    *res.status_mut() = status;
    for (key, value) in headers.into_iter() {
        res.headers_mut()
            .insert(key, HeaderValue::from_static(value));
    }
    res
}

fn method_not_allowed() -> Response<Body> {
    create_http_response(
        StatusCode::METHOD_NOT_ALLOWED,
        vec![(header::CONTENT_TYPE, "text/plain")],
        Some(String::from("Method not allowed")),
    )
}
