// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{convert::Infallible, future::Future, iter::once, sync::Arc};

use hyper::{
    header::{self, HeaderName, HeaderValue, ACCESS_CONTROL_ALLOW_CREDENTIALS},
    service::{service_fn, Service},
    Body, HeaderMap, Method, Request, Response, StatusCode,
};
use reflex::{
    compiler::{Compile, CompilerOptions, InstructionPointer, Program},
    core::{Applicable, Reducible, Rewritable},
    stdlib::Stdlib,
};
use reflex_graphql::{
    stdlib::Stdlib as GraphQlStdlib, AsyncGraphQlQueryTransform, NoopGraphQlQueryTransform,
};
use reflex_json::JsonValue;
use reflex_runtime::{AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, Runtime};

pub mod builtins;
pub mod cli {
    pub mod execute_query;
    pub mod profile_query;
    pub mod reflex_server;
}

mod graphql;
pub use graphql::compile_graphql_query;
use graphql::{
    http::handle_graphql_http_request, playground::handle_playground_http_request,
    websocket::handle_graphql_ws_request,
};

pub type RequestHeaders = HeaderMap<HeaderValue>;

pub trait GraphQlHttpQueryTransform: Send + Sync + 'static {
    type Transform: AsyncGraphQlQueryTransform;
    fn factory(
        &self,
        headers: &RequestHeaders,
        connection_params: Option<&JsonValue>,
    ) -> Result<Self::Transform, (StatusCode, String)>;
}
impl<TFactory, TTransform> GraphQlHttpQueryTransform for TFactory
where
    TFactory: Fn(&RequestHeaders, Option<&JsonValue>) -> Result<TTransform, (StatusCode, String)>
        + Send
        + Sync
        + 'static,
    TTransform: AsyncGraphQlQueryTransform,
{
    type Transform = TTransform;
    fn factory(
        &self,
        headers: &RequestHeaders,
        connection_params: Option<&JsonValue>,
    ) -> Result<Self::Transform, (StatusCode, String)> {
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
    type Transform = NoopGraphQlQueryTransform;
    fn factory(
        &self,
        _headers: &RequestHeaders,
        _connection_params: Option<&JsonValue>,
    ) -> Result<Self::Transform, (StatusCode, String)> {
        Ok(NoopGraphQlQueryTransform::default())
    }
}

pub fn graphql_service<T>(
    runtime: Arc<Runtime<T>>,
    graph_root: Arc<(Program, InstructionPointer)>,
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
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send + Sync,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
{
    service_fn({
        let factory = factory.clone();
        let allocator = allocator.clone();
        move |req| {
            let runtime = Arc::clone(&runtime);
            let graph_root = Arc::clone(&graph_root);
            let transform = Arc::clone(&transform);
            let factory = factory.clone();
            let allocator = allocator.clone();
            async move {
                match req.method() {
                    &Method::POST => {
                        handle_graphql_http_request(
                            req,
                            runtime,
                            &graph_root,
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
                                graph_root,
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

async fn handle_graphql_upgrade_request<T>(
    req: Request<Body>,
    runtime: Arc<Runtime<T>>,
    graph_root: Arc<(Program, InstructionPointer)>,
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
    compiler_options: CompilerOptions,
    transform: Arc<impl GraphQlHttpQueryTransform + Send + Sync + 'static>,
) -> Result<Response<Body>, Infallible>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send + Sync,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
{
    Ok(if hyper_tungstenite::is_upgrade_request(&req) {
        match handle_graphql_ws_request(
            req,
            runtime,
            graph_root,
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

#[cfg(test)]
mod tests {
    use std::{hash::Hash, iter::empty};

    use reflex::{
        allocator::DefaultAllocator,
        compiler::{Compile, Compiler, CompilerMode, CompilerOptions, InstructionPointer, Program},
        core::{
            Applicable, Expression, ExpressionFactory, HeapAllocator, Reducible, Rewritable, Signal,
        },
        interpreter::InterpreterOptions,
        lang::{create_struct, SharedTermFactory, ValueTerm},
        stdlib::Stdlib,
    };
    use reflex_graphql::{stdlib::Stdlib as GraphQlStdlib, NoopGraphQlQueryTransform};
    use reflex_js::{create_js_env, stdlib::Stdlib as JsStdlib};
    use reflex_runtime::{
        AsyncExpression, Runtime, RuntimeCache, RuntimeState, SignalHandler, SignalHandlerResult,
        SignalHelpers, Stream, StreamExt, StreamSubscription,
    };

    use crate::{builtins::ServerBuiltins, compile_graphql_query};

    fn noop_signal_handler<T: AsyncExpression + Compile<T>>(
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
    ) -> impl SignalHandler<T>
    where
        T::String: Send + Sync,
    {
        |_signal_type: &str,
         _signals: &[&Signal<T>],
         _signal_helpers: &SignalHelpers<T>|
         -> SignalHandlerResult<T> { None }
    }

    fn compile_js_graph_root<
        T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    >(
        input: &str,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Result<(Program, InstructionPointer), String>
    where
        T::Builtin: From<Stdlib> + From<JsStdlib>,
    {
        let root = reflex_js::parse(
            input,
            &create_js_env(factory, allocator),
            factory,
            allocator,
        )?;
        Compiler::new(CompilerOptions::default(), None)
            .compile(&root, CompilerMode::Function, factory, allocator)
            .map(|program| (program, InstructionPointer::default()))
    }

    async fn subscribe_query<'a, 'b, T: 'a>(
        query: &str,
        variables: impl IntoIterator<Item = (&'b str, T)>,
        operation_id: &impl Hash,
        graph_root: &(Program, InstructionPointer),
        runtime: &'a Runtime<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Result<StreamSubscription<'a, T, impl Stream<Item = T>>, String>
    where
        T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
        T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
    {
        let query = reflex_graphql::parse(
            query,
            variables,
            factory,
            allocator,
            &NoopGraphQlQueryTransform {},
        )?;
        let (program, entry_point) = compile_graphql_query(
            query,
            operation_id,
            graph_root.clone(),
            &CompilerOptions::default(),
            factory,
            allocator,
        )?;
        runtime.subscribe(program, entry_point).await
    }

    #[tokio::test]
    async fn simultaneous_subscriptions() {
        let factory = SharedTermFactory::<ServerBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let signal_handler = noop_signal_handler(&factory, &allocator);
        let runtime = Runtime::new(
            RuntimeState::default(),
            &signal_handler,
            RuntimeCache::default(),
            &factory,
            &allocator,
            InterpreterOptions::default(),
            CompilerOptions::default(),
        );

        let graph_root = compile_js_graph_root(
            "
            (_) => ({
                query: ({
                    foo: {
                        value: true,
                    },
                    bar: {
                        value: false
                    },
                }),
                mutation: () => null,
                subscription: () => null,
            })
        ",
            &factory,
            &allocator,
        )
        .unwrap();

        let mut subscription1 = subscribe_query(
            "{ foo { value } }",
            empty(),
            &0,
            &graph_root,
            &runtime,
            &factory,
            &allocator,
        )
        .await
        .unwrap()
        .into_stream();

        assert_eq!(
            subscription1.next().await,
            Some(create_struct(
                [(
                    String::from("foo"),
                    create_struct(
                        [(
                            String::from("value"),
                            factory.create_value_term(ValueTerm::Boolean(true)),
                        )],
                        &factory,
                        &allocator
                    ),
                )],
                &factory,
                &allocator
            ))
        );

        let mut subscription2 = subscribe_query(
            "{ bar { value } }",
            empty(),
            &1,
            &graph_root,
            &runtime,
            &factory,
            &allocator,
        )
        .await
        .unwrap()
        .into_stream();

        assert_eq!(
            subscription2.next().await,
            Some(create_struct(
                [(
                    String::from("bar"),
                    create_struct(
                        [(
                            String::from("value"),
                            factory.create_value_term(ValueTerm::Boolean(false)),
                        )],
                        &factory,
                        &allocator
                    ),
                )],
                &factory,
                &allocator
            ))
        );
    }
}
