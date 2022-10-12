// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::convert::Infallible;
use std::fs::File;
use std::future;
use std::io::Write;
use std::iter::empty;
use std::net::SocketAddr;
use std::path::PathBuf;
use std::sync::Arc;

use http::HeaderMap;
use hyper::server::conn::AddrStream;
use hyper::service::make_service_fn;
use hyper::Server;
use reflex_server::opentelemetry::trace::noop::NoopTracer;
use tokio::sync::oneshot;

use rand::distributions::Alphanumeric;
use rand::{thread_rng, Rng};
use reflex_cli::syntax::js::{compile_js_entry_point, default_js_loaders};
use reflex_dispatcher::SchedulerMiddleware;
use reflex_graphql::{GraphQlOperation, NoopGraphQlQueryTransform};
use reflex_handlers::actor::graphql::{GraphQlHandler, GraphQlHandlerMetricNames};
use reflex_interpreter::compiler::CompilerOptions;
use reflex_interpreter::InterpreterOptions;
use reflex_json::JsonValue;
use reflex_lang::allocator::DefaultAllocator;
use reflex_lang::SharedTermFactory;
use reflex_server::action::ServerCliAction;
use reflex_server::builtins::ServerBuiltins;
use reflex_server::cli::reflex_server::get_graphql_query_label;
use reflex_server::imports::server_imports;
use reflex_server::{graphql_service, GraphQlWebServer, GraphQlWebServerMetricNames};
use reflex_utils::reconnect::NoopReconnectTimeout;

fn random_string() -> String {
    let mut rng = thread_rng();
    let chars: String = std::iter::repeat(())
        .map(|()| rng.sample(Alphanumeric))
        .map(char::from)
        .take(7)
        .collect();
    chars.to_lowercase()
}

fn make_temp_file_containing(input: &str) -> PathBuf {
    let path = PathBuf::from(format!(
        "{}/{}",
        env!("CARGO_TARGET_TMPDIR"),
        random_string()
    ));
    let mut f = File::create(&path).unwrap();
    f.write(input.as_bytes()).unwrap();
    path
}

pub fn serve_graphql(input: &str) -> (SocketAddr, oneshot::Sender<()>) {
    let js_file = make_temp_file_containing(input);
    let middleware = SchedulerMiddleware::noop();
    let allocator = DefaultAllocator::default();
    let factory = SharedTermFactory::<ServerBuiltins>::default();

    let https_client = hyper::Client::builder().build(hyper_tls::HttpsConnector::new());
    let actor = GraphQlHandler::new(
        https_client,
        factory.clone(),
        allocator.clone(),
        NoopReconnectTimeout {},
        GraphQlHandlerMetricNames::default(),
    );
    let compiler_options = CompilerOptions::default();
    let interpreter_options = InterpreterOptions::default();
    let module_loader = Some(default_js_loaders(
        server_imports(&factory, &allocator),
        &factory,
        &allocator,
    ));
    let graph_root = {
        // let js_env = create_js_env(&factory, &allocator);
        compile_js_entry_point(
            &js_file,
            Some(empty()),
            module_loader,
            &compiler_options,
            &factory,
            &allocator,
        )
        .unwrap()
        // let root = parse(input, &js_env, &factory, &allocator).unwrap();
        // let program = Compiler::new(compiler_options, None)
        //     .compile(&root, CompilerMode::Function, &factory, &allocator)
        //     .unwrap();
        // (program, InstructionPointer::default())
    };

    let app: GraphQlWebServer<ServerCliAction<_>> = GraphQlWebServer::new(
        graph_root,
        None,
        actor,
        middleware,
        compiler_options,
        interpreter_options,
        factory,
        allocator,
        NoopGraphQlQueryTransform,
        NoopGraphQlQueryTransform,
        GraphQlWebServerMetricNames::default(),
        get_graphql_query_label,
        get_http_query_metric_labels,
        get_websocket_connection_metric_labels,
        get_websocket_operation_metric_labels,
        NoopTracer::default(),
    )
    .unwrap();
    let socket_addr: SocketAddr = "127.0.0.1:0".parse().unwrap();
    let service = make_service_fn({
        let app = Arc::new(app);
        move |_socket: &AddrStream| {
            let app = Arc::clone(&app);
            let service = graphql_service(app);
            future::ready(Ok::<_, Infallible>(service))
        }
    });
    let server = Server::bind(&socket_addr).serve(service);
    let (tx, rx) = oneshot::channel();
    let addr = server.local_addr().clone();
    tokio::task::spawn(async move {
        server
            .with_graceful_shutdown(async { rx.await.ok().unwrap() })
            .await
    });
    (addr, tx)
}

fn get_http_query_metric_labels(
    operation: &GraphQlOperation,
    _headers: &HeaderMap,
) -> Vec<(String, String)> {
    vec![(
        String::from("operation_name"),
        String::from(operation.operation_name().unwrap_or("<null>")),
    )]
}

fn get_websocket_connection_metric_labels(
    _connection_params: Option<&JsonValue>,
    _headers: &HeaderMap,
) -> Vec<(String, String)> {
    Vec::new()
}

fn get_websocket_operation_metric_labels(operation: &GraphQlOperation) -> Vec<(String, String)> {
    vec![(
        String::from("operation_name"),
        String::from(operation.operation_name().unwrap_or("<null>")),
    )]
}
