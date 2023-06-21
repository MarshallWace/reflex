// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    fs,
    net::SocketAddr,
    path::{Path, PathBuf},
    str::FromStr,
    time::{Duration, Instant},
};

use anyhow::{anyhow, Context, Result};
use clap::Parser;
use http::HeaderMap;
use metrics_exporter_prometheus::PrometheusBuilder;
use reflex::{
    allocator::DefaultAllocator,
    compiler::CompilerOptions,
    core::Expression,
    interpreter::InterpreterOptions,
    lang::{CachedSharedTerm, SharedTermFactory},
};
use reflex_cli::{compile_entry_point, syntax::js::default_js_loaders, Syntax};
use reflex_dispatcher::{compose_actors, scheduler::sync::SyncContext, EitherActor};
use reflex_graphql::{graphql_parser, GraphQlOperationPayload, NoopGraphQlQueryTransform};
use reflex_handlers::{
    default_handlers,
    utils::tls::{create_https_client, tokio_native_tls::native_tls::Certificate},
    DefaultHandlersMetricNames,
};
use reflex_json::JsonValue;
use reflex_server::{
    action::ServerCliAction,
    builtins::ServerBuiltins,
    cli::reflex_server::{
        cli, get_operation_transaction_labels, OpenTelemetryConfig, ReflexServerCliOptions,
    },
    imports::server_imports,
    logger::{formatted::FormattedLogger, json::JsonActionLogger, ActionLogger, EitherLogger},
    middleware::{LoggerMiddleware, ServerMiddleware, TelemetryMiddlewareMetricNames},
    server::action::init::{
        InitGraphRootAction, InitHttpServerAction, InitOpenTelemetryAction,
        InitPrometheusMetricsAction,
    },
    GraphQlWebServerMetricNames,
};
use reflex_utils::reconnect::FibonacciReconnectTimeout;

/// Launch a GraphQL server for the provided graph root
#[derive(Parser)]
struct Args {
    /// Path to graph definition entry point
    entry_point: PathBuf,
    /// Graph definition syntax
    #[clap(long, default_value = "javascript")]
    syntax: Syntax,
    /// Path to GraphQL schema SDL
    #[clap(long)]
    schema: Option<PathBuf>,
    /// Port on which to expose a GraphQL HTTP server
    #[clap(long)]
    port: u16,
    /// Port on which to expose Prometheus HTTP metrics
    #[clap(long)]
    metrics_port: Option<u16>,
    /// Path to custom TLS certificate
    #[clap(long)]
    tls_cert: Option<PathBuf>,
    /// Log runtime actions
    #[clap(long)]
    log: Option<LogFormat>,
    /// Prevent static compiler optimizations
    #[clap(long)]
    unoptimized: bool,
    /// Log compiler output
    #[clap(long)]
    debug_compiler: bool,
    /// Log interpreter instructions
    #[clap(long)]
    debug_interpreter: bool,
    /// Log interpreter stack
    #[clap(long)]
    debug_stack: bool,
}
impl Into<ReflexServerCliOptions> for Args {
    fn into(self) -> ReflexServerCliOptions {
        ReflexServerCliOptions {
            address: SocketAddr::from(([0, 0, 0, 0], self.port)),
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum LogFormat {
    Json,
}
impl FromStr for LogFormat {
    type Err = anyhow::Error;
    fn from_str(input: &str) -> Result<Self, Self::Err> {
        match input.to_lowercase().as_str() {
            "json" => Ok(Self::Json),
            _ => Err(anyhow!("Unrecognized log format: {}", input)),
        }
    }
}

#[tokio::main]
pub async fn main() -> Result<()> {
    let args = Args::parse();
    let mut logger = match args.log {
        Some(LogFormat::Json) => EitherLogger::Left(JsonActionLogger::stderr()),
        _ => EitherLogger::Right(FormattedLogger::stderr("server")),
    };
    if let Some(port) = args.metrics_port {
        let address = SocketAddr::from(([0, 0, 0, 0], port));
        log_server_action(&mut logger, InitPrometheusMetricsAction { address });
        PrometheusBuilder::new()
            .with_http_listener(address)
            .install()
            .with_context(|| anyhow!("Failed to initialize Prometheus metrics endpoint"))?;
    }
    let tls_cert = args
        .tls_cert
        .as_ref()
        .map(|path| load_tls_cert(path.as_path()))
        .transpose()?;
    let https_client = create_https_client(tls_cert)?;
    let schema = if let Some(schema_path) = &args.schema {
        Some(load_graphql_schema(schema_path.as_path())?)
    } else {
        None
    };
    let factory = SharedTermFactory::<ServerBuiltins>::default();
    let allocator = DefaultAllocator::default();
    let middleware = default_handlers(
        https_client,
        &factory,
        &allocator,
        FibonacciReconnectTimeout {
            units: Duration::from_secs(1),
            max_timeout: Duration::from_secs(30),
        },
        DefaultHandlersMetricNames::default(),
    );
    let middleware = match OpenTelemetryConfig::parse_env(std::env::vars())? {
        None => EitherActor::Left(middleware),
        Some(config) => {
            log_server_action(
                &mut logger,
                InitOpenTelemetryAction {
                    config: config.clone(),
                },
            );
            EitherActor::Right(compose_actors(
                middleware,
                config.into_middleware(
                    get_operation_transaction_labels,
                    &factory,
                    &allocator,
                    TelemetryMiddlewareMetricNames::default(),
                )?,
            ))
        }
    };
    let compiler_options = CompilerOptions {
        debug: args.debug_compiler,
        ..if args.unoptimized {
            CompilerOptions::unoptimized()
        } else {
            CompilerOptions::default()
        }
    };
    let interpreter_options = InterpreterOptions {
        debug_instructions: args.debug_interpreter || args.debug_stack,
        debug_stack: args.debug_stack,
        ..InterpreterOptions::default()
    };
    let module_loader =
        default_js_loaders(server_imports(&factory, &allocator), &factory, &allocator);
    let graph_root = {
        let compiler_start_time = Instant::now();
        let graph_root = compile_entry_point(
            args.entry_point.as_path(),
            args.syntax,
            Some(std::env::vars()),
            Some(module_loader),
            &compiler_options,
            &factory,
            &allocator,
        )?;
        let compiler_elapsed_time = compiler_start_time.elapsed();
        let (program, _entry_point) = &graph_root;
        log_server_action(
            &mut logger,
            InitGraphRootAction {
                compiler_duration: compiler_elapsed_time,
                instruction_count: program.len(),
            },
        );
        graph_root
    };
    let config: ReflexServerCliOptions = args.into();
    log_server_action(
        &mut logger,
        InitHttpServerAction {
            address: config.address,
        },
    );
    let middleware = compose_actors(LoggerMiddleware::new(logger), middleware);
    let server = cli(
        config,
        graph_root,
        schema,
        ServerMiddleware::<ServerCliAction<CachedSharedTerm<ServerBuiltins>>, _, _>::post(
            middleware,
        ),
        &factory,
        &allocator,
        compiler_options,
        interpreter_options,
        NoopGraphQlQueryTransform,
        NoopGraphQlQueryTransform,
        GraphQlWebServerMetricNames::default(),
        get_http_query_metric_labels,
        get_websocket_connection_metric_labels,
        get_websocket_operation_metric_labels,
    )
    .with_context(|| anyhow!("Server startup failed"))?;
    server.await.with_context(|| anyhow!("Server error"))
}

fn get_http_query_metric_labels(
    operation: &GraphQlOperationPayload,
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

fn get_websocket_operation_metric_labels(
    operation_name: Option<&str>,
    _operation: &GraphQlOperationPayload,
) -> Vec<(String, String)> {
    vec![(
        String::from("operation_name"),
        String::from(operation_name.unwrap_or("<null>")),
    )]
}

fn log_server_action<T: Expression>(
    logger: &mut impl ActionLogger<Action = ServerCliAction<T>>,
    action: impl Into<ServerCliAction<T>>,
) {
    logger.log(&(action.into()), None, Option::<&SyncContext>::None)
}

fn load_graphql_schema(path: &Path) -> Result<graphql_parser::schema::Document<'static, String>> {
    let source = fs::read_to_string(path)
        .with_context(|| format!("Failed to load GraphQL schema: {}", path.to_string_lossy()))?;
    graphql_parser::parse_schema(&source)
        .map(|document| document.into_static())
        .with_context(|| format!("Failed to load GraphQL schema: {}", path.to_string_lossy()))
}

fn load_tls_cert(path: &Path) -> Result<Certificate> {
    let source = fs::read_to_string(path)
        .with_context(|| format!("Failed to load TLS certificate: {}", path.to_string_lossy()))?;
    Certificate::from_pem(source.as_bytes()).with_context(|| {
        format!(
            "Failed to parse TLS certificate: {}",
            path.to_string_lossy()
        )
    })
}
