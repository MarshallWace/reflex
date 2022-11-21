// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{
    fs,
    net::SocketAddr,
    path::{Path, PathBuf},
    str::FromStr,
    time::{Duration, Instant},
};

use anyhow::{anyhow, Context, Result};
use clap::Parser;
use metrics_exporter_prometheus::PrometheusBuilder;
use opentelemetry::trace::noop::NoopTracer;
use reflex_cli::{compile_entry_point, syntax::js::default_js_loaders, Syntax};
use reflex_dispatcher::Action;
use reflex_graphql::{parse_graphql_schema, GraphQlSchema, NoopGraphQlQueryTransform};
use reflex_handlers::{
    default_handler_actors,
    utils::tls::{create_https_client, hyper_tls, tokio_native_tls::native_tls::Certificate},
    DefaultHandlerMetricNames,
};
use reflex_interpreter::{compiler::CompilerOptions, InterpreterOptions};
use reflex_lang::{allocator::DefaultAllocator, CachedSharedTerm, SharedTermFactory};
use reflex_server::{
    action::ServerCliAction,
    builtins::ServerBuiltins,
    cli::{
        reflex_server::{
            cli, GraphQlWebServerMetricLabels, OpenTelemetryConfig, ReflexServerCliOptions,
        },
        task::{ServerCliTaskActor, ServerCliTaskFactory},
    },
    imports::server_imports,
    logger::{
        formatted::FormattedLogger, json::JsonActionLogger, prometheus::PrometheusLogger,
        ActionLogger, ChainLogger, EitherLogger,
    },
    scheduler_metrics::{
        ServerMetricsHandlerTimer, ServerMetricsInstrumentation, ServerSchedulerMetricNames,
    },
    server::action::init::{
        InitGraphRootAction, InitHttpServerAction, InitOpenTelemetryAction,
        InitPrometheusMetricsAction,
    },
    GraphQlWebServerMetricNames,
};
use reflex_server::{
    server::utils::EitherTracer, tokio_runtime_metrics_export::TokioRuntimeMonitorMetricNames,
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
    type TBuiltin = ServerBuiltins;
    type T = CachedSharedTerm<TBuiltin>;
    type TFactory = SharedTermFactory<TBuiltin>;
    type TAllocator = DefaultAllocator<T>;
    type TConnect = hyper_tls::HttpsConnector<hyper::client::HttpConnector>;
    type TReconnect = FibonacciReconnectTimeout;
    type TAction = ServerCliAction<T>;
    type TTask = ServerCliTaskFactory<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        NoopGraphQlQueryTransform,
        NoopGraphQlQueryTransform,
        GraphQlWebServerMetricLabels,
        GraphQlWebServerMetricLabels,
        GraphQlWebServerMetricLabels,
        GraphQlWebServerMetricLabels,
        GraphQlWebServerMetricLabels,
        EitherTracer<NoopTracer, opentelemetry::sdk::trace::Tracer>,
    >;

    let args = Args::parse();
    let mut logger = match args.log {
        Some(LogFormat::Json) => EitherLogger::Left(JsonActionLogger::stderr()),
        _ => EitherLogger::Right(FormattedLogger::stderr("server")),
    };
    if let Some(port) = args.metrics_port {
        let address = SocketAddr::from(([0, 0, 0, 0], port));
        log_server_action(
            &mut logger,
            &TAction::from(InitPrometheusMetricsAction { address }),
        );
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
    let https_client: hyper::Client<TConnect> = create_https_client(tls_cert)?;
    let schema = if let Some(schema_path) = &args.schema {
        Some(load_graphql_schema(schema_path.as_path())?)
    } else {
        None
    };
    let mut logger = {
        let prometheus_logger = args
            .metrics_port
            .map(|_| PrometheusLogger::new(Default::default()));
        let stdout_logger = args.log.map(|_| logger.clone());
        ChainLogger::new(stdout_logger, prometheus_logger)
    };
    let factory: TFactory = SharedTermFactory::<TBuiltin>::default();
    let allocator: TAllocator = DefaultAllocator::default();
    let tracer = match OpenTelemetryConfig::parse_env(std::env::vars())? {
        None => None,
        Some(config) => {
            log_server_action(
                &mut logger,
                &TAction::from(InitOpenTelemetryAction {
                    config: config.clone(),
                }),
            );
            Some(config.into_tracer()?)
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
            &TAction::from(InitGraphRootAction {
                compiler_duration: compiler_elapsed_time,
                instruction_count: program.instructions.len(),
            }),
        );
        graph_root
    };
    let metric_names = ServerSchedulerMetricNames::default();
    let config: ReflexServerCliOptions = args.into();
    log_server_action(
        &mut logger,
        &TAction::from(InitHttpServerAction {
            address: config.address,
        }),
    );
    let server =
        cli::<TAction, TTask, T, TFactory, TAllocator, _, _, _, _, _, _, _, _, _, _, _, _>(
            config,
            graph_root,
            schema,
            |context, main_pid| {
                default_handler_actors::<
                    TAction,
                    TTask,
                    T,
                    TFactory,
                    TAllocator,
                    TConnect,
                    TReconnect,
                >(
                    https_client,
                    &factory,
                    &allocator,
                    FibonacciReconnectTimeout {
                        units: Duration::from_secs(1),
                        max_timeout: Duration::from_secs(30),
                    },
                    DefaultHandlerMetricNames::default(),
                    main_pid,
                )
                .into_iter()
                .map(|actor| (context.generate_pid(), ServerCliTaskActor::from(actor)))
                .collect::<Vec<_>>()
            },
            &factory,
            &allocator,
            compiler_options,
            interpreter_options,
            NoopGraphQlQueryTransform,
            NoopGraphQlQueryTransform,
            GraphQlWebServerMetricNames::default(),
            TokioRuntimeMonitorMetricNames::default(),
            GraphQlWebServerMetricLabels,
            GraphQlWebServerMetricLabels,
            GraphQlWebServerMetricLabels,
            GraphQlWebServerMetricLabels,
            GraphQlWebServerMetricLabels,
            match tracer {
                None => EitherTracer::Left(NoopTracer::default()),
                Some(tracer) => EitherTracer::Right(tracer),
            },
            logger,
            ServerMetricsHandlerTimer::new(metric_names),
            ServerMetricsInstrumentation::new(metric_names),
        )
        .with_context(|| anyhow!("Server startup failed"))?;
    server.await.with_context(|| anyhow!("Server error"))
}

fn log_server_action<TAction: Action>(
    logger: &mut impl ActionLogger<Action = TAction>,
    action: &TAction,
) {
    logger.log(action)
}

fn load_graphql_schema(path: &Path) -> Result<GraphQlSchema> {
    let source = fs::read_to_string(path)
        .with_context(|| format!("Failed to load GraphQL schema: {}", path.to_string_lossy()))?;
    parse_graphql_schema(&source)
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
