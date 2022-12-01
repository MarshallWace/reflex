// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{
    fs,
    path::{Path, PathBuf},
    str::FromStr,
};

use anyhow::{anyhow, Context, Result};
use clap::Parser;
use opentelemetry::trace::noop::NoopTracer;
use reflex_cli::{compile_entry_point, syntax::js::default_js_loaders, Syntax};
use reflex_dispatcher::HandlerContext;
use reflex_graphql::{parse_graphql_schema, GraphQlSchema, NoopGraphQlQueryTransform};
use reflex_handlers::{
    default_handler_actors,
    utils::tls::{create_https_client, hyper_tls, tokio_native_tls::native_tls::Certificate},
    DefaultHandlerMetricNames,
};
use reflex_interpreter::compiler::CompilerOptions;
use reflex_lang::{allocator::DefaultAllocator, CachedSharedTerm, SharedTermFactory};
use reflex_scheduler::threadpool::AsyncTokioThreadPoolFactory;
use reflex_server::{
    action::ServerCliAction,
    builtins::ServerBuiltins,
    cli::{
        execute_query::{
            cli, ExecuteQueryCliOptions, GraphQlWebServerMetricLabels, NoopHttpMiddleware,
        },
        task::{ServerCliTaskActor, ServerCliTaskFactory},
    },
    imports::server_imports,
    logger::{
        formatted::FormattedActionLogger, formatter::PrefixedLogFormatter, json::JsonActionLogger,
        messages::DefaultActionFormatter, EitherLogger,
    },
    scheduler_metrics::{
        NoopServerMetricsSchedulerQueueInstrumentation, ServerMetricsInstrumentation,
    },
    server::{utils::EitherTracer, NoopWebSocketGraphQlServerQueryTransform},
    GraphQlWebServerActorFactory, GraphQlWebServerMetricNames,
};
use reflex_server::{
    cli::reflex_server::OpenTelemetryConfig,
    tokio_runtime_metrics_export::TokioRuntimeMonitorMetricNames,
};
use reflex_utils::reconnect::NoopReconnectTimeout;

/// Execute a GraphQL query against the provided graph root
#[derive(Parser)]
pub struct Args {
    /// Path to graph definition entry point
    #[clap(long)]
    graph_root: PathBuf,
    /// Graph definition syntax
    #[clap(long, default_value = "javascript")]
    syntax: Syntax,
    /// Path to GraphQL schema SDL
    #[clap(long)]
    schema: Option<PathBuf>,
    /// GraphQL query
    #[clap(long)]
    query: String,
    /// JSON-formatted GraphQL query variables
    #[clap(long)]
    variables: Option<String>,
    /// Path to custom TLS certificate
    #[clap(long)]
    tls_cert: Option<PathBuf>,
    /// Log runtime actions
    #[clap(long)]
    log: Option<Option<LogFormat>>,
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
impl Into<ExecuteQueryCliOptions> for Args {
    fn into(self) -> ExecuteQueryCliOptions {
        ExecuteQueryCliOptions {
            query: self.query,
            variables: self.variables,
            headers: None,
            debug_compiler: self.debug_compiler,
            debug_interpreter: self.debug_interpreter,
            debug_stack: self.debug_stack,
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
async fn main() -> Result<()> {
    type TBuiltin = ServerBuiltins;
    type T = CachedSharedTerm<TBuiltin>;
    type TFactory = SharedTermFactory<TBuiltin>;
    type TAllocator = DefaultAllocator<T>;
    type TConnect = hyper_tls::HttpsConnector<hyper::client::HttpConnector>;
    type TReconnect = NoopReconnectTimeout;
    type TTransformHttp = NoopGraphQlQueryTransform;
    type TTransformWs = NoopWebSocketGraphQlServerQueryTransform;
    type TMetricLabels = GraphQlWebServerMetricLabels;
    type TTracer = EitherTracer<NoopTracer, opentelemetry::sdk::trace::Tracer>;
    type TAction = ServerCliAction<T>;
    type TTask = ServerCliTaskFactory<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TTransformHttp,
        TTransformWs,
        TMetricLabels,
        TMetricLabels,
        TMetricLabels,
        TMetricLabels,
        TMetricLabels,
        TTracer,
    >;

    let args = Args::parse();
    let schema = if let Some(schema_path) = &args.schema {
        Some(load_graphql_schema(schema_path.as_path())?)
    } else {
        None
    };
    let tls_cert = args
        .tls_cert
        .as_ref()
        .map(|path| load_tls_cert(path.as_path()))
        .transpose()?;
    let https_client: hyper::Client<TConnect> = create_https_client(tls_cert)?;
    let factory: TFactory = SharedTermFactory::<TBuiltin>::default();
    let allocator: TAllocator = DefaultAllocator::default();
    let tracer = match OpenTelemetryConfig::parse_env(std::env::vars())? {
        None => None,
        Some(config) => Some(config.into_tracer()?),
    };
    let compiler_options = CompilerOptions {
        debug: args.debug_compiler,
        ..Default::default()
    };
    let logger = args.log.map(|format| match format {
        Some(LogFormat::Json) => {
            EitherLogger::Left(JsonActionLogger::<_, TAction, TTask>::stderr())
        }
        None => EitherLogger::Right(FormattedActionLogger::<_, _, TAction, TTask>::stderr(
            PrefixedLogFormatter::new("server", DefaultActionFormatter::default()),
        )),
    });
    let module_loader = Some(default_js_loaders(
        server_imports(&factory, &allocator),
        &factory,
        &allocator,
    ));
    let env = Some(std::env::vars());
    let graph_root = compile_entry_point(
        args.graph_root.as_path(),
        args.syntax,
        env,
        module_loader,
        &compiler_options,
        &factory,
        &allocator,
    )?;
    cli::<TAction, TTask, T, TFactory, TAllocator, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _>(
        args.into(),
        graph_root,
        schema,
        GraphQlWebServerActorFactory::new(|context| {
            default_handler_actors::<TAction, TTask, T, TFactory, TAllocator, TConnect, TReconnect>(
                https_client,
                &factory,
                &allocator,
                NoopReconnectTimeout,
                DefaultHandlerMetricNames::default(),
                context.pid(),
            )
            .into_iter()
            .map(|actor| (context.generate_pid(), ServerCliTaskActor::from(actor)))
            .collect::<Vec<_>>()
        }),
        &factory,
        &allocator,
        NoopGraphQlQueryTransform,
        NoopWebSocketGraphQlServerQueryTransform,
        NoopHttpMiddleware,
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
        ServerMetricsInstrumentation::new(
            NoopServerMetricsSchedulerQueueInstrumentation::default(),
            Default::default(),
        ),
        GraphQlWebServerMetricNames::default(),
        TokioRuntimeMonitorMetricNames::default(),
        AsyncTokioThreadPoolFactory::default(),
        AsyncTokioThreadPoolFactory::default(),
    )
    .await
    .map(|response| println!("{}", response))
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
