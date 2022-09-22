// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    fs,
    path::{Path, PathBuf},
    str::FromStr,
};

use anyhow::{anyhow, Context, Result};
use clap::Parser;
use reflex_cli::{compile_entry_point, syntax::js::default_js_loaders, Syntax};
use reflex_dispatcher::{session_recorder::SessionRecorder, SchedulerMiddleware};
use reflex_graphql::{parse_graphql_schema, GraphQlSchema, NoopGraphQlQueryTransform};
use reflex_handlers::{
    default_handlers,
    utils::tls::{create_https_client, tokio_native_tls::native_tls::Certificate},
    DefaultHandlersMetricNames,
};
use reflex_interpreter::compiler::CompilerOptions;
use reflex_lang::{allocator::DefaultAllocator, SharedTermFactory};
use reflex_server::{
    action::ServerCliAction,
    builtins::ServerBuiltins,
    cli::execute_query::{cli, ExecuteQueryCliOptions, NoopHttpMiddleware},
    generate_session_recording_filename,
    imports::server_imports,
    logger::{formatted::FormattedLogger, json::JsonActionLogger, EitherLogger},
    middleware::LoggerMiddleware,
    recorder::FileRecorder,
    GraphQlWebServerMetricNames,
};
use reflex_utils::{reconnect::NoopReconnectTimeout, FileWriterFormat};

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
    /// Path to capture runtime event playback file
    #[clap(long)]
    capture_events: Option<Option<PathBuf>>,
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
    let https_client = create_https_client(tls_cert)?;
    let factory = SharedTermFactory::<ServerBuiltins>::default();
    let allocator = DefaultAllocator::default();
    let compiler_options = CompilerOptions {
        debug: args.debug_compiler,
        ..Default::default()
    };
    let logger_middleware = args.log.map(|format| {
        LoggerMiddleware::new(match format {
            Some(LogFormat::Json) => EitherLogger::Left(JsonActionLogger::stderr()),
            None => EitherLogger::Right(FormattedLogger::stderr("server")),
        })
    });
    let recorder_middleware = match args.capture_events.as_ref() {
        Some(output_path) => {
            let recorder = match output_path {
                Some(path) => FileRecorder::new(FileWriterFormat::MessagePack, path),
                None => FileRecorder::new(
                    FileWriterFormat::MessagePack,
                    PathBuf::from(generate_session_recording_filename(None)),
                ),
            };
            Some(SessionRecorder::new(recorder))
        }
        None => None,
    };
    let middleware = SchedulerMiddleware::new(logger_middleware, recorder_middleware);
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
    let actor = default_handlers::<ServerCliAction<_>, _, _, _, _, _>(
        https_client,
        &factory,
        &allocator,
        NoopReconnectTimeout,
        DefaultHandlersMetricNames::default(),
    );
    cli(
        args.into(),
        graph_root,
        schema,
        actor,
        middleware,
        &factory,
        &allocator,
        NoopGraphQlQueryTransform,
        NoopHttpMiddleware,
        GraphQlWebServerMetricNames::default(),
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
