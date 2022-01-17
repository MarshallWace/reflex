// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    fs,
    path::{Path, PathBuf},
};

use anyhow::{anyhow, Context, Result};
use clap::Parser;
use reflex::{allocator::DefaultAllocator, lang::SharedTermFactory};
use reflex_cli::{syntax::js::default_js_loaders, Syntax};
use reflex_graphql::graphql_parser;
use reflex_handlers::{
    actor::{fetch::EFFECT_TYPE_FETCH, graphql::EFFECT_TYPE_GRAPHQL, grpc::EFFECT_TYPE_GRPC},
    default_handlers,
};
use reflex_server::{
    action::ServerCliAction,
    builtins::ServerBuiltins,
    cli::execute_query::{cli, ExecuteQueryCliOptions},
    imports::server_imports,
    GraphQlServerQueryTransform,
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
    /// Path to capture runtime signal playback file
    #[clap(long)]
    capture_effects: Option<PathBuf>,
    /// Path to previously-captured signal playback file
    #[clap(long)]
    replay_effects: Option<PathBuf>,
    /// Comma-separated list of captured signal types
    #[clap(long)]
    captured_effects: Option<Vec<String>>,
    /// Log runtime actions
    #[clap(long)]
    log: bool,
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
            graph_root: self.graph_root,
            syntax: self.syntax,
            query: self.query,
            variables: self.variables,
            headers: None,
            capture_effects: self.capture_effects,
            replay_effects: self.replay_effects,
            debug_actions: self.log,
            debug_compiler: self.debug_compiler,
            debug_interpreter: self.debug_interpreter,
            debug_stack: self.debug_stack,
            captured_effects: self
                .captured_effects
                .unwrap_or_else(|| default_captured_effects()),
        }
    }
}

fn default_captured_effects() -> Vec<String> {
    vec![
        String::from(EFFECT_TYPE_FETCH),
        String::from(EFFECT_TYPE_GRAPHQL),
        String::from(EFFECT_TYPE_GRPC),
    ]
}

#[tokio::main]
async fn main() -> Result<()> {
    let args = Args::parse();
    let schema = if let Some(schema_path) = &args.schema {
        Some(load_graphql_schema(schema_path.as_path())?)
    } else {
        None
    };
    let factory = SharedTermFactory::<ServerBuiltins>::default();
    let allocator = DefaultAllocator::default();
    let module_loader = Some(default_js_loaders(
        server_imports(&factory, &allocator),
        &factory,
        &allocator,
    ));
    let middleware = default_handlers::<ServerCliAction<_>, _, _, _, _>(
        &factory,
        &allocator,
        NoopReconnectTimeout,
    );
    let query_transform = GraphQlServerQueryTransform::new(schema)
        .map_err(|err| anyhow!("{}", err))
        .with_context(|| String::from("GraphQL schema error"))?;
    cli(
        args.into(),
        Some(std::env::vars()),
        module_loader,
        middleware,
        &factory,
        &allocator,
        query_transform,
    )
    .await
    .map(|response| println!("{}", response))
}

fn load_graphql_schema(path: &Path) -> Result<graphql_parser::schema::Document<'static, String>> {
    let source = fs::read_to_string(path)
        .with_context(|| format!("Failed to load GraphQL schema: {}", path.to_string_lossy()))?;
    graphql_parser::parse_schema(&source)
        .map(|document| document.into_static())
        .with_context(|| format!("Failed to load GraphQL schema: {}", path.to_string_lossy()))
}
