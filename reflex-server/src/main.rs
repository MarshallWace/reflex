// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{path::Path, process};

use reflex_server::cli::{
    builtin_imports, builtin_loaders, builtin_signal_handler, cli, dynamic_module_loader,
    reflex::core::{Expression, SerializedTerm},
    SignalResult,
};

#[tokio::main]
pub async fn main() {
    process::exit(
        match cli(create_signal_handler(), create_module_loader()).await {
            Ok(_) => 0,
            Err(error) => {
                eprintln!("{}", error);
                1
            }
        },
    );
}

fn create_signal_handler(
) -> impl Fn(&str, &[SerializedTerm]) -> Option<Result<SignalResult, String>> + Send + Sync + 'static
{
    builtin_signal_handler()
}

fn create_module_loader() -> impl Fn(&str, &Path) -> Result<Expression, String> {
    dynamic_module_loader(builtin_loaders(), Some(builtin_imports()))
}
