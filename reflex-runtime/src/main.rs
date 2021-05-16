// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex_js::{
    stdlib::{builtin_globals, builtin_imports},
    Env,
};
use reflex_runtime::{builtin_signal_handlers, run};

#[tokio::main]
pub async fn main() {
    let env = Env::new()
        .with_globals(builtin_globals())
        .with_imports(builtin_imports());
    let signal_handlers = builtin_signal_handlers();
    let command_buffer_size = 32;
    let result_buffer_size = 32;
    run(
        env,
        signal_handlers,
        command_buffer_size,
        result_buffer_size,
    )
    .await
}
