// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::path::PathBuf;

fn main() -> std::io::Result<()> {
    compile_protos()
}

fn compile_protos() -> std::io::Result<()> {
    compile_mocks()?;
    Ok(())
}

fn compile_mocks() -> std::io::Result<()> {
    const PROTOS: [&'static str; 1] = ["src/proto/mocks/hello_world.proto"];
    prost_build::Config::new()
        .out_dir(create_package_path("./grpc")?)
        .include_file(get_package_path("./grpc/mocks.rs"))
        .file_descriptor_set_path(get_package_path("./grpc/mocks.proto.bin"))
        .compile_protos(&PROTOS, &["src/proto/mocks"])
}

fn create_package_path(path: &str) -> std::io::Result<PathBuf> {
    let out_path = get_package_path(path);
    match std::fs::create_dir(&out_path) {
        Err(err) if err.kind() != std::io::ErrorKind::AlreadyExists => Err(err),
        _ => Ok(out_path),
    }
}

fn get_package_path(path: &str) -> PathBuf {
    PathBuf::from(format!("{}/{}", std::env::var("OUT_DIR").unwrap(), path))
}
