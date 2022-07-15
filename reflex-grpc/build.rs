// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::path::PathBuf;

fn main() -> std::io::Result<()> {
    compile_protos()
}

fn compile_protos() -> std::io::Result<()> {
    compile_mocks()?;
    compile_hashable_well_known_types()?;
    Ok(())
}

fn compile_mocks() -> std::io::Result<()> {
    const PROTOS: [&'static str; 1] = ["src/proto/mocks/hello_world_service.proto"];
    prost_build::Config::new()
        .out_dir(create_package_path("./grpc")?)
        .include_file(get_package_path("./grpc/mocks.rs"))
        .file_descriptor_set_path(get_package_path("./grpc/mocks.proto.bin"))
        .compile_protos(&PROTOS, &["src/proto/mocks"])
}

fn compile_hashable_well_known_types() -> std::io::Result<()> {
    const PROTOS: [&'static str; 11] = [
        "src/proto/google/protobuf/any.proto",
        "src/proto/google/protobuf/api.proto",
        "src/proto/google/protobuf/descriptor.proto",
        "src/proto/google/protobuf/duration.proto",
        "src/proto/google/protobuf/empty.proto",
        "src/proto/google/protobuf/field_mask.proto",
        "src/proto/google/protobuf/source_context.proto",
        "src/proto/google/protobuf/struct.proto",
        "src/proto/google/protobuf/timestamp.proto",
        "src/proto/google/protobuf/type.proto",
        "src/proto/google/protobuf/wrappers.proto",
    ];
    let derive_hash_types = {
        const MESSAGE_TYPES: [&'static str; 54] = [
            "google.protobuf.Any",
            "google.protobuf.Api",
            "google.protobuf.BoolValue",
            "google.protobuf.BytesValue",
            "google.protobuf.DescriptorProto",
            "google.protobuf.DescriptorProto.ExtensionRange",
            "google.protobuf.DescriptorProto.ReservedRange",
            "google.protobuf.DoubleValue",
            "google.protobuf.Duration",
            "google.protobuf.Empty",
            "google.protobuf.Enum",
            "google.protobuf.EnumDescriptorProto",
            "google.protobuf.EnumDescriptorProto.EnumReservedRange",
            "google.protobuf.EnumOptions",
            "google.protobuf.EnumValue",
            "google.protobuf.EnumValueDescriptorProto",
            "google.protobuf.EnumValueOptions",
            "google.protobuf.ExtensionRangeOptions",
            "google.protobuf.Field",
            "google.protobuf.FieldDescriptorProto",
            "google.protobuf.FieldMask",
            "google.protobuf.FieldOptions",
            "google.protobuf.FileDescriptorProto",
            "google.protobuf.FileDescriptorSet",
            "google.protobuf.FileOptions",
            "google.protobuf.FloatValue",
            "google.protobuf.GeneratedCodeInfo",
            "google.protobuf.GeneratedCodeInfo.Annotation",
            "google.protobuf.Int32Value",
            "google.protobuf.Int64Value",
            "google.protobuf.ListValue",
            "google.protobuf.MessageOptions",
            "google.protobuf.Method",
            "google.protobuf.MethodDescriptorProto",
            "google.protobuf.MethodOptions",
            "google.protobuf.Mixin",
            "google.protobuf.OneofDescriptorProto",
            "google.protobuf.OneofOptions",
            "google.protobuf.Option",
            "google.protobuf.ServiceDescriptorProto",
            "google.protobuf.ServiceOptions",
            "google.protobuf.SourceCodeInfo.Location",
            "google.protobuf.SourceCodeInfo",
            "google.protobuf.SourceContext",
            "google.protobuf.StringValue",
            "google.protobuf.Struct",
            "google.protobuf.Timestamp",
            "google.protobuf.Type",
            "google.protobuf.UInt32Value",
            "google.protobuf.UInt64Value",
            "google.protobuf.UninterpretedOption",
            "google.protobuf.UninterpretedOption.NamePart",
            "google.protobuf.Value",
            "google.protobuf.Value.kind",
        ];
        const CUSTOM_HASH_TYPES: [&'static str; 5] = [
            "google.protobuf.DoubleValue",
            "google.protobuf.FloatValue",
            "google.protobuf.Struct",
            "google.protobuf.UninterpretedOption",
            "google.protobuf.Value.kind",
        ];
        MESSAGE_TYPES
            .iter()
            .copied()
            .filter(|type_name| !CUSTOM_HASH_TYPES.contains(type_name))
    };
    let mut config = prost_build::Config::new();
    config
        .out_dir(create_package_path("./protos")?)
        .compile_well_known_types()
        .disable_comments(["."])
        .include_file(get_package_path("./protos/protobuf.rs"))
        .file_descriptor_set_path(get_package_path("./protos/protobuf.proto.bin"));
    for type_name in derive_hash_types {
        config.type_attribute(type_name, "#[derive(Hash)]");
    }
    config.compile_protos(&PROTOS, &["src/proto"])
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
