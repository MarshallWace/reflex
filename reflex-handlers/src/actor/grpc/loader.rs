// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{collections::HashMap, iter::once, path::Path};

use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator},
    lang::create_record,
    stdlib::Stdlib,
};

use super::{
    schema::{get_grpc_schema_checksum, GrpcServiceId},
    GrpcClient, GrpcService,
};

pub fn create_grpc_loader<T: Expression>(
    services: impl IntoIterator<Item = (GrpcServiceId, T)>,
) -> impl Fn(&str, &Path) -> Option<Result<T, String>> {
    let client_imports = services.into_iter().collect::<HashMap<_, _>>();
    move |import_path: &str, module_path: &Path| {
        if !import_path.ends_with(".proto") {
            return None;
        }
        let schema_path = module_path
            .parent()
            .map(|parent| parent.join(import_path))
            .unwrap_or_else(|| Path::new(import_path).to_path_buf());
        match std::fs::read(&schema_path) {
            Err(error) => Some(Err(format!("{}", error))),
            Ok(bytes) => {
                let protocol_id = get_grpc_schema_checksum(&bytes);
                client_imports
                    .get(&protocol_id)
                    .map(|client| Ok(client.clone()))
            }
        }
    }
}

pub fn create_grpc_exports<'a, T: Expression, TService, TClient>(
    services: impl IntoIterator<Item = &'a TService>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Vec<(GrpcServiceId, T)>
where
    TService: GrpcService<TClient> + 'a,
    TClient: GrpcClient + 'a,
    T::Builtin: From<Stdlib>,
{
    services
        .into_iter()
        .map(|service| {
            (
                service.id(),
                create_default_module_export(
                    service.factory(factory, allocator),
                    factory,
                    allocator,
                ),
            )
        })
        .collect::<Vec<_>>()
}

fn create_default_module_export<T: Expression>(
    value: T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    create_record(once((String::from("default"), value)), factory, allocator)
}
