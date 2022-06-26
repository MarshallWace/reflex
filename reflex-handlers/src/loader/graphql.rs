// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    fs,
    iter::{empty, once},
    path::Path,
};

use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator},
    lang::create_struct,
    stdlib::Stdlib,
};
use reflex_graphql::graphql_parser::{self, schema::Document};

use crate::actor::graphql::EFFECT_TYPE_GRAPHQL;

pub fn graphql_loader<T: Expression>(
    factory: &(impl ExpressionFactory<T> + Clone + 'static),
    allocator: &(impl HeapAllocator<T> + Clone + 'static),
) -> impl Fn(&str, &Path) -> Option<Result<T, String>>
where
    T::Builtin: From<Stdlib>,
{
    let factory = factory.clone();
    let allocator = allocator.clone();
    move |import_path: &str, module_path: &Path| {
        if !import_path.ends_with(".graphql") {
            return None;
        }
        let schema_path = module_path
            .parent()
            .map(|parent| parent.join(import_path))
            .unwrap_or_else(|| Path::new(import_path).to_path_buf());
        Some(load_graphql_module(&schema_path, &factory, &allocator))
    }
}

fn load_graphql_module<T: Expression>(
    path: &Path,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, String>
where
    T::Builtin: From<Stdlib>,
{
    let source = match fs::read_to_string(path) {
        Ok(source) => Ok(source),
        Err(error) => Err(format!("{}", error)),
    }?;
    match graphql_parser::parse_schema(&source) {
        Ok(schema) => Ok(create_graphql_module(&schema, factory, allocator)),
        Err(error) => Err(format!("{}", error)),
    }
}

fn create_graphql_module<'a: 'src, 'src, T: Expression>(
    schema: &'a Document<&'src str>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Stdlib>,
{
    create_default_export(
        create_graphql_client_constructor(schema, factory, allocator),
        factory,
        allocator,
    )
}

fn create_graphql_client_constructor<'a: 'src, 'src, T: Expression>(
    schema: &'a Document<&'src str>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Stdlib>,
{
    factory.create_lambda_term(
        1,
        factory.create_application_term(
            create_graphql_client_instance(schema, factory, allocator),
            allocator.create_unit_list(get_struct_field(
                factory.create_static_variable_term(0),
                String::from("url"),
                factory,
                allocator,
            )),
        ),
    )
}

fn create_graphql_client_instance<'a: 'src, 'src, T: Expression>(
    _schema: &'a Document<&'src str>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Stdlib>,
{
    factory.create_lambda_term(
        1,
        create_struct(
            vec![(
                String::from("execute"),
                factory.create_lambda_term(
                    1,
                    factory.create_application_term(
                        factory.create_builtin_term(Stdlib::Effect),
                        allocator.create_list(vec![
                            factory.create_string_term(
                                allocator.create_static_string(EFFECT_TYPE_GRAPHQL),
                            ),
                            factory.create_static_variable_term(1),
                            get_struct_field(
                                factory.create_static_variable_term(0),
                                String::from("query"),
                                factory,
                                allocator,
                            ),
                            get_optional_struct_field(
                                factory.create_static_variable_term(0),
                                String::from("operationName"),
                                factory.create_nil_term(),
                                factory,
                                allocator,
                            ),
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::ResolveDeep),
                                allocator.create_list([get_struct_field(
                                    factory.create_static_variable_term(0),
                                    String::from("variables"),
                                    factory,
                                    allocator,
                                )]),
                            ),
                            create_struct(empty(), factory, allocator),
                            get_optional_struct_field(
                                factory.create_static_variable_term(0),
                                String::from("token"),
                                factory.create_nil_term(),
                                factory,
                                allocator,
                            ),
                        ]),
                    ),
                ),
            )],
            factory,
            allocator,
        ),
    )
}

fn create_default_export<T: Expression>(
    value: T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    create_struct(once((String::from("default"), value)), factory, allocator)
}

fn get_struct_field<T: Expression>(
    target: T,
    field: String,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Stdlib>,
{
    factory.create_application_term(
        factory.create_builtin_term(Stdlib::Get),
        allocator.create_pair(
            target,
            factory.create_string_term(allocator.create_string(field)),
        ),
    )
}

fn get_optional_struct_field<T: Expression>(
    target: T,
    field: String,
    fallback: T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Stdlib>,
{
    factory.create_application_term(
        factory.create_builtin_term(Stdlib::If),
        allocator.create_triple(
            factory.create_application_term(
                factory.create_builtin_term(Stdlib::Contains),
                allocator.create_pair(
                    target.clone(),
                    factory.create_string_term(allocator.create_string(field.clone())),
                ),
            ),
            factory.create_application_term(
                factory.create_builtin_term(Stdlib::Get),
                allocator.create_pair(
                    target,
                    factory.create_string_term(allocator.create_string(field)),
                ),
            ),
            fallback,
        ),
    )
}
