// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{fs, path::Path};

use graphql_parser::schema::Document;
use reflex::{
    core::{
        ApplicationTerm, Arity, Expression, LambdaTerm, StructPrototype, StructTerm, Term,
        VariableTerm,
    },
    stdlib::{
        builtin::BuiltinTerm,
        value::{StringValue, ValueTerm},
    },
};
use reflex_js::builtins::flatten_deep;

pub fn graphql_loader(import_path: &str, module_path: &Path) -> Option<Result<Expression, String>> {
    if !import_path.ends_with(".graphql") {
        return None;
    }
    let schema_path = module_path
        .parent()
        .map(|parent| parent.join(import_path))
        .unwrap_or_else(|| Path::new(import_path).to_path_buf());
    Some(load_graphql_module(&schema_path))
}

fn load_graphql_module(path: &Path) -> Result<Expression, String> {
    let source = match fs::read_to_string(path) {
        Ok(source) => Ok(source),
        Err(error) => Err(format!("{}", error)),
    }?;
    match graphql_parser::parse_schema(&source) {
        Ok(schema) => Ok(create_graphql_module(&schema)),
        Err(error) => Err(format!("{}", error)),
    }
}

fn create_graphql_module<'a: 'src, 'src>(schema: &'a Document<&'src str>) -> Expression {
    create_default_export(create_graphql_client_constructor(schema))
}

fn create_graphql_client_constructor<'a: 'src, 'src>(
    schema: &'a Document<&'src str>,
) -> Expression {
    Expression::new(Term::Lambda(LambdaTerm::new(
        Arity::from(0, 1, None),
        Expression::new(Term::Application(ApplicationTerm::new(
            create_graphql_client_instance(schema),
            vec![get_struct_field(
                Expression::new(Term::Variable(VariableTerm::scoped(0))),
                String::from("url"),
            )],
        ))),
    )))
}

fn create_graphql_client_instance<'a: 'src, 'src>(_schema: &'a Document<&'src str>) -> Expression {
    Expression::new(Term::Lambda(LambdaTerm::new(
        Arity::from(0, 1, None),
        create_struct(vec![(
            String::from("execute"),
            Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 1, None),
                Expression::new(Term::Application(ApplicationTerm::new(
                    Expression::new(Term::Builtin(BuiltinTerm::Effect)),
                    vec![
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                            "reflex::graphql::execute",
                        )))),
                        Expression::new(Term::Variable(VariableTerm::scoped(1))),
                        get_struct_field(
                            Expression::new(Term::Variable(VariableTerm::scoped(0))),
                            String::from("query"),
                        ),
                        Expression::new(Term::Value(ValueTerm::Null)),
                        Expression::new(Term::Application(ApplicationTerm::new(
                            flatten_deep(),
                            vec![get_struct_field(
                                Expression::new(Term::Variable(VariableTerm::scoped(0))),
                                String::from("variables"),
                            )],
                        ))),
                        Expression::new(Term::Struct(StructTerm::new(
                            Some(StructPrototype::new(Vec::new())),
                            Vec::new(),
                        ))),
                    ],
                ))),
            ))),
        )]),
    )))
}

fn create_default_export(value: Expression) -> Expression {
    Expression::new(Term::Struct(StructTerm::new(
        Some(StructPrototype::new(vec![ValueTerm::String(
            StringValue::from("default"),
        )])),
        vec![value],
    )))
}

fn get_struct_field(target: Expression, field: String) -> Expression {
    Expression::new(Term::Application(ApplicationTerm::new(
        Expression::new(Term::Builtin(BuiltinTerm::Get)),
        vec![
            target,
            Expression::new(Term::Value(ValueTerm::String(StringValue::from(field)))),
        ],
    )))
}

fn create_struct(fields: impl IntoIterator<Item = (String, Expression)>) -> Expression {
    let (keys, values) = fields
        .into_iter()
        .map(|(key, value)| (ValueTerm::String(key), value))
        .unzip();
    Expression::new(Term::Struct(StructTerm::new(
        Some(StructPrototype::new(keys)),
        values,
    )))
}
