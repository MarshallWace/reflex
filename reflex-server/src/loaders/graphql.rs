// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{fs, path::Path};

use graphql_parser::schema::Document;
use reflex::{
    core::{
        ApplicationTerm, Arity, Expression, LambdaTerm, Signal, SignalTerm, StaticVariableTerm,
        StructPrototype, StructTerm, Term, VariableTerm,
    },
    stdlib::{
        builtin::BuiltinTerm,
        signal::SignalType,
        value::{StringValue, ValueTerm},
    },
};
use reflex_js::stdlib::{global_json_parse, global_json_stringify};

pub fn graphql_module_loader(
    import_path: &str,
    module_path: &Path,
) -> Option<Result<Expression, String>> {
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
        Ok(schema) => create_graphql_client(&schema),
        Err(error) => Err(format!("{}", error)),
    }
}

fn create_graphql_client<'a: 'src, 'src>(
    schema: &'a Document<&'src str>,
) -> Result<Expression, String> {
    Ok(create_struct(vec![(
        String::from("default"),
        Expression::new(Term::Lambda(LambdaTerm::new(
            Arity::from(0, 1, None),
            Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 1, None),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Builtin(BuiltinTerm::If)),
                        vec![
                            Expression::new(Term::Application(ApplicationTerm::new(
                                Expression::new(Term::Builtin(BuiltinTerm::StartsWith)),
                                vec![
                                    Expression::new(Term::Variable(VariableTerm::Static(
                                        StaticVariableTerm::new(0),
                                    ))),
                                    Expression::new(Term::Value(ValueTerm::String(
                                        StringValue::from("ws"),
                                    ))),
                                ],
                            ))),
                            Expression::new(Term::Application(ApplicationTerm::new(
                                create_graphql_ws_client(schema),
                                vec![Expression::new(Term::Variable(VariableTerm::Static(
                                    StaticVariableTerm::new(0),
                                )))],
                            ))),
                            Expression::new(Term::Application(ApplicationTerm::new(
                                create_graphql_http_client(schema),
                                vec![Expression::new(Term::Variable(VariableTerm::Static(
                                    StaticVariableTerm::new(0),
                                )))],
                            ))),
                        ],
                    ))),
                ))),
                vec![Expression::new(Term::Application(ApplicationTerm::new(
                    Expression::new(Term::Builtin(BuiltinTerm::Get)),
                    vec![
                        Expression::new(Term::Variable(VariableTerm::Static(
                            StaticVariableTerm::new(0),
                        ))),
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from("url")))),
                    ],
                )))],
            ))),
        ))),
    )]))
}

fn create_graphql_http_client<'a: 'src, 'src>(_schema: &'a Document<&'src str>) -> Expression {
    Expression::new(Term::Lambda(LambdaTerm::new(
        Arity::from(0, 1, None),
        create_struct(vec![(
            String::from("execute"),
            Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 1, None),
                Expression::new(Term::Application(ApplicationTerm::new(
                    Expression::new(Term::Builtin(BuiltinTerm::Get)),
                    vec![
                        Expression::new(Term::Application(ApplicationTerm::new(
                            global_json_parse(),
                            vec![Expression::new(Term::Application(ApplicationTerm::new(
                                Expression::new(Term::Builtin(BuiltinTerm::Effect)),
                                vec![
                                    Expression::new(Term::Value(ValueTerm::String(
                                        StringValue::from("reflex::http::fetch"),
                                    ))),
                                    Expression::new(Term::Variable(VariableTerm::Static(
                                        StaticVariableTerm::new(1),
                                    ))),
                                    Expression::new(Term::Value(ValueTerm::String(
                                        StringValue::from("POST"),
                                    ))),
                                    Expression::new(Term::Struct(StructTerm::new(
                                        Some(StructPrototype::new(vec![ValueTerm::String(
                                            StringValue::from("Content-Type"),
                                        )])),
                                        vec![Expression::new(Term::Value(ValueTerm::String(
                                            StringValue::from("application/json"),
                                        )))],
                                    ))),
                                    Expression::new(Term::Application(ApplicationTerm::new(
                                        global_json_stringify(),
                                        vec![Expression::new(Term::Variable(
                                            VariableTerm::Static(StaticVariableTerm::new(0)),
                                        ))],
                                    ))),
                                ],
                            )))],
                        ))),
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from("data")))),
                    ],
                ))),
            ))),
        )]),
    )))
}

fn create_graphql_ws_client<'a: 'src, 'src>(_schema: &'a Document<&'src str>) -> Expression {
    create_struct(vec![(
        String::from("execute"),
        Expression::new(Term::Lambda(LambdaTerm::new(
            Arity::from(0, 1, None),
            Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![ValueTerm::String(StringValue::from(
                    "GraphQL websocket connections not yet implemented",
                ))],
            )))),
        ))),
    )])
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
