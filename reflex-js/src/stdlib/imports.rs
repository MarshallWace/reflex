// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::{
    core::{Expression, StructPrototype, StructTerm, Term},
    stdlib::value::{StringValue, ValueTerm},
};

mod date;
use date::import_date;
mod graphql;
use graphql::import_graphql;
mod http;
use http::import_http;
mod utils;
use utils::import_utils;

pub fn builtin_imports() -> Vec<(&'static str, Expression)> {
    vec![
        ("reflex::utils", import_utils()),
        ("reflex::date", import_date()),
        ("reflex::graphql", import_graphql()),
        ("reflex::http", import_http()),
    ]
}

fn create_struct<'a>(fields: impl IntoIterator<Item = (&'a str, Expression)>) -> Expression {
    let (keys, values) = fields
        .into_iter()
        .map(|(key, value)| (ValueTerm::String(StringValue::from(key)), value))
        .unzip();
    Expression::new(Term::Struct(StructTerm::new(
        Some(StructPrototype::new(keys)),
        values,
    )))
}
