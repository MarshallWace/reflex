// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::{
    core::{Expression, StructPrototype, StructTerm, Term},
    stdlib::value::{StringValue, ValueTerm},
};

mod boolean;
use boolean::global_boolean;
mod json;
use json::global_json;
mod map;
use map::global_map;
mod math;
use math::global_math;
mod set;
use set::global_set;

pub use json::json_stringify;
mod process;
pub use process::global_process;

pub fn builtin_globals() -> Vec<(&'static str, Expression)> {
    vec![
        ("Boolean", global_boolean()),
        ("Math", global_math()),
        ("Map", global_map()),
        ("Set", global_set()),
        ("JSON", global_json()),
    ]
}

fn create_struct<'a>(fields: impl IntoIterator<Item = (&'a str, Expression)>) -> Expression {
    let (keys, values) = fields
        .into_iter()
        .map(|(key, value)| {
            (
                Expression::new(Term::Value(ValueTerm::String(StringValue::from(key)))),
                value,
            )
        })
        .unzip();
    Expression::new(Term::Struct(StructTerm::new(
        Some(StructPrototype::new(keys)),
        values,
    )))
}
