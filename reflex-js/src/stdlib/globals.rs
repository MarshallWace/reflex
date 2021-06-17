// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::{
    core::{Expression, StructPrototype, StructTerm, Term},
    stdlib::value::ValueTerm,
};

mod boolean;
use boolean::global_boolean;
mod json;
use json::global_json;
mod map;
use map::global_map;
mod math;
use math::global_math;
mod object;
use object::global_object;
mod set;
use set::global_set;

pub use json::{global_json_parse, global_json_stringify, json_stringify, json_stringify_string};
mod process;
pub use process::global_process;

pub fn builtin_globals() -> Vec<(&'static str, Expression)> {
    vec![
        ("Boolean", global_boolean()),
        ("Object", global_object()),
        ("Math", global_math()),
        ("Map", global_map()),
        ("Set", global_set()),
        ("JSON", global_json()),
    ]
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
