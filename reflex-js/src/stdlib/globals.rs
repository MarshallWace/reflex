// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::{
    core::{Expression, StructPrototype, StructTerm, Term},
    stdlib::value::ValueTerm,
};

mod boolean;
pub use boolean::global_boolean;
mod json;
pub use json::{
    global_json, global_json_parse, global_json_stringify, json_stringify, json_stringify_string,
};
mod map;
pub use map::global_map;
mod math;
pub use math::global_math;
mod object;
pub use object::global_object;
mod process;
pub use process::global_process;
mod set;
pub use set::global_set;
mod string;
pub use string::{global_encode_uri_component, global_string};

pub fn builtin_globals() -> Vec<(&'static str, Expression)> {
    vec![
        ("Boolean", global_boolean()),
        ("String", global_string()),
        ("Object", global_object()),
        ("Math", global_math()),
        ("Map", global_map()),
        ("Set", global_set()),
        ("JSON", global_json()),
        ("encodeURIComponent", global_encode_uri_component()),
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
