// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::stdlib::Stdlib as JsStdlib;
use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator},
    stdlib::Stdlib,
};

pub(crate) mod boolean;
pub use boolean::global_boolean;
pub(crate) mod error;
pub use error::{global_aggregate_error, global_error};
pub(crate) mod json;
pub use json::global_json;
pub(crate) mod map;
pub use map::global_map;
pub(crate) mod math;
pub use math::global_math;
pub(crate) mod object;
pub use object::global_object;
pub(crate) mod process;
pub use process::global_process;
pub(crate) mod set;
pub use set::global_set;
pub(crate) mod string;
pub use string::global_string;

pub fn builtin_globals<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Vec<(&'static str, T)>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    vec![
        ("Boolean", global_boolean(factory, allocator)),
        ("String", global_string(factory, allocator)),
        ("Object", global_object(factory, allocator)),
        ("Error", global_error(factory, allocator)),
        ("AggregateError", global_aggregate_error(factory, allocator)),
        ("Math", global_math(factory, allocator)),
        ("Map", global_map(factory)),
        ("Set", global_set(factory)),
        ("JSON", global_json(factory, allocator)),
        (
            "encodeURIComponent",
            factory.create_builtin_term(JsStdlib::EncodeUriComponent),
        ),
        ("process", global_process(factory, allocator)),
    ]
}
