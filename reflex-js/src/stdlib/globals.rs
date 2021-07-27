// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{Expression, ExpressionFactory, HeapAllocator};

pub(crate) mod boolean;
pub use boolean::global_boolean;
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
pub use string::{global_encode_uri_component, global_string};

pub fn builtin_globals<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Vec<(&'static str, T)> {
    vec![
        ("Boolean", global_boolean(factory, allocator)),
        ("String", global_string(factory, allocator)),
        ("Object", global_object(factory, allocator)),
        ("Math", global_math(factory, allocator)),
        ("Map", global_map(factory)),
        ("Set", global_set(factory)),
        ("JSON", global_json(factory, allocator)),
        (
            "encodeURIComponent",
            global_encode_uri_component(factory, allocator),
        ),
    ]
}
