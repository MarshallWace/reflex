// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
pub mod allocator;
pub mod cache;
pub mod core;
pub mod hash;

pub mod compiler;
pub mod interpreter;

pub mod lang {
    mod factory;
    pub use factory::*;
    pub mod term;
    pub(crate) use term::*;
    pub use term::{
        as_integer, create_struct, deduplicate_hashmap_entries, deduplicate_hashset_entries,
        is_integer, is_truthy, BuiltinTerm, NativeFunction, ValueTerm,
    };
}

pub mod parser {
    pub mod sexpr;
}
