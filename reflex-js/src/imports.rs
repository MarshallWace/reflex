// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use crate::stdlib::Stdlib as JsStdlib;
use reflex::core::{Expression, ExpressionFactory, HeapAllocator};
use reflex_stdlib::Stdlib;

pub(crate) mod core;
pub(crate) mod utils;

pub use self::core::import_core;
pub use self::utils::import_utils;

pub fn builtin_imports<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Vec<(&'static str, T)>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    vec![
        ("reflex::core", import_core(factory, allocator)),
        ("reflex::utils", import_utils(factory, allocator)),
    ]
}
