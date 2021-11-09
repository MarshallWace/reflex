// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::path::Path;

use crate::static_module_loader;
use crate::stdlib::Stdlib as JsStdlib;
use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator},
    stdlib::Stdlib,
};

pub(crate) mod date;
pub use date::import_date;
pub(crate) mod graphql;
pub use graphql::import_graphql;
pub(crate) mod http;
pub use http::import_http;
pub(crate) mod loader;
pub use loader::import_loader;
pub(crate) mod utils;
pub use utils::import_utils;

pub fn builtin_imports<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Vec<(&'static str, T)>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    vec![
        ("reflex::date", import_date(factory, allocator)),
        ("reflex::graphql", import_graphql(factory, allocator)),
        ("reflex::http", import_http(factory, allocator)),
        ("reflex::loader", import_loader(factory, allocator)),
        ("reflex::utils", import_utils(factory, allocator)),
    ]
}

pub fn builtin_imports_loader<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> impl Fn(&str, &Path) -> Option<Result<T, String>>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    static_module_loader(builtin_imports(factory, allocator))
}