// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{iter::once, path::Path};

use graphql::graphql_loader;
use reflex::core::Expression;

pub mod graphql;

pub fn builtin_loaders(
) -> impl IntoIterator<Item = impl Fn(&str, &Path) -> Option<Result<Expression, String>>> {
    once(graphql_loader)
}
