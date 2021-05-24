// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{collections::HashMap, path::Path};

use reflex::core::Expression;

pub fn static_module_loader<'a>(
    modules: impl IntoIterator<Item = (&'a str, Expression)>,
) -> impl Fn(&str, &Path) -> Result<Expression, String> {
    let modules = modules
        .into_iter()
        .map(|(key, value)| (String::from(key), value))
        .collect::<HashMap<_, _>>();
    move |import_path: &str, _: &Path| match modules.get(import_path) {
        Some(value) => Ok(Expression::clone(value)),
        None => Err(format!("Module not found: {}", import_path)),
    }
}

pub fn dynamic_module_loader<'a>(
    loaders: impl IntoIterator<Item = impl Fn(&str, &Path) -> Option<Result<Expression, String>>>,
    static_modules: Option<impl IntoIterator<Item = (&'a str, Expression)>>,
) -> impl Fn(&str, &Path) -> Result<Expression, String> {
    let loaders = loaders.into_iter().collect::<Vec<_>>();
    let static_modules = static_modules.map(|modules| {
        modules
            .into_iter()
            .map(|(key, value)| (String::from(key), value))
            .collect::<HashMap<_, _>>()
    });
    // TODO: resolve absolute path before loading
    move |import_path: &str, module_path: &Path| {
        loaders
            .iter()
            .find_map(|loader| match loader(import_path, module_path) {
                Some(Err(error)) => Some(Err(format!(
                    "Failed to load module {}: {}",
                    import_path, error
                ))),
                result => result,
            })
            .or_else(|| match &static_modules {
                None => None,
                Some(static_modules) => match static_modules.get(import_path) {
                    Some(value) => Some(Ok(Expression::clone(value))),
                    None => None,
                },
            })
            .unwrap_or_else(|| Err(format!("Module not found: {}", import_path)))
    }
}
