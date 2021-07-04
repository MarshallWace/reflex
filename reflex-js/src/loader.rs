// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    cell::RefCell,
    collections::HashMap,
    fs::{self, Metadata},
    io,
    path::{Path, PathBuf},
    rc::Rc,
};

use crate::{
    parse_module,
    stdlib::{builtin_globals, global_process},
    Env,
};
use reflex::{
    core::{Expression, StructPrototype, StructTerm, Term},
    stdlib::value::{StringValue, ValueTerm},
};

pub fn create_module_loader(
    env: Env,
    custom_loader: Option<impl Fn(&str, &Path) -> Option<Result<Expression, String>> + 'static>,
) -> impl Fn(&str, &Path) -> Option<Result<Expression, String>> {
    recursive_module_loader(move |loader| {
        let js_loader = create_js_loader(env, loader);
        move |import_path: &str, module_path: &Path| match js_loader(import_path, module_path) {
            Some(result) => Some(result),
            None => match custom_loader
                .as_ref()
                .and_then(|custom_loader| custom_loader(import_path, module_path))
            {
                Some(result) => Some(result),
                None => error_module_loader(import_path, module_path),
            },
        }
    })
}

pub fn error_module_loader(
    import_path: &str,
    module_path: &Path,
) -> Option<Result<Expression, String>> {
    Some(Err(
        match get_module_path_metadata(import_path, module_path) {
            Ok(Some(metadata)) => match metadata.is_dir() {
                true => String::from("Module path is a directory"),
                false => String::from("No compatible loaders"),
            },
            Ok(None) => String::from("Module not found"),
            Err(error) => error,
        },
    ))
}

fn recursive_module_loader<TResult>(
    factory: impl FnOnce(Box<dyn Fn(&str, &Path) -> Option<Result<Expression, String>>>) -> TResult,
) -> impl Fn(&str, &Path) -> Option<Result<Expression, String>>
where
    TResult: Fn(&str, &Path) -> Option<Result<Expression, String>> + 'static,
{
    fn placeholder_loader(_: &str, _: &Path) -> Option<Result<Expression, String>> {
        Some(Err(String::from("Module loader not yet initialized")))
    }

    let loader = Rc::new(RefCell::<
        Box<dyn Fn(&str, &Path) -> Option<Result<Expression, String>> + 'static>,
    >::new(Box::new(placeholder_loader)));
    let inner_loader = loader.clone();
    let result = factory(Box::new(move |import_path, module_path| {
        let load = inner_loader.borrow();
        load(import_path, module_path)
    }));
    *loader.borrow_mut() = Box::new(result);
    move |import_path, module_path| (loader.borrow())(import_path, module_path)
}

pub fn static_module_loader<'a>(
    modules: impl IntoIterator<Item = (&'a str, Expression)>,
) -> impl Fn(&str, &Path) -> Option<Result<Expression, String>> {
    let modules = modules
        .into_iter()
        .map(|(key, value)| (String::from(key), value))
        .collect::<HashMap<_, _>>();
    move |import_path: &str, _: &Path| match modules.get(import_path) {
        Some(value) => Some(Ok(Expression::clone(value))),
        None => None,
    }
}

pub fn compose_module_loaders(
    head: impl Fn(&str, &Path) -> Option<Result<Expression, String>> + 'static,
    tail: impl Fn(&str, &Path) -> Option<Result<Expression, String>> + 'static,
) -> impl Fn(&str, &Path) -> Option<Result<Expression, String>> {
    move |import_path: &str, module_path: &Path| match head(import_path, module_path) {
        Some(result) => Some(result),
        None => tail(import_path, module_path),
    }
}

pub fn create_js_loader(
    env: Env,
    module_loader: impl Fn(&str, &Path) -> Option<Result<Expression, String>>,
) -> impl Fn(&str, &Path) -> Option<Result<Expression, String>> {
    move |import_path: &str, module_path: &Path| {
        if !import_path.ends_with(".js") {
            return None;
        }
        let target_path = get_module_filesystem_path(import_path, module_path);
        Some(match fs::read_to_string(&target_path) {
            Err(error) => Err(format!("{}", error)),
            Ok(source) => parse_module(&source, &env, &target_path, &module_loader)
                .map(create_default_module_export),
        })
    }
}

pub fn get_module_filesystem_path(import_path: &str, module_path: &Path) -> PathBuf {
    module_path
        .parent()
        .map(|parent| parent.join(import_path))
        .unwrap_or_else(|| Path::new(import_path).to_path_buf())
}

pub fn create_js_env(env_vars: impl IntoIterator<Item = (String, String)>) -> Env {
    Env::new().with_globals(builtin_globals()).with_global(
        "process",
        global_process(
            env_vars
                .into_iter()
                .map(|(key, value)| (key, Expression::new(Term::Value(ValueTerm::String(value))))),
        ),
    )
}

fn create_default_module_export(value: Expression) -> Expression {
    Expression::new(Term::Struct(StructTerm::new(
        Some(StructPrototype::new(vec![ValueTerm::String(
            StringValue::from("default"),
        )])),
        vec![value],
    )))
}

fn get_module_path_metadata(
    import_path: &str,
    module_path: &Path,
) -> Result<Option<Metadata>, String> {
    match fs::metadata(get_module_filesystem_path(import_path, module_path)) {
        Ok(metadata) => Ok(Some(metadata)),
        Err(error) => match error.kind() {
            io::ErrorKind::NotFound => Ok(None),
            _ => Err(format!("{}", error)),
        },
    }
}
