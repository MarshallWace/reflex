// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    cell::RefCell,
    collections::HashMap,
    fs::{self, Metadata},
    io,
    iter::once,
    path::{Path, PathBuf},
    rc::Rc,
};

use crate::{
    globals::{builtin_globals, JsGlobalsBuiltin},
    parse_module,
    parser::JsParserBuiltin,
    Env,
};
use reflex::core::{Expression, ExpressionFactory, HeapAllocator, Rewritable};

pub fn create_module_loader<T: Expression + Rewritable<T> + 'static>(
    env: Env<T>,
    custom_loader: Option<impl Fn(&str, &Path) -> Option<Result<T, String>> + 'static>,
    factory: &(impl ExpressionFactory<T> + Clone + 'static),
    allocator: &(impl HeapAllocator<T> + Clone + 'static),
) -> impl Fn(&str, &Path) -> Option<Result<T, String>>
where
    T::Builtin: JsParserBuiltin,
{
    let factory = factory.clone();
    let allocator = allocator.clone();
    recursive_module_loader(move |loader| {
        let js_loader = create_js_loader(env, loader, &factory, &allocator);
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

pub fn error_module_loader<T: Expression>(
    import_path: &str,
    module_path: &Path,
) -> Option<Result<T, String>> {
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

fn recursive_module_loader<T: Expression + 'static, TResult>(
    create_loader: impl FnOnce(Box<dyn Fn(&str, &Path) -> Option<Result<T, String>>>) -> TResult,
) -> impl Fn(&str, &Path) -> Option<Result<T, String>>
where
    TResult: Fn(&str, &Path) -> Option<Result<T, String>> + 'static,
{
    fn placeholder_loader<T: Expression>(_: &str, _: &Path) -> Option<Result<T, String>> {
        Some(Err(String::from("Module loader not yet initialized")))
    }

    let loader = Rc::new(RefCell::<
        Box<dyn Fn(&str, &Path) -> Option<Result<T, String>> + 'static>,
    >::new(Box::new(placeholder_loader)));
    let inner_loader = loader.clone();
    let result = create_loader(Box::new(move |import_path, module_path| {
        let load = inner_loader.borrow();
        load(import_path, module_path)
    }));
    *loader.borrow_mut() = Box::new(result);
    move |import_path, module_path| (loader.borrow())(import_path, module_path)
}

pub fn static_module_loader<'a, T: Expression>(
    modules: impl IntoIterator<Item = (&'a str, T)>,
) -> impl Fn(&str, &Path) -> Option<Result<T, String>> {
    let modules = modules
        .into_iter()
        .map(|(key, value)| (String::from(key), value))
        .collect::<HashMap<_, _>>();
    move |import_path: &str, _: &Path| match modules.get(import_path) {
        Some(value) => Some(Ok(value.clone())),
        None => None,
    }
}

pub fn compose_module_loaders<T: Expression>(
    head: impl Fn(&str, &Path) -> Option<Result<T, String>> + 'static,
    tail: impl Fn(&str, &Path) -> Option<Result<T, String>> + 'static,
) -> impl Fn(&str, &Path) -> Option<Result<T, String>> {
    move |import_path: &str, module_path: &Path| match head(import_path, module_path) {
        Some(result) => Some(result),
        None => tail(import_path, module_path),
    }
}

pub fn create_js_loader<T: Expression + Rewritable<T> + 'static>(
    env: Env<T>,
    module_loader: impl Fn(&str, &Path) -> Option<Result<T, String>>,
    factory: &(impl ExpressionFactory<T> + Clone),
    allocator: &(impl HeapAllocator<T> + Clone),
) -> impl Fn(&str, &Path) -> Option<Result<T, String>>
where
    T::Builtin: JsParserBuiltin,
{
    let factory = factory.clone();
    let allocator = allocator.clone();
    move |import_path: &str, module_path: &Path| {
        if !import_path.ends_with(".js") {
            return None;
        }
        let target_path = get_module_filesystem_path(import_path, module_path);
        Some(match fs::read_to_string(&target_path) {
            Err(error) => Err(format!("{}", error)),
            Ok(source) => parse_module(
                &source,
                &env,
                &target_path,
                &module_loader,
                &factory,
                &allocator,
            )
            .map(|result| create_default_module_export(result, &factory, &allocator)),
        })
    }
}

pub fn get_module_filesystem_path(import_path: &str, module_path: &Path) -> PathBuf {
    module_path
        .parent()
        .map(|parent| parent.join(import_path))
        .unwrap_or_else(|| Path::new(import_path).to_path_buf())
}

pub fn create_js_env<T: Expression + Rewritable<T>>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Env<T>
where
    T::Builtin: JsGlobalsBuiltin,
{
    Env::new().with_globals(builtin_globals(factory, allocator))
}

fn create_default_module_export<T: Expression>(
    value: T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_record_term(
        allocator.create_struct_prototype(allocator.create_list(once(
            factory.create_string_term(allocator.create_string(String::from("default"))),
        ))),
        allocator.create_unit_list(value),
    )
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
