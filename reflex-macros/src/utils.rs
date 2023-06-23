// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use proc_macro2::{Ident, Span};
use syn::spanned::Spanned;
use syn::{
    Error, GenericArgument, GenericParam, Item, ItemEnum, ItemImpl, Path, PathSegment, Stmt, Type,
    TypePath,
};

/// Arguments vs params:
/// struct MyStruct<X> {}
/// impl<GENERIC_PARAMETERS> Foo for MyStruct<GENERIC_ARGUMENTS>
pub(crate) fn create_generic_arguments_for_params<'a>(
    params: impl IntoIterator<
        Item = &'a GenericParam,
        IntoIter = impl Iterator<Item = &'a GenericParam> + 'a,
    >,
) -> impl Iterator<Item = GenericArgument> + 'a {
    params.into_iter().map(|param| match param {
        GenericParam::Lifetime(lifetime) => GenericArgument::Lifetime(lifetime.lifetime.clone()),
        GenericParam::Type(param) => create_generic_argument_for_ident(&param.ident),
        GenericParam::Const(param) => create_generic_argument_for_ident(&param.ident),
    })
}

pub(crate) fn create_generic_argument_for_ident(ident: &Ident) -> GenericArgument {
    GenericArgument::Type(Type::Path(TypePath {
        qself: None,
        path: Path {
            leading_colon: None,
            segments: [PathSegment {
                ident: ident.clone(),
                arguments: Default::default(),
            }]
            .into_iter()
            .collect(),
        },
    }))
}

pub(crate) fn parse_item_enum(
    stmts: &mut impl Iterator<Item = Stmt>,
    span: Span,
) -> syn::Result<ItemEnum> {
    match stmts.next() {
        Some(stmt) => match stmt {
            Stmt::Item(item) => match item {
                Item::Enum(item) => Ok(item),
                item => Err(item.span()),
            },
            stmt => Err(stmt.span()),
        },
        None => Err(span),
    }
    .map_err(|span| Error::new(span, "Expected enum definition"))
}

pub(crate) fn parse_item_impl(
    stmts: &mut impl Iterator<Item = Stmt>,
    span: Span,
) -> syn::Result<ItemImpl> {
    match stmts.next() {
        Some(stmt) => match stmt {
            Stmt::Item(item) => match item {
                Item::Impl(item) => Ok(item),
                item => Err(item.span()),
            },
            stmt => Err(stmt.span()),
        },
        None => Err(span),
    }
    .map_err(|span| Error::new(span, "Expected impl block"))
}
