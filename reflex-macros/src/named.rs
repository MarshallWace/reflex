// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use proc_macro::TokenStream;
use quote::quote;
use syn::{
    punctuated::Punctuated, token::Comma, GenericArgument, GenericParam, Ident, Path, PathSegment,
    Type, TypePath,
};

pub fn execute(input: TokenStream) -> TokenStream {
    syn::parse(input)
        .map(|ast: syn::DeriveInput| {
            let name = &ast.ident;
            let generic_params = &ast.generics.params;
            let generic_args = create_generic_arguments_for_params(ast.generics.params.iter())
                .collect::<Punctuated<_, Comma>>();
            let where_clause = &ast.generics.where_clause;
            TokenStream::from(quote! {
                impl<#generic_params> ::reflex_dispatcher::Named for #name<#generic_args>
                    #where_clause
                {
                    fn name(&self) -> &'static str {
                        stringify!(#name)
                    }
                }
            })
        })
        .unwrap_or_else(|err| err.to_compile_error().into())
}

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

fn create_generic_argument_for_ident(ident: &Ident) -> GenericArgument {
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
