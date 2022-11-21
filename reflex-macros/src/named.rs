// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::utils;
use proc_macro::TokenStream;
use quote::quote;
use syn::{punctuated::Punctuated, token::Comma};

pub fn execute(input: TokenStream) -> TokenStream {
    syn::parse(input)
        .map(|ast: syn::DeriveInput| {
            let name = &ast.ident;
            let generic_params = &ast.generics.params;
            let generic_args =
                utils::create_generic_arguments_for_params(ast.generics.params.iter())
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
