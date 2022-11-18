// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use crate::named::create_generic_arguments_for_params;
use proc_macro::TokenStream;
use quote::quote_spanned;
use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::{ItemImpl, ItemTrait};

pub fn execute(input: TokenStream) -> TokenStream {
    let mut input = input;
    let impl_block: TokenStream = syn::parse(input.clone())
        .and_then(|ast: ItemTrait| {
            let trait_name = &ast.ident;
            let trait_bounds = &ast.supertraits;
            let generic_params = &ast.generics.params;
            let generic_args = create_generic_arguments_for_params(ast.generics.params.iter()).collect::<Punctuated<_, Comma>>();
            let where_clause = &ast.generics.where_clause;
            let span = trait_name.span();
            let dummy_impl_block: ItemImpl = syn::parse(TokenStream::from(quote_spanned! {
                span => impl<_Self> Foo for _Self where _Self: #trait_bounds {}
            }))?;
            let mut self_where_clause = dummy_impl_block.generics.where_clause
                .expect("Infallible parsing in macro. If this fails: programmer error in reflex-macros");
            if let Some(where_clause) = where_clause {
                self_where_clause.predicates.extend(where_clause.predicates.clone());
            }
            Ok(quote_spanned! { span =>
                impl<_Self, #generic_params> #trait_name<#generic_args> for _Self #self_where_clause {}
            }.into())
        })
        .unwrap_or_else(|err| err.to_compile_error().into());

    input.extend(impl_block);
    input
}
