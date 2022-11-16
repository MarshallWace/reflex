// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use proc_macro::TokenStream;

mod dispatcher;
mod matcher;
mod named;

#[proc_macro_derive(Matcher, attributes(matcher))]
pub fn derive_matcher(input: TokenStream) -> TokenStream {
    matcher::execute(input)
}

#[proc_macro_derive(Named)]
pub fn named_matcher(input: TokenStream) -> TokenStream {
    named::execute(input)
}

#[proc_macro]
pub fn dispatcher(input: TokenStream) -> TokenStream {
    dispatcher::execute(input)
}
