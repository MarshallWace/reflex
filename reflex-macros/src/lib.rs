// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use proc_macro::TokenStream;

mod blanket_trait;
mod dispatcher;
mod matcher;
mod named;
mod task_factory_enum;
mod utils;

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

#[proc_macro]
pub fn blanket_trait(input: TokenStream) -> TokenStream {
    blanket_trait::execute(input)
}

#[proc_macro]
pub fn task_factory_enum(input: TokenStream) -> TokenStream {
    task_factory_enum::execute(input)
}
