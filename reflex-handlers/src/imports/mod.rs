// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{Builtin, Expression, ExpressionFactory, HeapAllocator};
use reflex_json::stdlib::JsonDeserialize;
use reflex_stdlib::{CollectList, Contains, Effect, Get, If, Lt, Map, ResolveDeep};

use crate::stdlib::{
    DecrementVariable, GetVariable, IncrementVariable, Scan, SetVariable, ToRequest,
};

pub(crate) mod http;
pub(crate) mod loader;
pub(crate) mod state;
pub(crate) mod time;

pub use self::http::import_http;
pub use self::loader::import_loader;
pub use self::state::import_state;
pub use self::time::import_time;

pub trait HandlerImportsBuiltin:
    Builtin
    + From<CollectList>
    + From<Contains>
    + From<DecrementVariable>
    + From<Effect>
    + From<Get>
    + From<GetVariable>
    + From<If>
    + From<IncrementVariable>
    + From<JsonDeserialize>
    + From<Lt>
    + From<Map>
    + From<ResolveDeep>
    + From<Scan>
    + From<SetVariable>
    + From<ToRequest>
{
}
impl<T> HandlerImportsBuiltin for T where
    T: Builtin
        + From<CollectList>
        + From<Contains>
        + From<DecrementVariable>
        + From<Effect>
        + From<Get>
        + From<GetVariable>
        + From<If>
        + From<IncrementVariable>
        + From<JsonDeserialize>
        + From<Lt>
        + From<Map>
        + From<ResolveDeep>
        + From<Scan>
        + From<SetVariable>
        + From<ToRequest>
{
}

pub fn handler_imports<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Vec<(&'static str, T)>
where
    T::Builtin: HandlerImportsBuiltin,
{
    vec![
        ("reflex::http", import_http(factory, allocator)),
        ("reflex::loader", import_loader(factory, allocator)),
        ("reflex::state", import_state(factory, allocator)),
        ("reflex::time", import_time(factory, allocator)),
    ]
}
