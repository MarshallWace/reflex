// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use reflex::core::{Builtin, Expression, ExpressionFactory, HeapAllocator};
use reflex_stdlib::*;

use crate::stdlib::*;

pub(crate) mod core;
pub(crate) mod utils;

pub use self::core::import_core;
pub use self::utils::import_utils;

pub trait JsImportsBuiltin:
    Builtin
    + From<Abs>
    + From<Add>
    + From<And>
    + From<Append>
    + From<Apply>
    + From<Car>
    + From<Cdr>
    + From<Ceil>
    + From<Collect>
    + From<CollectHashMap>
    + From<CollectHashSet>
    + From<CollectList>
    + From<CollectRecord>
    + From<Concat>
    + From<Cons>
    + From<ConstructHashMap>
    + From<ConstructHashSet>
    + From<ConstructList>
    + From<ConstructRecord>
    + From<Contains>
    + From<Divide>
    + From<Effect>
    + From<EndsWith>
    + From<Entries>
    + From<Eq>
    + From<Equal>
    + From<Filter>
    + From<Floor>
    + From<Get>
    + From<Gt>
    + From<Gte>
    + From<Hash>
    + From<If>
    + From<IfError>
    + From<IfPending>
    + From<Insert>
    + From<Keys>
    + From<Length>
    + From<Log>
    + From<Lt>
    + From<Lte>
    + From<Map>
    + From<Match>
    + From<Max>
    + From<Merge>
    + From<Min>
    + From<Multiply>
    + From<Not>
    + From<Or>
    + From<Pow>
    + From<Push>
    + From<PushFront>
    + From<Reduce>
    + From<Remainder>
    + From<Replace>
    + From<ResolveArgs>
    + From<ResolveDeep>
    + From<ResolveHashMap>
    + From<ResolveHashSet>
    + From<ResolveList>
    + From<ResolveRecord>
    + From<ResolveShallow>
    + From<Round>
    + From<Sequence>
    + From<Slice>
    + From<Split>
    + From<StartsWith>
    + From<StructTypeFactory>
    + From<Subtract>
    + From<Values>
{
}
impl<T> JsImportsBuiltin for T where
    T: Builtin
        + From<Abs>
        + From<Add>
        + From<And>
        + From<Append>
        + From<Apply>
        + From<Car>
        + From<Cdr>
        + From<Ceil>
        + From<Collect>
        + From<CollectHashMap>
        + From<CollectHashSet>
        + From<CollectList>
        + From<CollectRecord>
        + From<Concat>
        + From<Cons>
        + From<ConstructHashMap>
        + From<ConstructHashSet>
        + From<ConstructList>
        + From<ConstructRecord>
        + From<Contains>
        + From<Divide>
        + From<Effect>
        + From<EndsWith>
        + From<Entries>
        + From<Eq>
        + From<Equal>
        + From<Filter>
        + From<Floor>
        + From<Get>
        + From<Gt>
        + From<Gte>
        + From<Hash>
        + From<If>
        + From<IfError>
        + From<IfPending>
        + From<Insert>
        + From<Keys>
        + From<Length>
        + From<Log>
        + From<Lt>
        + From<Lte>
        + From<Map>
        + From<Match>
        + From<Max>
        + From<Merge>
        + From<Min>
        + From<Multiply>
        + From<Not>
        + From<Or>
        + From<Pow>
        + From<Push>
        + From<PushFront>
        + From<Reduce>
        + From<Remainder>
        + From<Replace>
        + From<ResolveArgs>
        + From<ResolveDeep>
        + From<ResolveHashMap>
        + From<ResolveHashSet>
        + From<ResolveList>
        + From<ResolveRecord>
        + From<ResolveShallow>
        + From<Round>
        + From<Sequence>
        + From<Slice>
        + From<Split>
        + From<StartsWith>
        + From<StructTypeFactory>
        + From<Subtract>
        + From<Values>
{
}

pub fn builtin_imports<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Vec<(&'static str, T)>
where
    T::Builtin: JsImportsBuiltin,
{
    vec![
        ("reflex::core", import_core(factory, allocator)),
        ("reflex::utils", import_utils(factory, allocator)),
    ]
}
