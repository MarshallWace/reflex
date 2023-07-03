// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use reflex::core::{Builtin, Expression, ExpressionFactory, HeapAllocator};
use reflex_stdlib::*;

use crate::stdlib::*;

pub mod core;
pub mod utils;

pub use self::core::import_core;
pub use self::utils::import_utils;

pub trait JsImportsBuiltin:
    Builtin
    + From<Abs>
    + From<Add>
    + From<And>
    + From<Apply>
    + From<Car>
    + From<Cdr>
    + From<Ceil>
    + From<Chain>
    + From<CollectHashMap>
    + From<CollectHashSet>
    + From<CollectList>
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
    + From<Eq>
    + From<Equal>
    + From<Filter>
    + From<Flatten>
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
    + From<Max>
    + From<Merge>
    + From<Min>
    + From<Multiply>
    + From<Not>
    + From<Or>
    + From<Pow>
    + From<Push>
    + From<PushFront>
    + From<Raise>
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
    + From<Subtract>
    + From<Unzip>
    + From<Values>
    + From<Zip>
{
}
impl<T> JsImportsBuiltin for T where
    T: Builtin
        + From<Abs>
        + From<Add>
        + From<And>
        + From<Apply>
        + From<Car>
        + From<Cdr>
        + From<Ceil>
        + From<Chain>
        + From<CollectHashMap>
        + From<CollectHashSet>
        + From<CollectList>
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
        + From<Eq>
        + From<Equal>
        + From<Filter>
        + From<Flatten>
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
        + From<Max>
        + From<Merge>
        + From<Min>
        + From<Multiply>
        + From<Not>
        + From<Or>
        + From<Pow>
        + From<Push>
        + From<PushFront>
        + From<Raise>
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
        + From<Subtract>
        + From<Unzip>
        + From<Values>
        + From<Zip>
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
