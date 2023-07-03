// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{
    convert::{TryFrom, TryInto},
    iter::empty,
};

use reflex::core::{
    Applicable, Arity, Builtin, EvaluationCache, Expression, ExpressionFactory, HeapAllocator, Uid,
    Uuid,
};
use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Eq, PartialEq, Hash, Debug, Serialize, Deserialize)]
pub enum ServerBuiltins {
    Stdlib(reflex_stdlib::stdlib::Stdlib),
    Json(reflex_json::stdlib::Stdlib),
    Js(reflex_js::stdlib::Stdlib),
    GraphQl(reflex_graphql::stdlib::Stdlib),
    Handlers(reflex_handlers::stdlib::Stdlib),
}
impl From<reflex_stdlib::stdlib::Stdlib> for ServerBuiltins {
    fn from(target: reflex_stdlib::stdlib::Stdlib) -> Self {
        Self::Stdlib(target)
    }
}
impl From<reflex_json::stdlib::Stdlib> for ServerBuiltins {
    fn from(target: reflex_json::stdlib::Stdlib) -> Self {
        Self::Json(target)
    }
}
impl From<reflex_js::stdlib::Stdlib> for ServerBuiltins {
    fn from(target: reflex_js::stdlib::Stdlib) -> Self {
        Self::Js(target)
    }
}
impl From<reflex_graphql::stdlib::Stdlib> for ServerBuiltins {
    fn from(target: reflex_graphql::stdlib::Stdlib) -> Self {
        Self::GraphQl(target)
    }
}
impl From<reflex_handlers::stdlib::Stdlib> for ServerBuiltins {
    fn from(target: reflex_handlers::stdlib::Stdlib) -> Self {
        Self::Handlers(target)
    }
}
impl ServerBuiltins {
    pub fn entries() -> impl Iterator<Item = Self> {
        empty()
            .chain(reflex_stdlib::stdlib::Stdlib::entries().map(Self::Stdlib))
            .chain(reflex_json::stdlib::Stdlib::entries().map(Self::Json))
            .chain(reflex_js::stdlib::Stdlib::entries().map(Self::Js))
            .chain(reflex_graphql::stdlib::Stdlib::entries().map(Self::GraphQl))
            .chain(reflex_handlers::stdlib::Stdlib::entries().map(Self::Handlers))
    }
}
impl Uid for ServerBuiltins {
    fn uid(&self) -> reflex::core::Uuid {
        match self {
            Self::Stdlib(term) => term.uid(),
            Self::Json(term) => term.uid(),
            Self::Js(term) => term.uid(),
            Self::GraphQl(term) => term.uid(),
            Self::Handlers(term) => term.uid(),
        }
    }
}
impl TryFrom<Uuid> for ServerBuiltins {
    type Error = ();
    fn try_from(value: Uuid) -> Result<Self, Self::Error> {
        Err(())
            .or_else(|_| {
                TryInto::<reflex_stdlib::stdlib::Stdlib>::try_into(value).map(Self::Stdlib)
            })
            .or_else(|_| TryInto::<reflex_json::stdlib::Stdlib>::try_into(value).map(Self::Json))
            .or_else(|_| TryInto::<reflex_js::stdlib::Stdlib>::try_into(value).map(Self::Js))
            .or_else(|_| {
                TryInto::<reflex_graphql::stdlib::Stdlib>::try_into(value).map(Self::GraphQl)
            })
            .or_else(|_| {
                TryInto::<reflex_handlers::stdlib::Stdlib>::try_into(value).map(Self::Handlers)
            })
    }
}
impl Builtin for ServerBuiltins {
    fn arity(&self) -> Arity {
        match self {
            Self::Stdlib(term) => term.arity(),
            Self::Json(term) => term.arity(),
            Self::Js(term) => term.arity(),
            Self::GraphQl(term) => term.arity(),
            Self::Handlers(term) => term.arity(),
        }
    }
    fn apply<T: Expression<Builtin = Self> + Applicable<T>>(
        &self,
        args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        match self {
            Self::Stdlib(term) => term.apply(args, factory, allocator, cache),
            Self::Json(term) => term.apply(args, factory, allocator, cache),
            Self::Js(term) => term.apply(args, factory, allocator, cache),
            Self::GraphQl(term) => term.apply(args, factory, allocator, cache),
            Self::Handlers(term) => term.apply(args, factory, allocator, cache),
        }
    }
    fn should_parallelize<T: Expression<Builtin = Self> + Applicable<T>>(
        &self,
        args: &[T],
    ) -> bool {
        match self {
            Self::Stdlib(term) => term.should_parallelize(args),
            Self::Json(term) => term.should_parallelize(args),
            Self::Js(term) => term.should_parallelize(args),
            Self::GraphQl(term) => term.should_parallelize(args),
            Self::Handlers(term) => term.should_parallelize(args),
        }
    }
}
impl std::fmt::Display for ServerBuiltins {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Stdlib(target) => std::fmt::Display::fmt(target, f),
            Self::Json(target) => std::fmt::Display::fmt(target, f),
            Self::Js(target) => std::fmt::Display::fmt(target, f),
            Self::GraphQl(target) => std::fmt::Display::fmt(target, f),
            Self::Handlers(target) => std::fmt::Display::fmt(target, f),
        }
    }
}

impl From<reflex_stdlib::stdlib::Abs> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Abs) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Add> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Add) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::And> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::And) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Apply> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Apply) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Car> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Car) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Cdr> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Cdr) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Ceil> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Ceil) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Chain> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Chain) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::CollectHashMap> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::CollectHashMap) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::CollectHashSet> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::CollectHashSet) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::CollectList> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::CollectList) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Concat> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Concat) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Cons> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Cons) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::ConstructHashMap> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::ConstructHashMap) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::ConstructHashSet> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::ConstructHashSet) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::ConstructRecord> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::ConstructRecord) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::ConstructList> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::ConstructList) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Contains> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Contains) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Divide> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Divide) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Effect> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Effect) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::EndsWith> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::EndsWith) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Eq> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Eq) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Equal> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Equal) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Filter> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Filter) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Flatten> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Flatten) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Floor> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Floor) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Get> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Get) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Gt> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Gt) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Gte> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Gte) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Hash> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Hash) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::If> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::If) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::IfError> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::IfError) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::IfPending> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::IfPending) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Insert> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Insert) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Keys> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Keys) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Length> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Length) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Lt> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Lt) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Lte> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Lte) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Map> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Map) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Max> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Max) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Merge> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Merge) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Min> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Min) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Multiply> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Multiply) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Not> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Not) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Or> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Or) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Pow> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Pow) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Push> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Push) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::PushFront> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::PushFront) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Raise> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Raise) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Reduce> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Reduce) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Remainder> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Remainder) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Replace> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Replace) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::ResolveArgs> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::ResolveArgs) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::ResolveDeep> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::ResolveDeep) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::ResolveHashMap> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::ResolveHashMap) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::ResolveHashSet> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::ResolveHashSet) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::ResolveShallow> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::ResolveShallow) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::ResolveRecord> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::ResolveRecord) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::ResolveList> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::ResolveList) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Round> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Round) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Sequence> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Sequence) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Slice> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Slice) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Split> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Split) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::StartsWith> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::StartsWith) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Subtract> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Subtract) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Unzip> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Unzip) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Values> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Values) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_stdlib::stdlib::Zip> for ServerBuiltins {
    fn from(value: reflex_stdlib::stdlib::Zip) -> Self {
        Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
    }
}

impl From<reflex_json::stdlib::JsonDeserialize> for ServerBuiltins {
    fn from(value: reflex_json::stdlib::JsonDeserialize) -> Self {
        Self::from(reflex_json::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_json::stdlib::JsonSerialize> for ServerBuiltins {
    fn from(value: reflex_json::stdlib::JsonSerialize) -> Self {
        Self::from(reflex_json::stdlib::Stdlib::from(value))
    }
}

impl From<reflex_js::stdlib::Accessor> for ServerBuiltins {
    fn from(value: reflex_js::stdlib::Accessor) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_js::stdlib::Construct> for ServerBuiltins {
    fn from(value: reflex_js::stdlib::Construct) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_js::stdlib::DateConstructor> for ServerBuiltins {
    fn from(value: reflex_js::stdlib::DateConstructor) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_js::stdlib::EncodeUriComponent> for ServerBuiltins {
    fn from(value: reflex_js::stdlib::EncodeUriComponent) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_js::stdlib::FormatErrorMessage> for ServerBuiltins {
    fn from(value: reflex_js::stdlib::FormatErrorMessage) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_js::stdlib::IsFinite> for ServerBuiltins {
    fn from(value: reflex_js::stdlib::IsFinite) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_js::stdlib::Log> for ServerBuiltins {
    fn from(value: reflex_js::stdlib::Log) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_js::stdlib::LogArgs> for ServerBuiltins {
    fn from(value: reflex_js::stdlib::LogArgs) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_js::stdlib::ParseFloat> for ServerBuiltins {
    fn from(value: reflex_js::stdlib::ParseFloat) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_js::stdlib::ParseInt> for ServerBuiltins {
    fn from(value: reflex_js::stdlib::ParseInt) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_js::stdlib::Throw> for ServerBuiltins {
    fn from(value: reflex_js::stdlib::Throw) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_js::stdlib::ToString> for ServerBuiltins {
    fn from(value: reflex_js::stdlib::ToString) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}

impl From<reflex_graphql::stdlib::CollectQueryListItems> for ServerBuiltins {
    fn from(value: reflex_graphql::stdlib::CollectQueryListItems) -> Self {
        Self::from(reflex_graphql::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_graphql::stdlib::DynamicQueryBranch> for ServerBuiltins {
    fn from(value: reflex_graphql::stdlib::DynamicQueryBranch) -> Self {
        Self::from(reflex_graphql::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_graphql::stdlib::FlattenDeep> for ServerBuiltins {
    fn from(value: reflex_graphql::stdlib::FlattenDeep) -> Self {
        Self::from(reflex_graphql::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_graphql::stdlib::GraphQlResolver> for ServerBuiltins {
    fn from(value: reflex_graphql::stdlib::GraphQlResolver) -> Self {
        Self::from(reflex_graphql::stdlib::Stdlib::from(value))
    }
}

impl From<reflex_handlers::stdlib::Scan> for ServerBuiltins {
    fn from(value: reflex_handlers::stdlib::Scan) -> Self {
        Self::from(reflex_handlers::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_handlers::stdlib::ToRequest> for ServerBuiltins {
    fn from(value: reflex_handlers::stdlib::ToRequest) -> Self {
        Self::from(reflex_handlers::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_handlers::stdlib::GetVariable> for ServerBuiltins {
    fn from(value: reflex_handlers::stdlib::GetVariable) -> Self {
        Self::from(reflex_handlers::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_handlers::stdlib::SetVariable> for ServerBuiltins {
    fn from(value: reflex_handlers::stdlib::SetVariable) -> Self {
        Self::from(reflex_handlers::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_handlers::stdlib::IncrementVariable> for ServerBuiltins {
    fn from(value: reflex_handlers::stdlib::IncrementVariable) -> Self {
        Self::from(reflex_handlers::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_handlers::stdlib::DecrementVariable> for ServerBuiltins {
    fn from(value: reflex_handlers::stdlib::DecrementVariable) -> Self {
        Self::from(reflex_handlers::stdlib::Stdlib::from(value))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use reflex_lang::{allocator::DefaultAllocator, CachedSharedTerm, SharedTermFactory};
    use reflex_lisp::parse;

    #[test]
    fn serialization() {
        let factory = SharedTermFactory::<ServerBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let input = parse("((lambda (foo) (* (+ 2 3) foo)) 2)", &factory, &allocator).unwrap();
        let serialized = serde_json::to_string(&input).unwrap();
        let deserialized: CachedSharedTerm<ServerBuiltins> =
            serde_json::from_str(&serialized).unwrap();
        assert_eq!(deserialized, input);
    }
}
