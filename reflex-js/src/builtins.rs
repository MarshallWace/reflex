// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::convert::{TryFrom, TryInto};

use reflex::core::Uuid;
use reflex::core::{
    Applicable, Arity, Builtin, EvaluationCache, Expression, ExpressionFactory, HeapAllocator, Uid,
};
use reflex_stdlib::stdlib;
use serde::{Deserialize, Serialize};

use crate as reflex_js;

#[derive(Clone, Copy, Eq, PartialEq, Hash, Debug, Serialize, Deserialize)]
pub enum JsBuiltins {
    Stdlib(stdlib::Stdlib),
    Json(reflex_json::stdlib::Stdlib),
    Js(reflex_js::stdlib::Stdlib),
}
impl From<stdlib::Stdlib> for JsBuiltins {
    fn from(builtin: stdlib::Stdlib) -> Self {
        JsBuiltins::Stdlib(builtin)
    }
}
impl From<reflex_json::stdlib::Stdlib> for JsBuiltins {
    fn from(target: reflex_json::stdlib::Stdlib) -> Self {
        JsBuiltins::Json(target)
    }
}
impl From<reflex_js::stdlib::Stdlib> for JsBuiltins {
    fn from(target: reflex_js::stdlib::Stdlib) -> Self {
        JsBuiltins::Js(target)
    }
}
impl JsBuiltins {
    pub fn entries() -> impl Iterator<Item = Self> {
        stdlib::Stdlib::entries()
            .map(Self::Stdlib)
            .chain(reflex_json::stdlib::Stdlib::entries().map(Self::Json))
            .chain(reflex_js::stdlib::Stdlib::entries().map(Self::Js))
    }
}
impl Uid for JsBuiltins {
    fn uid(&self) -> reflex::core::Uuid {
        match self {
            JsBuiltins::Stdlib(term) => term.uid(),
            JsBuiltins::Json(term) => term.uid(),
            JsBuiltins::Js(term) => term.uid(),
        }
    }
}
impl Builtin for JsBuiltins {
    fn arity(&self) -> Arity {
        match self {
            JsBuiltins::Stdlib(term) => term.arity(),
            JsBuiltins::Json(term) => term.arity(),
            JsBuiltins::Js(term) => term.arity(),
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
            JsBuiltins::Stdlib(term) => term.apply(args, factory, allocator, cache),
            JsBuiltins::Json(term) => term.apply(args, factory, allocator, cache),
            JsBuiltins::Js(term) => term.apply(args, factory, allocator, cache),
        }
    }
    fn should_parallelize<T: Expression<Builtin = Self> + Applicable<T>>(
        &self,
        args: &[T],
    ) -> bool {
        match self {
            JsBuiltins::Stdlib(term) => term.should_parallelize(args),
            JsBuiltins::Json(term) => term.should_parallelize(args),
            JsBuiltins::Js(term) => term.should_parallelize(args),
        }
    }
}
impl std::fmt::Display for JsBuiltins {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Stdlib(target) => std::fmt::Display::fmt(target, f),
            Self::Json(target) => std::fmt::Display::fmt(target, f),
            Self::Js(target) => std::fmt::Display::fmt(target, f),
        }
    }
}
impl TryFrom<Uuid> for JsBuiltins {
    type Error = ();
    fn try_from(value: Uuid) -> Result<Self, Self::Error> {
        TryInto::<stdlib::Stdlib>::try_into(value)
            .map(Self::Stdlib)
            .or_else(|_| TryInto::<reflex_json::stdlib::Stdlib>::try_into(value).map(Self::Json))
            .or_else(|_| TryInto::<reflex_js::stdlib::Stdlib>::try_into(value).map(Self::Js))
    }
}

impl From<stdlib::Abs> for JsBuiltins {
    fn from(value: stdlib::Abs) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Add> for JsBuiltins {
    fn from(value: stdlib::Add) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::And> for JsBuiltins {
    fn from(value: stdlib::And) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Apply> for JsBuiltins {
    fn from(value: stdlib::Apply) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Car> for JsBuiltins {
    fn from(value: stdlib::Car) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Cdr> for JsBuiltins {
    fn from(value: stdlib::Cdr) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Ceil> for JsBuiltins {
    fn from(value: stdlib::Ceil) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Chain> for JsBuiltins {
    fn from(value: stdlib::Chain) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::CollectHashMap> for JsBuiltins {
    fn from(value: stdlib::CollectHashMap) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::CollectHashSet> for JsBuiltins {
    fn from(value: stdlib::CollectHashSet) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::CollectList> for JsBuiltins {
    fn from(value: stdlib::CollectList) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Concat> for JsBuiltins {
    fn from(value: stdlib::Concat) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Cons> for JsBuiltins {
    fn from(value: stdlib::Cons) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::ConstructHashMap> for JsBuiltins {
    fn from(value: stdlib::ConstructHashMap) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::ConstructHashSet> for JsBuiltins {
    fn from(value: stdlib::ConstructHashSet) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::ConstructRecord> for JsBuiltins {
    fn from(value: stdlib::ConstructRecord) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::ConstructList> for JsBuiltins {
    fn from(value: stdlib::ConstructList) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Contains> for JsBuiltins {
    fn from(value: stdlib::Contains) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Divide> for JsBuiltins {
    fn from(value: stdlib::Divide) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Effect> for JsBuiltins {
    fn from(value: stdlib::Effect) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::EndsWith> for JsBuiltins {
    fn from(value: stdlib::EndsWith) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Eq> for JsBuiltins {
    fn from(value: stdlib::Eq) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Equal> for JsBuiltins {
    fn from(value: stdlib::Equal) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Filter> for JsBuiltins {
    fn from(value: stdlib::Filter) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Flatten> for JsBuiltins {
    fn from(value: stdlib::Flatten) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Floor> for JsBuiltins {
    fn from(value: stdlib::Floor) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Get> for JsBuiltins {
    fn from(value: stdlib::Get) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Gt> for JsBuiltins {
    fn from(value: stdlib::Gt) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Gte> for JsBuiltins {
    fn from(value: stdlib::Gte) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Hash> for JsBuiltins {
    fn from(value: stdlib::Hash) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::If> for JsBuiltins {
    fn from(value: stdlib::If) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::IfError> for JsBuiltins {
    fn from(value: stdlib::IfError) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::IfPending> for JsBuiltins {
    fn from(value: stdlib::IfPending) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Insert> for JsBuiltins {
    fn from(value: stdlib::Insert) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Keys> for JsBuiltins {
    fn from(value: stdlib::Keys) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Length> for JsBuiltins {
    fn from(value: stdlib::Length) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Lt> for JsBuiltins {
    fn from(value: stdlib::Lt) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Lte> for JsBuiltins {
    fn from(value: stdlib::Lte) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Map> for JsBuiltins {
    fn from(value: stdlib::Map) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Max> for JsBuiltins {
    fn from(value: stdlib::Max) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Merge> for JsBuiltins {
    fn from(value: stdlib::Merge) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Min> for JsBuiltins {
    fn from(value: stdlib::Min) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Multiply> for JsBuiltins {
    fn from(value: stdlib::Multiply) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Not> for JsBuiltins {
    fn from(value: stdlib::Not) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Or> for JsBuiltins {
    fn from(value: stdlib::Or) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Pow> for JsBuiltins {
    fn from(value: stdlib::Pow) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Push> for JsBuiltins {
    fn from(value: stdlib::Push) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::PushFront> for JsBuiltins {
    fn from(value: stdlib::PushFront) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Raise> for JsBuiltins {
    fn from(value: stdlib::Raise) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Reduce> for JsBuiltins {
    fn from(value: stdlib::Reduce) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Remainder> for JsBuiltins {
    fn from(value: stdlib::Remainder) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Replace> for JsBuiltins {
    fn from(value: stdlib::Replace) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::ResolveArgs> for JsBuiltins {
    fn from(value: stdlib::ResolveArgs) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::ResolveDeep> for JsBuiltins {
    fn from(value: stdlib::ResolveDeep) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::ResolveHashMap> for JsBuiltins {
    fn from(value: stdlib::ResolveHashMap) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::ResolveHashSet> for JsBuiltins {
    fn from(value: stdlib::ResolveHashSet) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::ResolveList> for JsBuiltins {
    fn from(value: stdlib::ResolveList) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::ResolveRecord> for JsBuiltins {
    fn from(value: stdlib::ResolveRecord) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::ResolveShallow> for JsBuiltins {
    fn from(value: stdlib::ResolveShallow) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Round> for JsBuiltins {
    fn from(value: stdlib::Round) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Sequence> for JsBuiltins {
    fn from(value: stdlib::Sequence) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Slice> for JsBuiltins {
    fn from(value: stdlib::Slice) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Split> for JsBuiltins {
    fn from(value: stdlib::Split) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::StartsWith> for JsBuiltins {
    fn from(value: stdlib::StartsWith) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Subtract> for JsBuiltins {
    fn from(value: stdlib::Subtract) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Unzip> for JsBuiltins {
    fn from(value: stdlib::Unzip) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Values> for JsBuiltins {
    fn from(value: stdlib::Values) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Zip> for JsBuiltins {
    fn from(value: stdlib::Zip) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}

impl From<reflex_json::stdlib::JsonDeserialize> for JsBuiltins {
    fn from(value: reflex_json::stdlib::JsonDeserialize) -> Self {
        Self::from(reflex_json::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_json::stdlib::JsonSerialize> for JsBuiltins {
    fn from(value: reflex_json::stdlib::JsonSerialize) -> Self {
        Self::from(reflex_json::stdlib::Stdlib::from(value))
    }
}

impl From<reflex_js::stdlib::Accessor> for JsBuiltins {
    fn from(value: reflex_js::stdlib::Accessor) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_js::stdlib::Construct> for JsBuiltins {
    fn from(value: reflex_js::stdlib::Construct) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_js::stdlib::DateConstructor> for JsBuiltins {
    fn from(value: reflex_js::stdlib::DateConstructor) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_js::stdlib::EncodeUriComponent> for JsBuiltins {
    fn from(value: reflex_js::stdlib::EncodeUriComponent) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_js::stdlib::FormatErrorMessage> for JsBuiltins {
    fn from(value: reflex_js::stdlib::FormatErrorMessage) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_js::stdlib::IsFinite> for JsBuiltins {
    fn from(value: reflex_js::stdlib::IsFinite) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_js::stdlib::Log> for JsBuiltins {
    fn from(value: reflex_js::stdlib::Log) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_js::stdlib::LogArgs> for JsBuiltins {
    fn from(value: reflex_js::stdlib::LogArgs) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_js::stdlib::ParseFloat> for JsBuiltins {
    fn from(value: reflex_js::stdlib::ParseFloat) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_js::stdlib::ParseInt> for JsBuiltins {
    fn from(value: reflex_js::stdlib::ParseInt) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_js::stdlib::Throw> for JsBuiltins {
    fn from(value: reflex_js::stdlib::Throw) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_js::stdlib::ToString> for JsBuiltins {
    fn from(value: reflex_js::stdlib::ToString) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}

#[cfg(test)]
mod test {
    use reflex::core::ExpressionFactory;
    use reflex_lang::{allocator::DefaultAllocator, CachedSharedTerm, SharedTermFactory};

    use crate::{builtins::JsBuiltins, create_js_env, parse};

    #[test]
    fn round_trip_serde() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();

        let value = factory.create_int_term(5);
        let serialized = serde_json::to_string(&value).unwrap();
        let deser: CachedSharedTerm<JsBuiltins> = serde_json::from_str(&serialized).unwrap();
        assert_eq!(value, deser);

        let value = parse(
            "const x = () => {return 1 + 1;}; throw x",
            &create_js_env(&factory, &allocator),
            &factory,
            &allocator,
        )
        .unwrap();
        let serialized = serde_json::to_string(&value).unwrap();
        let deser: CachedSharedTerm<JsBuiltins> = serde_json::from_str(&serialized).unwrap();
        assert_eq!(value, deser);
    }
}
