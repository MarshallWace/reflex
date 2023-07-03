// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::convert::{TryFrom, TryInto};

use reflex::core::{
    Applicable, Arity, Builtin, EvaluationCache, Expression, ExpressionFactory, HeapAllocator, Uid,
    Uuid,
};
use reflex_stdlib::stdlib;

#[derive(Clone, Copy, Eq, PartialEq, Hash, Debug)]
pub enum CliBuiltins {
    Stdlib(stdlib::Stdlib),
    Json(reflex_json::stdlib::Stdlib),
    Js(reflex_js::stdlib::Stdlib),
    Handlers(reflex_handlers::stdlib::Stdlib),
}
impl From<stdlib::Stdlib> for CliBuiltins {
    fn from(target: stdlib::Stdlib) -> Self {
        CliBuiltins::Stdlib(target)
    }
}
impl From<reflex_json::stdlib::Stdlib> for CliBuiltins {
    fn from(target: reflex_json::stdlib::Stdlib) -> Self {
        CliBuiltins::Json(target)
    }
}
impl From<reflex_js::stdlib::Stdlib> for CliBuiltins {
    fn from(target: reflex_js::stdlib::Stdlib) -> Self {
        CliBuiltins::Js(target)
    }
}
impl From<reflex_handlers::stdlib::Stdlib> for CliBuiltins {
    fn from(target: reflex_handlers::stdlib::Stdlib) -> Self {
        CliBuiltins::Handlers(target)
    }
}
impl Uid for CliBuiltins {
    fn uid(&self) -> reflex::core::Uuid {
        match self {
            CliBuiltins::Stdlib(term) => term.uid(),
            CliBuiltins::Json(term) => term.uid(),
            CliBuiltins::Js(term) => term.uid(),
            CliBuiltins::Handlers(term) => term.uid(),
        }
    }
}
impl TryFrom<Uuid> for CliBuiltins {
    type Error = ();
    fn try_from(value: Uuid) -> Result<Self, Self::Error> {
        TryInto::<stdlib::Stdlib>::try_into(value)
            .map(Self::Stdlib)
            .or_else(|_| TryInto::<reflex_json::stdlib::Stdlib>::try_into(value).map(Self::Json))
            .or_else(|_| TryInto::<reflex_js::stdlib::Stdlib>::try_into(value).map(Self::Js))
            .or_else(|_| {
                TryInto::<reflex_handlers::stdlib::Stdlib>::try_into(value).map(Self::Handlers)
            })
    }
}
impl Builtin for CliBuiltins {
    fn arity(&self) -> Arity {
        match self {
            CliBuiltins::Stdlib(term) => term.arity(),
            CliBuiltins::Json(term) => term.arity(),
            CliBuiltins::Js(term) => term.arity(),
            CliBuiltins::Handlers(term) => term.arity(),
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
            CliBuiltins::Stdlib(term) => term.apply(args, factory, allocator, cache),
            CliBuiltins::Json(term) => term.apply(args, factory, allocator, cache),
            CliBuiltins::Js(term) => term.apply(args, factory, allocator, cache),
            CliBuiltins::Handlers(term) => term.apply(args, factory, allocator, cache),
        }
    }
    fn should_parallelize<T: Expression<Builtin = Self> + Applicable<T>>(
        &self,
        args: &[T],
    ) -> bool {
        match self {
            CliBuiltins::Stdlib(term) => term.should_parallelize(args),
            CliBuiltins::Json(term) => term.should_parallelize(args),
            CliBuiltins::Js(term) => term.should_parallelize(args),
            CliBuiltins::Handlers(term) => term.should_parallelize(args),
        }
    }
}
impl std::fmt::Display for CliBuiltins {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Stdlib(target) => std::fmt::Display::fmt(target, f),
            Self::Json(target) => std::fmt::Display::fmt(target, f),
            Self::Js(target) => std::fmt::Display::fmt(target, f),
            Self::Handlers(target) => std::fmt::Display::fmt(target, f),
        }
    }
}

impl From<stdlib::Abs> for CliBuiltins {
    fn from(value: stdlib::Abs) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Add> for CliBuiltins {
    fn from(value: stdlib::Add) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::And> for CliBuiltins {
    fn from(value: stdlib::And) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Apply> for CliBuiltins {
    fn from(value: stdlib::Apply) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Car> for CliBuiltins {
    fn from(value: stdlib::Car) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Cdr> for CliBuiltins {
    fn from(value: stdlib::Cdr) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Ceil> for CliBuiltins {
    fn from(value: stdlib::Ceil) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Chain> for CliBuiltins {
    fn from(value: stdlib::Chain) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::CollectHashMap> for CliBuiltins {
    fn from(value: stdlib::CollectHashMap) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::CollectHashSet> for CliBuiltins {
    fn from(value: stdlib::CollectHashSet) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::CollectList> for CliBuiltins {
    fn from(value: stdlib::CollectList) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::CollectSignal> for CliBuiltins {
    fn from(value: stdlib::CollectSignal) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Concat> for CliBuiltins {
    fn from(value: stdlib::Concat) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Cons> for CliBuiltins {
    fn from(value: stdlib::Cons) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::ConstructHashMap> for CliBuiltins {
    fn from(value: stdlib::ConstructHashMap) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::ConstructHashSet> for CliBuiltins {
    fn from(value: stdlib::ConstructHashSet) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::ConstructRecord> for CliBuiltins {
    fn from(value: stdlib::ConstructRecord) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::ConstructList> for CliBuiltins {
    fn from(value: stdlib::ConstructList) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Contains> for CliBuiltins {
    fn from(value: stdlib::Contains) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Divide> for CliBuiltins {
    fn from(value: stdlib::Divide) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Effect> for CliBuiltins {
    fn from(value: stdlib::Effect) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::EndsWith> for CliBuiltins {
    fn from(value: stdlib::EndsWith) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Eq> for CliBuiltins {
    fn from(value: stdlib::Eq) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Equal> for CliBuiltins {
    fn from(value: stdlib::Equal) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Filter> for CliBuiltins {
    fn from(value: stdlib::Filter) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Flatten> for CliBuiltins {
    fn from(value: stdlib::Flatten) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Floor> for CliBuiltins {
    fn from(value: stdlib::Floor) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Get> for CliBuiltins {
    fn from(value: stdlib::Get) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Gt> for CliBuiltins {
    fn from(value: stdlib::Gt) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Gte> for CliBuiltins {
    fn from(value: stdlib::Gte) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Hash> for CliBuiltins {
    fn from(value: stdlib::Hash) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::If> for CliBuiltins {
    fn from(value: stdlib::If) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::IfError> for CliBuiltins {
    fn from(value: stdlib::IfError) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::IfPending> for CliBuiltins {
    fn from(value: stdlib::IfPending) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Insert> for CliBuiltins {
    fn from(value: stdlib::Insert) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Keys> for CliBuiltins {
    fn from(value: stdlib::Keys) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Length> for CliBuiltins {
    fn from(value: stdlib::Length) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Lt> for CliBuiltins {
    fn from(value: stdlib::Lt) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Lte> for CliBuiltins {
    fn from(value: stdlib::Lte) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Map> for CliBuiltins {
    fn from(value: stdlib::Map) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Max> for CliBuiltins {
    fn from(value: stdlib::Max) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Merge> for CliBuiltins {
    fn from(value: stdlib::Merge) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Min> for CliBuiltins {
    fn from(value: stdlib::Min) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Multiply> for CliBuiltins {
    fn from(value: stdlib::Multiply) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Not> for CliBuiltins {
    fn from(value: stdlib::Not) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Or> for CliBuiltins {
    fn from(value: stdlib::Or) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Pow> for CliBuiltins {
    fn from(value: stdlib::Pow) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Push> for CliBuiltins {
    fn from(value: stdlib::Push) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::PushFront> for CliBuiltins {
    fn from(value: stdlib::PushFront) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Raise> for CliBuiltins {
    fn from(value: stdlib::Raise) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Reduce> for CliBuiltins {
    fn from(value: stdlib::Reduce) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Remainder> for CliBuiltins {
    fn from(value: stdlib::Remainder) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Replace> for CliBuiltins {
    fn from(value: stdlib::Replace) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::ResolveArgs> for CliBuiltins {
    fn from(value: stdlib::ResolveArgs) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::ResolveDeep> for CliBuiltins {
    fn from(value: stdlib::ResolveDeep) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::ResolveHashMap> for CliBuiltins {
    fn from(value: stdlib::ResolveHashMap) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::ResolveHashSet> for CliBuiltins {
    fn from(value: stdlib::ResolveHashSet) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::ResolveShallow> for CliBuiltins {
    fn from(value: stdlib::ResolveShallow) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::ResolveRecord> for CliBuiltins {
    fn from(value: stdlib::ResolveRecord) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::ResolveList> for CliBuiltins {
    fn from(value: stdlib::ResolveList) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Round> for CliBuiltins {
    fn from(value: stdlib::Round) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Sequence> for CliBuiltins {
    fn from(value: stdlib::Sequence) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Slice> for CliBuiltins {
    fn from(value: stdlib::Slice) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Split> for CliBuiltins {
    fn from(value: stdlib::Split) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::StartsWith> for CliBuiltins {
    fn from(value: stdlib::StartsWith) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Subtract> for CliBuiltins {
    fn from(value: stdlib::Subtract) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Unzip> for CliBuiltins {
    fn from(value: stdlib::Unzip) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Values> for CliBuiltins {
    fn from(value: stdlib::Values) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}
impl From<stdlib::Zip> for CliBuiltins {
    fn from(value: stdlib::Zip) -> Self {
        Self::from(stdlib::Stdlib::from(value))
    }
}

impl From<reflex_json::stdlib::JsonDeserialize> for CliBuiltins {
    fn from(value: reflex_json::stdlib::JsonDeserialize) -> Self {
        Self::from(reflex_json::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_json::stdlib::JsonSerialize> for CliBuiltins {
    fn from(value: reflex_json::stdlib::JsonSerialize) -> Self {
        Self::from(reflex_json::stdlib::Stdlib::from(value))
    }
}

impl From<reflex_js::stdlib::Accessor> for CliBuiltins {
    fn from(value: reflex_js::stdlib::Accessor) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_js::stdlib::Construct> for CliBuiltins {
    fn from(value: reflex_js::stdlib::Construct) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_js::stdlib::DateConstructor> for CliBuiltins {
    fn from(value: reflex_js::stdlib::DateConstructor) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_js::stdlib::EncodeUriComponent> for CliBuiltins {
    fn from(value: reflex_js::stdlib::EncodeUriComponent) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_js::stdlib::FormatErrorMessage> for CliBuiltins {
    fn from(value: reflex_js::stdlib::FormatErrorMessage) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_js::stdlib::IsFinite> for CliBuiltins {
    fn from(value: reflex_js::stdlib::IsFinite) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_js::stdlib::Log> for CliBuiltins {
    fn from(value: reflex_js::stdlib::Log) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_js::stdlib::LogArgs> for CliBuiltins {
    fn from(value: reflex_js::stdlib::LogArgs) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_js::stdlib::ParseFloat> for CliBuiltins {
    fn from(value: reflex_js::stdlib::ParseFloat) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_js::stdlib::ParseInt> for CliBuiltins {
    fn from(value: reflex_js::stdlib::ParseInt) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_js::stdlib::Throw> for CliBuiltins {
    fn from(value: reflex_js::stdlib::Throw) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_js::stdlib::ToString> for CliBuiltins {
    fn from(value: reflex_js::stdlib::ToString) -> Self {
        Self::from(reflex_js::stdlib::Stdlib::from(value))
    }
}

impl From<reflex_handlers::stdlib::Scan> for CliBuiltins {
    fn from(value: reflex_handlers::stdlib::Scan) -> Self {
        Self::from(reflex_handlers::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_handlers::stdlib::ToRequest> for CliBuiltins {
    fn from(value: reflex_handlers::stdlib::ToRequest) -> Self {
        Self::from(reflex_handlers::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_handlers::stdlib::GetVariable> for CliBuiltins {
    fn from(value: reflex_handlers::stdlib::GetVariable) -> Self {
        Self::from(reflex_handlers::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_handlers::stdlib::SetVariable> for CliBuiltins {
    fn from(value: reflex_handlers::stdlib::SetVariable) -> Self {
        Self::from(reflex_handlers::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_handlers::stdlib::IncrementVariable> for CliBuiltins {
    fn from(value: reflex_handlers::stdlib::IncrementVariable) -> Self {
        Self::from(reflex_handlers::stdlib::Stdlib::from(value))
    }
}
impl From<reflex_handlers::stdlib::DecrementVariable> for CliBuiltins {
    fn from(value: reflex_handlers::stdlib::DecrementVariable) -> Self {
        Self::from(reflex_handlers::stdlib::Stdlib::from(value))
    }
}
