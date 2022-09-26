// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::convert::{TryFrom, TryInto};

use reflex::core::{
    Applicable, Arity, Builtin, EvaluationCache, Expression, ExpressionFactory, HeapAllocator, Uid,
    Uuid,
};
use reflex_handlers::stdlib::Stdlib as HandlersStdlib;
use reflex_js::stdlib::Stdlib as JsStdlib;
use reflex_json::stdlib::Stdlib as JsonStdlib;
use reflex_stdlib::Stdlib;

#[derive(Clone, Copy, Eq, PartialEq, Hash, Debug)]
pub enum CliBuiltins {
    Stdlib(Stdlib),
    Json(JsonStdlib),
    Js(JsStdlib),
    Handlers(HandlersStdlib),
}
impl From<Stdlib> for CliBuiltins {
    fn from(target: Stdlib) -> Self {
        CliBuiltins::Stdlib(target)
    }
}
impl From<JsonStdlib> for CliBuiltins {
    fn from(target: JsonStdlib) -> Self {
        CliBuiltins::Json(target)
    }
}
impl From<JsStdlib> for CliBuiltins {
    fn from(target: JsStdlib) -> Self {
        CliBuiltins::Js(target)
    }
}
impl From<HandlersStdlib> for CliBuiltins {
    fn from(target: HandlersStdlib) -> Self {
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
        TryInto::<Stdlib>::try_into(value)
            .map(Self::Stdlib)
            .or_else(|_| TryInto::<JsonStdlib>::try_into(value).map(Self::Json))
            .or_else(|_| TryInto::<JsStdlib>::try_into(value).map(Self::Js))
            .or_else(|_| TryInto::<HandlersStdlib>::try_into(value).map(Self::Handlers))
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
