// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::{
    core::{
        Applicable, Arity, Builtin, EvaluationCache, Expression, ExpressionFactory, HeapAllocator,
        Uid,
    },
    stdlib::Stdlib,
};
use reflex_graphql::stdlib::Stdlib as GraphQlStdlib;
use reflex_js::stdlib::Stdlib as JsStdlib;

#[derive(Clone, Copy, Eq, PartialEq, Hash, Debug)]
pub enum ServerBuiltins {
    Stdlib(Stdlib),
    Js(JsStdlib),
    GraphQl(GraphQlStdlib),
}
impl From<Stdlib> for ServerBuiltins {
    fn from(target: Stdlib) -> Self {
        ServerBuiltins::Stdlib(target)
    }
}
impl From<JsStdlib> for ServerBuiltins {
    fn from(target: JsStdlib) -> Self {
        ServerBuiltins::Js(target)
    }
}
impl From<GraphQlStdlib> for ServerBuiltins {
    fn from(target: GraphQlStdlib) -> Self {
        ServerBuiltins::GraphQl(target)
    }
}
impl ServerBuiltins {
    pub fn entries() -> impl Iterator<Item = Self> {
        Stdlib::entries()
            .map(Self::Stdlib)
            .chain(JsStdlib::entries().map(Self::Js))
            .chain(GraphQlStdlib::entries().map(Self::GraphQl))
    }
}
impl Uid for ServerBuiltins {
    fn uid(&self) -> reflex::core::Uuid {
        match self {
            ServerBuiltins::Stdlib(term) => term.uid(),
            ServerBuiltins::Js(term) => term.uid(),
            ServerBuiltins::GraphQl(term) => term.uid(),
        }
    }
}
impl Builtin for ServerBuiltins {
    fn arity<T: Expression<Builtin = Self> + Applicable<T>>(&self) -> Option<Arity> {
        match self {
            ServerBuiltins::Stdlib(term) => term.arity::<T>(),
            ServerBuiltins::Js(term) => term.arity::<T>(),
            ServerBuiltins::GraphQl(term) => term.arity::<T>(),
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
            ServerBuiltins::Stdlib(term) => term.apply(args, factory, allocator, cache),
            ServerBuiltins::Js(term) => term.apply(args, factory, allocator, cache),
            ServerBuiltins::GraphQl(term) => term.apply(args, factory, allocator, cache),
        }
    }
}
impl std::fmt::Display for ServerBuiltins {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Stdlib(target) => std::fmt::Display::fmt(target, f),
            Self::Js(target) => std::fmt::Display::fmt(target, f),
            Self::GraphQl(target) => std::fmt::Display::fmt(target, f),
        }
    }
}
