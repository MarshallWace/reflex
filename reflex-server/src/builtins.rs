// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::convert::{TryFrom, TryInto};

use reflex::{
    core::{
        Applicable, Arity, Builtin, EvaluationCache, Expression, ExpressionFactory, HeapAllocator,
        Uid, Uuid,
    },
    stdlib::Stdlib,
};
use reflex_graphql::stdlib::Stdlib as GraphQlStdlib;
use reflex_handlers::stdlib::Stdlib as HandlersStdlib;
use reflex_js::stdlib::Stdlib as JsStdlib;
use reflex_json::stdlib::Stdlib as JsonStdlib;
use serde::{Deserialize, Serialize};

use crate::stdlib::Stdlib as ServerStdlib;

#[derive(Clone, Copy, Eq, PartialEq, Hash, Debug, Serialize, Deserialize)]
pub enum ServerBuiltins {
    Stdlib(Stdlib),
    Json(JsonStdlib),
    Js(JsStdlib),
    GraphQl(GraphQlStdlib),
    Handlers(HandlersStdlib),
    Server(ServerStdlib),
}
impl From<Stdlib> for ServerBuiltins {
    fn from(target: Stdlib) -> Self {
        ServerBuiltins::Stdlib(target)
    }
}
impl From<JsonStdlib> for ServerBuiltins {
    fn from(target: JsonStdlib) -> Self {
        ServerBuiltins::Json(target)
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
impl From<HandlersStdlib> for ServerBuiltins {
    fn from(target: HandlersStdlib) -> Self {
        ServerBuiltins::Handlers(target)
    }
}
impl From<ServerStdlib> for ServerBuiltins {
    fn from(target: ServerStdlib) -> Self {
        ServerBuiltins::Server(target)
    }
}
impl ServerBuiltins {
    pub fn entries() -> impl Iterator<Item = Self> {
        Stdlib::entries()
            .map(Self::Stdlib)
            .chain(JsonStdlib::entries().map(Self::Json))
            .chain(JsStdlib::entries().map(Self::Js))
            .chain(GraphQlStdlib::entries().map(Self::GraphQl))
            .chain(HandlersStdlib::entries().map(Self::Handlers))
            .chain(ServerStdlib::entries().map(Self::Server))
    }
}
impl Uid for ServerBuiltins {
    fn uid(&self) -> reflex::core::Uuid {
        match self {
            ServerBuiltins::Stdlib(term) => term.uid(),
            ServerBuiltins::Json(term) => term.uid(),
            ServerBuiltins::Js(term) => term.uid(),
            ServerBuiltins::GraphQl(term) => term.uid(),
            ServerBuiltins::Handlers(term) => term.uid(),
            ServerBuiltins::Server(term) => term.uid(),
        }
    }
}
impl TryFrom<Uuid> for ServerBuiltins {
    type Error = ();
    fn try_from(value: Uuid) -> Result<Self, Self::Error> {
        TryInto::<Stdlib>::try_into(value)
            .map(Self::Stdlib)
            .or_else(|_| TryInto::<JsonStdlib>::try_into(value).map(Self::Json))
            .or_else(|_| TryInto::<JsStdlib>::try_into(value).map(Self::Js))
            .or_else(|_| TryInto::<GraphQlStdlib>::try_into(value).map(Self::GraphQl))
            .or_else(|_| TryInto::<HandlersStdlib>::try_into(value).map(Self::Handlers))
            .or_else(|_| TryInto::<ServerStdlib>::try_into(value).map(Self::Server))
    }
}
impl Builtin for ServerBuiltins {
    fn arity(&self) -> Arity {
        match self {
            ServerBuiltins::Stdlib(term) => term.arity(),
            ServerBuiltins::Json(term) => term.arity(),
            ServerBuiltins::Js(term) => term.arity(),
            ServerBuiltins::GraphQl(term) => term.arity(),
            ServerBuiltins::Handlers(term) => term.arity(),
            ServerBuiltins::Server(term) => term.arity(),
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
            ServerBuiltins::Json(term) => term.apply(args, factory, allocator, cache),
            ServerBuiltins::Js(term) => term.apply(args, factory, allocator, cache),
            ServerBuiltins::GraphQl(term) => term.apply(args, factory, allocator, cache),
            ServerBuiltins::Handlers(term) => term.apply(args, factory, allocator, cache),
            ServerBuiltins::Server(term) => term.apply(args, factory, allocator, cache),
        }
    }
    fn should_parallelize<T: Expression<Builtin = Self> + Applicable<T>>(
        &self,
        args: &[T],
    ) -> bool {
        match self {
            ServerBuiltins::Stdlib(term) => term.should_parallelize(args),
            ServerBuiltins::Json(term) => term.should_parallelize(args),
            ServerBuiltins::Js(term) => term.should_parallelize(args),
            ServerBuiltins::GraphQl(term) => term.should_parallelize(args),
            ServerBuiltins::Handlers(term) => term.should_parallelize(args),
            ServerBuiltins::Server(term) => term.should_parallelize(args),
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
            Self::Server(target) => std::fmt::Display::fmt(target, f),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use reflex::allocator::DefaultAllocator;
    use reflex::lang::{CachedSharedTerm, SharedTermFactory};
    use reflex::parser::sexpr::parse;

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
