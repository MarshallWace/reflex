// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::cli::reflex::core::Uuid;
use reflex::{
    core::{
        Applicable, Arity, Builtin, EvaluationCache, Expression, ExpressionFactory, HeapAllocator,
        Uid,
    },
    stdlib::Stdlib,
};
use reflex_graphql::stdlib::Stdlib as GraphQlStdlib;
use reflex_js::stdlib::Stdlib as JsStdlib;
use serde::{Deserialize, Serialize};
use std::convert::{TryFrom, TryInto};

#[derive(Clone, Copy, Eq, PartialEq, Hash, Debug, Serialize, Deserialize)]
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
impl TryFrom<Uuid> for ServerBuiltins {
    type Error = ();
    fn try_from(value: Uuid) -> Result<Self, Self::Error> {
        TryInto::<Stdlib>::try_into(value)
            .map(Self::Stdlib)
            .or_else(|_| TryInto::<JsStdlib>::try_into(value).map(Self::Js))
            .or_else(|_| TryInto::<GraphQlStdlib>::try_into(value).map(Self::GraphQl))
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

#[cfg(test)]
mod test {
    use super::*;
    use reflex::allocator::DefaultAllocator;
    use reflex::lang::{CachedSharedTerm, SharedTermFactory};
    use reflex::parser::sexpr::parse;
    use rmp_serde::Serializer;

    #[test]
    fn round_trip_serde() {
        let factory = SharedTermFactory::<ServerBuiltins>::default();
        let allocator = DefaultAllocator::default();

        let value = parse("((lambda (foo) (* (+ 2 3) foo)) 2)", &factory, &allocator).unwrap();
        let mut buf = Vec::new();
        value.serialize(&mut Serializer::new(&mut buf)).unwrap();
        let deser: CachedSharedTerm<ServerBuiltins> = rmp_serde::from_read_ref(&buf).unwrap();
        assert_eq!(value, deser);
    }
}
