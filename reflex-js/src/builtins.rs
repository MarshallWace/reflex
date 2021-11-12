// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use reflex::core::Uuid;
use reflex::{
    core::{
        Applicable, Arity, Builtin, EvaluationCache, Expression, ExpressionFactory, HeapAllocator,
        Uid,
    },
    stdlib::Stdlib,
};
use serde::{Deserialize, Serialize};
use std::convert::{TryFrom, TryInto};

use crate::stdlib::Stdlib as JsStdlib;

#[derive(Clone, Copy, Eq, PartialEq, Hash, Debug, Serialize, Deserialize)]
pub enum JsBuiltins {
    Stdlib(Stdlib),
    Js(JsStdlib),
}
impl From<Stdlib> for JsBuiltins {
    fn from(builtin: Stdlib) -> Self {
        JsBuiltins::Stdlib(builtin)
    }
}
impl From<JsStdlib> for JsBuiltins {
    fn from(target: JsStdlib) -> Self {
        JsBuiltins::Js(target)
    }
}
impl JsBuiltins {
    pub fn entries() -> impl Iterator<Item = Self> {
        Stdlib::entries()
            .map(Self::Stdlib)
            .chain(JsStdlib::entries().map(Self::Js))
    }
}
impl Uid for JsBuiltins {
    fn uid(&self) -> reflex::core::Uuid {
        match self {
            JsBuiltins::Stdlib(term) => term.uid(),
            JsBuiltins::Js(term) => term.uid(),
        }
    }
}
impl Builtin for JsBuiltins {
    fn arity<T: Expression<Builtin = Self> + Applicable<T>>(&self) -> Option<Arity> {
        match self {
            JsBuiltins::Stdlib(term) => term.arity::<T>(),
            JsBuiltins::Js(term) => term.arity::<T>(),
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
            JsBuiltins::Js(term) => term.apply(args, factory, allocator, cache),
        }
    }
}
impl std::fmt::Display for JsBuiltins {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Stdlib(target) => std::fmt::Display::fmt(target, f),
            Self::Js(target) => std::fmt::Display::fmt(target, f),
        }
    }
}
impl TryFrom<Uuid> for JsBuiltins {
    type Error = ();
    fn try_from(value: Uuid) -> Result<Self, Self::Error> {
        TryInto::<Stdlib>::try_into(value)
            .map(Self::Stdlib)
            .or_else(|_| TryInto::<JsStdlib>::try_into(value).map(Self::Js))
    }
}

#[cfg(test)]
mod test {
    use crate::builtins::JsBuiltins;
    use crate::{create_js_env, parse};
    use reflex::allocator::DefaultAllocator;
    use reflex::core::ExpressionFactory;
    use reflex::lang::{CachedSharedTerm, SharedTermFactory, ValueTerm};

    #[test]
    fn round_trip_serde() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();

        let value = factory.create_value_term(ValueTerm::Int(5));
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
