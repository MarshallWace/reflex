// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
};

use reflex::{
    core::{
        uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
        FunctionArity, HeapAllocator, StateToken, Uid, Uuid,
    },
    lang::ValueTerm,
    stdlib::Stdlib,
};

use super::setter::create_setter_function;

const STATE_NAMESPACE_VARIABLE: &'static str = "reflex::state::variable";

pub struct Variable {}
impl Variable {
    pub(crate) const UUID: Uuid = uuid!("d353ffb2-3223-4752-bf8e-d5b65390d780");
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Lazy],
        optional: [],
        variadic: None,
    };
}
impl Uid for Variable {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Variable
where
    T::Builtin: From<Stdlib>,
{
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(&Self::ARITY))
    }
    fn apply(
        &self,
        args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
        let state_token = args.next().unwrap();
        let initial_value = args.next().unwrap();
        if let Some(ValueTerm::Hash(state_token)) = factory.match_value_term(&state_token) {
            let state_token = prefix_state_token(STATE_NAMESPACE_VARIABLE, *state_token);
            let getter = factory.create_dynamic_variable_term(state_token, initial_value);
            let setter = create_setter_function(state_token, factory, allocator);
            Ok(factory.create_tuple_term(allocator.create_pair(getter, setter)))
        } else {
            Err(format!(
                "Invalid variable identifier: Expected Hash, received {}",
                state_token
            ))
        }
    }
}

fn prefix_state_token(prefix: &'static str, state_token: StateToken) -> StateToken {
    let mut hasher = DefaultHasher::new();
    hasher.write(prefix.as_bytes());
    state_token.hash(&mut hasher);
    hasher.finish()
}
