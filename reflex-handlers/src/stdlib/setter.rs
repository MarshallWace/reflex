// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::{
    core::{
        uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
        FunctionArity, HeapAllocator, StateToken, Uid, Uuid,
    },
    lang::ValueTerm,
    stdlib::Stdlib,
};

use crate::actor::assign::EFFECT_TYPE_ASSIGN;

pub struct Setter {}
impl Setter {
    pub(crate) const UUID: Uuid = uuid!("bf353115-91a3-414d-aefe-d99851dac777");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
}
impl Uid for Setter {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Setter
where
    T::Builtin: From<Stdlib>,
{
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(&Self::ARITY))
    }
    fn should_parallelize(&self, _args: &[T]) -> bool {
        false
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
        if let Some(ValueTerm::Hash(state_token)) = factory.match_value_term(&state_token) {
            Ok(create_setter_function(*state_token, factory, allocator))
        } else {
            Err(format!(
                "Invalid variable identifier: Expected Hash, received {}",
                state_token
            ))
        }
    }
}

pub(crate) fn create_setter_function<T: Expression>(
    state_token: StateToken,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Stdlib>,
{
    factory.create_lambda_term(
        1,
        factory.create_application_term(
            factory.create_builtin_term(Stdlib::Effect),
            allocator.create_triple(
                factory.create_value_term(ValueTerm::String(
                    allocator.create_string(String::from(EFFECT_TYPE_ASSIGN)),
                )),
                factory.create_value_term(ValueTerm::Hash(state_token)),
                factory.create_static_variable_term(0),
            ),
        ),
    )
}
