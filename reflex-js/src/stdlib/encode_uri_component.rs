// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    FunctionArity, HeapAllocator, RefType, StringTermType, StringValue, Uid, Uuid,
};

pub struct EncodeUriComponent {}
impl EncodeUriComponent {
    pub(crate) const UUID: Uuid = uuid!("ad730068-31a7-49a4-ae09-ecbda0c9914a");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for EncodeUriComponent {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for EncodeUriComponent {
    fn arity(&self) -> Option<Arity> {
        Some(Self::arity())
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
        let input = args.next().unwrap();
        match factory.match_string_term(&input) {
            Some(term) => {
                let value = urlencoding::encode(term.value().as_deref().as_str());
                Ok(factory.create_string_term(allocator.create_string(value)))
            }
            _ => Err(format!("Expected String, received {}", input)),
        }
    }
}
