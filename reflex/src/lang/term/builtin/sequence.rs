// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::core::{
    Applicable, Arity, EvaluationCache, Expression, ExpressionFactory, HeapAllocator,
};

pub struct Sequence {}
impl<T: Expression> Applicable<T> for Sequence {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(2, 0, None))
    }
    fn apply(
        &self,
        args: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
        if args.len() != 2 {
            return Err(format!("Expected 2 arguments, received {}", args.len()));
        }
        let value = args.next().unwrap();
        let callback = args.next().unwrap();
        Ok(factory.create_application_term(callback, allocator.create_unit_list(value)))
    }
}
