// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::core::{
    Applicable, Arity, EvaluationCache, Expression, ExpressionFactory, HeapAllocator, VarArgs,
};

pub struct Tuple {}
impl<T: Expression> Applicable<T> for Tuple {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(0, 0, Some(VarArgs::Lazy)))
    }
    fn apply(
        &self,
        args: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        Ok(factory.create_tuple_term(allocator.create_list(args)))
    }
}
