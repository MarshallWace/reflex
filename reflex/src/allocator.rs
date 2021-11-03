// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{
    iter::{empty, once},
    marker::PhantomData,
};
use tracing::trace;

use crate::core::{
    Expression, ExpressionList, HeapAllocator, Signal, SignalList, SignalType, StructPrototype,
};

#[derive(Copy, Clone)]
pub struct DefaultAllocator<T: Expression> {
    _type: PhantomData<T>,
}
impl<T: Expression> Default for DefaultAllocator<T> {
    fn default() -> Self {
        Self { _type: PhantomData }
    }
}
impl<T: Expression<String = String>> HeapAllocator<T> for DefaultAllocator<T> {
    fn create_list(
        &self,
        expressions: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
    ) -> ExpressionList<T> {
        trace!(alloc = "list");
        let expressions = expressions.into_iter();
        HeapAllocator::<T>::create_sized_list(self, expressions.len(), expressions)
    }
    fn create_sized_list(
        &self,
        _length: usize,
        expressions: impl IntoIterator<Item = T>,
    ) -> ExpressionList<T> {
        let list = ExpressionList::new(expressions);
        trace!(alloc = "sized_list", list_id = list.id());
        list
    }
    fn create_unsized_list(&self, expressions: impl IntoIterator<Item = T>) -> ExpressionList<T> {
        trace!(alloc = "unsized_list");
        HeapAllocator::<T>::create_list(self, expressions.into_iter().collect::<Vec<_>>())
    }
    fn create_empty_list(&self) -> ExpressionList<T> {
        trace!(alloc = "empty_list");
        HeapAllocator::<T>::create_sized_list(self, 0, empty())
    }
    fn create_unit_list(&self, value: T) -> ExpressionList<T> {
        trace!(alloc = "unit_list");
        HeapAllocator::<T>::create_sized_list(self, 1, once(value))
    }
    fn create_pair(&self, left: T, right: T) -> ExpressionList<T> {
        trace!(alloc = "pair");
        HeapAllocator::<T>::create_sized_list(self, 2, once(left).chain(once(right)))
    }
    fn clone_list(&self, expressions: &ExpressionList<T>) -> ExpressionList<T> {
        let list = ExpressionList::new(expressions.iter().cloned());
        trace!(alloc = "clone_list", list_id = list.id());
        list
    }
    fn create_triple(&self, first: T, second: T, third: T) -> ExpressionList<T> {
        trace!(alloc = "triple");
        HeapAllocator::<T>::create_sized_list(
            self,
            3,
            once(first).chain(once(second)).chain(once(third)),
        )
    }
    fn create_signal_list(&self, signals: impl IntoIterator<Item = Signal<T>>) -> SignalList<T> {
        trace!(alloc = "signal_list");
        SignalList::new(signals)
    }
    fn create_struct_prototype(
        &self,
        keys: impl IntoIterator<Item = String, IntoIter = impl ExactSizeIterator<Item = String>>,
    ) -> StructPrototype {
        trace!(alloc = "struct_prototype");
        StructPrototype::new(keys.into_iter())
    }
    fn clone_struct_prototype(&self, prototype: &StructPrototype) -> StructPrototype {
        trace!(alloc = "clone_struct_prototype");
        StructPrototype::new(prototype.keys().iter().cloned())
    }
    fn create_signal(&self, signal_type: SignalType, args: ExpressionList<T>) -> Signal<T> {
        trace!(alloc = "signal");
        Signal::new(signal_type, args)
    }
    fn clone_signal(&self, signal: &Signal<T>) -> Signal<T> {
        trace!(alloc = "clone_signal");
        Signal::new(
            signal.signal_type().clone(),
            HeapAllocator::<T>::clone_list(self, signal.args()),
        )
    }
    fn create_string(&self, value: String) -> T::String {
        trace!(alloc = "string");
        value
    }
    fn clone_string(&self, value: &T::String) -> T::String {
        trace!(alloc = "clone_string");
        String::from(value)
    }
}
