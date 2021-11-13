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
    Expression, ExpressionList, HeapAllocator, Signal, SignalList, SignalType, StringValue,
    StructPrototype,
};

#[derive(Copy, Clone)]
pub struct DefaultAllocator<T: Expression> {
    _type: PhantomData<T>,
}
impl<T: Expression> DefaultAllocator<T> {
    fn sized_iterator_list(
        &self,
        expressions: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
    ) -> ExpressionList<T> {
        // TODO: optimise for lists with known size
        ExpressionList::new(expressions)
    }
    fn sized_list(
        &self,
        _length: usize,
        expressions: impl IntoIterator<Item = T>,
    ) -> ExpressionList<T> {
        // TODO: optimise for lists with known size
        ExpressionList::new(expressions)
    }
    fn unsized_list(&self, expressions: impl IntoIterator<Item = T>) -> ExpressionList<T> {
        ExpressionList::new(expressions)
    }
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
        let result = self.sized_iterator_list(expressions);
        trace!(alloc = "create_list", list_id = result.id());
        result
    }
    fn create_sized_list(
        &self,
        length: usize,
        expressions: impl IntoIterator<Item = T>,
    ) -> ExpressionList<T> {
        let result = self.sized_list(length, expressions);
        trace!(alloc = "create_sized_list", list_id = result.id());
        result
    }
    fn create_unsized_list(&self, expressions: impl IntoIterator<Item = T>) -> ExpressionList<T> {
        let result = self.unsized_list(expressions.into_iter().collect::<Vec<_>>());
        trace!(alloc = "create_unsized_list", list_id = result.id());
        result
    }
    fn create_empty_list(&self) -> ExpressionList<T> {
        trace!(alloc = "create_empty_list");
        self.sized_iterator_list(empty())
    }
    fn create_unit_list(&self, value: T) -> ExpressionList<T> {
        let result = self.sized_iterator_list(once(value));
        trace!(alloc = "create_unit_list", list_id = result.id());
        result
    }
    fn create_pair(&self, left: T, right: T) -> ExpressionList<T> {
        let result = self.sized_iterator_list([left, right]);
        trace!(alloc = "create_pair", list_id = result.id());
        result
    }
    fn clone_list(&self, expressions: &ExpressionList<T>) -> ExpressionList<T> {
        trace!(alloc = "clone_list");
        expressions.clone()
    }
    fn create_triple(&self, first: T, second: T, third: T) -> ExpressionList<T> {
        let result = self.sized_iterator_list([first, second, third]);
        trace!(alloc = "create_triple", list_id = result.id());
        result
    }
    fn create_signal_list(&self, signals: impl IntoIterator<Item = Signal<T>>) -> SignalList<T> {
        trace!(alloc = "create_signal_list");
        SignalList::new(signals)
    }
    fn create_struct_prototype(
        &self,
        keys: impl IntoIterator<Item = String, IntoIter = impl ExactSizeIterator<Item = String>>,
    ) -> StructPrototype {
        trace!(alloc = "create_struct_prototype");
        StructPrototype::new(keys.into_iter())
    }
    fn clone_struct_prototype(&self, prototype: &StructPrototype) -> StructPrototype {
        trace!(alloc = "clone_struct_prototype");
        StructPrototype::new(prototype.keys().iter().cloned())
    }
    fn create_signal(&self, signal_type: SignalType, args: ExpressionList<T>) -> Signal<T> {
        trace!(alloc = "create_signal");
        Signal::new(signal_type, args)
    }
    fn clone_signal(&self, signal: &Signal<T>) -> Signal<T> {
        trace!(alloc = "clone_signal");
        Signal::new(
            signal.signal_type().clone(),
            HeapAllocator::<T>::clone_list(self, signal.args()),
        )
    }
    fn create_string(&self, value: impl Into<T::String>) -> T::String {
        trace!(alloc = "create_string");
        value.into()
    }
    fn create_static_string(&self, value: &'static str) -> T::String {
        trace!(alloc = "create_static_string");
        T::String::from_static(Option::<T::String>::None, value)
    }
    fn clone_string(&self, value: &T::String) -> T::String {
        trace!(alloc = "clone_string");
        String::from(value)
    }
}
