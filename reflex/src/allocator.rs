// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{
    iter::{empty, once},
    marker::PhantomData,
};

use crate::core::{
    Expression, ExpressionList, HeapAllocator, NativeAllocator, Signal, SignalList, SignalType,
    StructPrototype,
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
        let expressions = expressions.into_iter();
        HeapAllocator::<T>::create_sized_list(self, expressions.len(), expressions)
    }
    fn create_sized_list(
        &self,
        _length: usize,
        expressions: impl IntoIterator<Item = T>,
    ) -> ExpressionList<T> {
        ExpressionList::new(expressions)
    }
    fn create_unsized_list(&self, expressions: impl IntoIterator<Item = T>) -> ExpressionList<T> {
        HeapAllocator::<T>::create_list(self, expressions.into_iter().collect::<Vec<_>>())
    }
    fn create_empty_list(&self) -> ExpressionList<T> {
        HeapAllocator::<T>::create_sized_list(self, 0, empty())
    }
    fn create_unit_list(&self, value: T) -> ExpressionList<T> {
        HeapAllocator::<T>::create_sized_list(self, 1, once(value))
    }
    fn create_pair(&self, left: T, right: T) -> ExpressionList<T> {
        HeapAllocator::<T>::create_sized_list(self, 2, once(left).chain(once(right)))
    }
    fn clone_list(&self, expressions: &ExpressionList<T>) -> ExpressionList<T> {
        ExpressionList::new(expressions.iter().cloned())
    }
    fn create_triple(&self, first: T, second: T, third: T) -> ExpressionList<T> {
        HeapAllocator::<T>::create_sized_list(
            self,
            3,
            once(first).chain(once(second)).chain(once(third)),
        )
    }
    fn create_signal_list(&self, signals: impl IntoIterator<Item = Signal<T>>) -> SignalList<T> {
        SignalList::new(signals)
    }
    fn create_struct_prototype(
        &self,
        keys: impl IntoIterator<Item = String, IntoIter = impl ExactSizeIterator<Item = String>>,
    ) -> StructPrototype {
        StructPrototype::new(keys.into_iter())
    }
    fn clone_struct_prototype(&self, prototype: &StructPrototype) -> StructPrototype {
        StructPrototype::new(prototype.keys().iter().cloned())
    }
    fn create_signal(&self, signal_type: SignalType, args: ExpressionList<T>) -> Signal<T> {
        Signal::new(signal_type, args)
    }
    fn clone_signal(&self, signal: &Signal<T>) -> Signal<T> {
        Signal::new(
            signal.signal_type().clone(),
            HeapAllocator::<T>::clone_list(self, signal.args()),
        )
    }
    fn create_string(&self, value: String) -> T::String {
        value
    }
    fn clone_string(&self, value: &T::String) -> T::String {
        String::from(value)
    }
    fn as_object(&self) -> &dyn NativeAllocator<T> {
        self
    }
}
impl<T: Expression<String = String>> NativeAllocator<T> for DefaultAllocator<T> {
    fn create_list(&self, expressions: Vec<T>) -> ExpressionList<T> {
        HeapAllocator::<T>::create_list(self, expressions)
    }
    fn create_sized_list(&self, length: usize, expressions: Vec<T>) -> ExpressionList<T> {
        HeapAllocator::<T>::create_sized_list(self, length, expressions)
    }
    fn create_unsized_list(&self, expressions: Vec<T>) -> ExpressionList<T> {
        HeapAllocator::<T>::create_unsized_list(self, expressions)
    }
    fn create_empty_list(&self) -> ExpressionList<T> {
        HeapAllocator::<T>::create_empty_list(self)
    }
    fn create_unit_list(&self, value: T) -> ExpressionList<T> {
        HeapAllocator::<T>::create_unit_list(self, value)
    }
    fn create_pair(&self, left: T, right: T) -> ExpressionList<T> {
        HeapAllocator::<T>::create_pair(self, left, right)
    }
    fn clone_list(&self, expressions: &ExpressionList<T>) -> ExpressionList<T> {
        HeapAllocator::<T>::clone_list(self, expressions)
    }
    fn create_triple(&self, first: T, second: T, third: T) -> ExpressionList<T> {
        HeapAllocator::<T>::create_triple(self, first, second, third)
    }
    fn create_signal_list(&self, signals: Vec<Signal<T>>) -> SignalList<T> {
        HeapAllocator::<T>::create_signal_list(self, signals)
    }
    fn create_struct_prototype(&self, keys: Vec<String>) -> StructPrototype {
        HeapAllocator::<T>::create_struct_prototype(self, keys)
    }
    fn clone_struct_prototype(&self, prototype: &StructPrototype) -> StructPrototype {
        HeapAllocator::<T>::clone_struct_prototype(self, prototype)
    }
    fn create_signal(&self, signal_type: SignalType, args: ExpressionList<T>) -> Signal<T> {
        HeapAllocator::<T>::create_signal(self, signal_type, args)
    }
    fn clone_signal(&self, signal: &Signal<T>) -> Signal<T> {
        HeapAllocator::<T>::clone_signal(self, signal)
    }
    fn create_string(&self, value: String) -> T::String {
        HeapAllocator::<T>::create_string(self, value)
    }
    fn clone_string(&self, value: &T::String) -> T::String {
        HeapAllocator::<T>::clone_string(self, value)
    }
}
