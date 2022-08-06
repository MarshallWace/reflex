// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    iter::{empty, once},
    marker::PhantomData,
};

use crate::{ExpressionList, Signal, SignalList, StructPrototype};
use reflex::core::{ConditionType, Expression, HeapAllocator, SignalType, StringValue};

#[derive(Copy, Clone)]
pub struct DefaultAllocator<T: Expression> {
    _type: PhantomData<T>,
}
impl<T: Expression<ExpressionList = ExpressionList<T>>> DefaultAllocator<T> {
    fn sized_iterator_list(
        &self,
        expressions: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
    ) -> T::ExpressionList {
        // TODO: optimise for lists with known size
        ExpressionList::new(expressions)
    }
    fn sized_list(
        &self,
        _length: usize,
        expressions: impl IntoIterator<Item = T>,
    ) -> T::ExpressionList {
        // TODO: optimise for lists with known size
        ExpressionList::new(expressions)
    }
    fn unsized_list(&self, expressions: impl IntoIterator<Item = T>) -> T::ExpressionList {
        ExpressionList::new(expressions)
    }
}
impl<T: Expression<ExpressionList = ExpressionList<T>>> Default for DefaultAllocator<T> {
    fn default() -> Self {
        Self {
            _type: Default::default(),
        }
    }
}
impl<
        T: Expression<
            String = String,
            Signal = Signal<T>,
            StructPrototype = StructPrototype<T>,
            ExpressionList = ExpressionList<T>,
            SignalList = SignalList<T>,
        >,
    > HeapAllocator<T> for DefaultAllocator<T>
{
    fn create_list(
        &self,
        expressions: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
    ) -> T::ExpressionList {
        self.sized_iterator_list(expressions)
    }
    fn create_sized_list(
        &self,
        length: usize,
        expressions: impl IntoIterator<Item = T>,
    ) -> T::ExpressionList {
        self.sized_list(length, expressions)
    }
    fn create_unsized_list(&self, expressions: impl IntoIterator<Item = T>) -> T::ExpressionList {
        self.unsized_list(expressions.into_iter().collect::<Vec<_>>())
    }
    fn create_empty_list(&self) -> T::ExpressionList {
        self.sized_iterator_list(empty())
    }
    fn create_unit_list(&self, value: T) -> T::ExpressionList {
        self.sized_iterator_list(once(value))
    }
    fn create_pair(&self, left: T, right: T) -> T::ExpressionList {
        self.sized_iterator_list([left, right])
    }
    fn clone_list(&self, expressions: &T::ExpressionList) -> T::ExpressionList {
        expressions.clone()
    }
    fn create_triple(&self, first: T, second: T, third: T) -> T::ExpressionList {
        self.sized_iterator_list([first, second, third])
    }
    fn create_signal_list(&self, signals: impl IntoIterator<Item = T::Signal>) -> T::SignalList {
        SignalList::new(signals)
    }
    fn create_struct_prototype(&self, keys: T::ExpressionList) -> T::StructPrototype {
        StructPrototype::new(keys)
    }
    fn clone_struct_prototype(&self, prototype: &T::StructPrototype) -> T::StructPrototype {
        prototype.clone()
    }
    fn create_signal(&self, signal_type: SignalType, args: T::ExpressionList) -> T::Signal {
        Signal::new(signal_type, args)
    }
    fn clone_signal(&self, signal: &T::Signal) -> T::Signal {
        Signal::new(
            signal.signal_type().clone(),
            HeapAllocator::<T>::clone_list(self, signal.args()),
        )
    }
    fn create_string(&self, value: impl Into<T::String>) -> T::String {
        value.into()
    }
    fn create_static_string(&self, value: &'static str) -> T::String {
        T::String::from_static(Option::<T::String>::None, value)
    }
    fn clone_string(&self, value: &T::String) -> T::String {
        String::from(value)
    }
}
