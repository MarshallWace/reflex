// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::{
    iter::{empty, once},
    marker::PhantomData,
};

use crate::{ExpressionList, Signal, SignalList, StructPrototype};
use reflex::core::{Expression, HeapAllocator, RefType, SignalType, StringValue};
use serde::{Deserialize, Serialize};

#[derive(Copy, Clone, Serialize, Deserialize)]
pub struct DefaultAllocator<T: Expression> {
    _type: PhantomData<T>,
}
impl<T: Expression<ExpressionList<T> = ExpressionList<T>>> DefaultAllocator<T> {
    fn sized_iterator_list(
        &self,
        expressions: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
    ) -> T::ExpressionList<T> {
        // TODO: optimise for lists with known size
        ExpressionList::new(expressions)
    }
    fn sized_list(
        &self,
        _length: usize,
        expressions: impl IntoIterator<Item = T>,
    ) -> T::ExpressionList<T> {
        // TODO: optimise for lists with known size
        ExpressionList::new(expressions)
    }
    fn unsized_list(&self, expressions: impl IntoIterator<Item = T>) -> T::ExpressionList<T> {
        ExpressionList::new(expressions)
    }
}
impl<T: Expression<ExpressionList<T> = ExpressionList<T>>> Default for DefaultAllocator<T> {
    fn default() -> Self {
        Self {
            _type: Default::default(),
        }
    }
}
impl<
        T: Expression<
            String = String,
            Signal<T> = Signal<T>,
            StructPrototype<T> = StructPrototype<T>,
            ExpressionList<T> = ExpressionList<T>,
            SignalList<T> = SignalList<T>,
        >,
    > HeapAllocator<T> for DefaultAllocator<T>
{
    fn create_list(
        &self,
        expressions: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
    ) -> T::ExpressionList<T> {
        self.sized_iterator_list(expressions)
    }
    fn create_sized_list(
        &self,
        length: usize,
        expressions: impl IntoIterator<Item = T>,
    ) -> T::ExpressionList<T> {
        self.sized_list(length, expressions)
    }
    fn create_unsized_list(
        &self,
        expressions: impl IntoIterator<Item = T>,
    ) -> T::ExpressionList<T> {
        self.unsized_list(expressions.into_iter().collect::<Vec<_>>())
    }
    fn create_empty_list(&self) -> T::ExpressionList<T> {
        self.sized_iterator_list(empty())
    }
    fn create_unit_list(&self, value: T) -> T::ExpressionList<T> {
        self.sized_iterator_list(once(value))
    }
    fn create_pair(&self, left: T, right: T) -> T::ExpressionList<T> {
        self.sized_iterator_list([left, right])
    }
    fn clone_list<'a>(&self, expressions: T::ExpressionListRef<'a, T>) -> T::ExpressionList<T> {
        expressions.as_deref().clone()
    }
    fn create_triple(&self, first: T, second: T, third: T) -> T::ExpressionList<T> {
        self.sized_iterator_list([first, second, third])
    }
    fn create_signal_list(
        &self,
        signals: impl IntoIterator<Item = T::Signal<T>>,
    ) -> T::SignalList<T> {
        SignalList::new(signals)
    }
    fn create_struct_prototype(&self, keys: T::ExpressionList<T>) -> T::StructPrototype<T> {
        StructPrototype::new(keys)
    }
    fn clone_struct_prototype<'a>(
        &self,
        prototype: T::StructPrototypeRef<'a, T>,
    ) -> T::StructPrototype<T> {
        prototype.as_deref().clone()
    }
    fn create_signal(&self, signal_type: SignalType, payload: T, token: T) -> T::Signal<T> {
        Signal::new(signal_type, payload, token)
    }
    fn clone_signal<'a>(&self, signal: T::SignalRef<'a, T>) -> T::Signal<T> {
        signal.as_deref().clone()
    }
    fn create_string(&self, value: impl Into<String>) -> T::String {
        value.into()
    }
    fn create_static_string(&self, value: &'static str) -> T::String {
        T::String::from_static(Option::<T::String>::None, value)
            .unwrap_or_else(|| self.create_string(value))
    }
    fn clone_string<'a>(&self, value: T::StringRef<'a>) -> T::String {
        value.as_deref().clone()
    }
}
