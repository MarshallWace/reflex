// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    pin::Pin,
    task::{Context, Poll},
};

use futures::Stream;
use pin_project::pin_project;

#[pin_project]
pub struct TakeUntilFinalItem<T: Stream, F: for<'a> Fn(&'a T::Item) -> bool> {
    #[pin]
    inner: T,
    predicate: F,
    is_completed: bool,
}
impl<T: Stream, F: for<'a> Fn(&'a T::Item) -> bool> TakeUntilFinalItem<T, F> {
    pub fn new(inner: T, predicate: F) -> Self {
        Self {
            inner,
            predicate,
            is_completed: false,
        }
    }
}
impl<T: Stream, F: for<'a> Fn(&'a T::Item) -> bool> Stream for TakeUntilFinalItem<T, F> {
    type Item = T::Item;
    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<T::Item>> {
        if self.is_completed {
            return Poll::Ready(None);
        }
        let mut this = self.project();
        match this.inner.as_mut().poll_next(cx) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(value) => Poll::Ready(match value {
                None => None,
                Some(value) => {
                    let is_completed = (this.predicate)(&value);
                    if is_completed {
                        *this.is_completed = true;
                    }
                    Some(value)
                }
            }),
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        if self.is_completed {
            return (0, Some(0));
        }
        let (_, upper) = self.inner.size_hint();
        (0, upper)
    }
}
