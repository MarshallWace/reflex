// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    pin::Pin,
    task::{Context, Poll},
};

use futures::Stream;
use pin_project::{pin_project, pinned_drop};

#[pin_project(PinnedDrop)]
pub struct WithUnsubscribeCallback<T: Stream, F: FnOnce()> {
    #[pin]
    inner: T,
    callback: Option<F>,
}
#[pinned_drop]
impl<T: Stream, F: FnOnce()> PinnedDrop for WithUnsubscribeCallback<T, F> {
    fn drop(self: Pin<&mut Self>) {
        if let Some(callback) = self.project().callback.take() {
            callback();
        }
    }
}
impl<T: Stream, F: FnOnce()> WithUnsubscribeCallback<T, F> {
    pub fn new(inner: T, callback: F) -> Self {
        Self {
            inner,
            callback: Some(callback),
        }
    }
}
impl<T: Stream, F: FnOnce()> Stream for WithUnsubscribeCallback<T, F>
where
    T: Stream,
    F: FnOnce(),
{
    type Item = T::Item;
    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        self.project().inner.as_mut().poll_next(cx)
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}
