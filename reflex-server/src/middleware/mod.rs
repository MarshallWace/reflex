// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::marker::PhantomData;

use reflex_dispatcher::{Action, Actor, NoopActor};

pub mod action;
pub(crate) mod actor;

pub use self::actor::*;

pub struct ServerMiddleware<T: Action, TPre: Actor<T>, TPost: Actor<T>> {
    pub pre: TPre,
    pub post: TPost,
    _action: PhantomData<T>,
}
impl<T, TPre, TPost> ServerMiddleware<T, TPre, TPost>
where
    T: Action,
    TPre: Actor<T>,
    TPost: Actor<T>,
{
    pub fn new(pre: TPre, post: TPost) -> Self {
        Self {
            pre,
            post,
            _action: Default::default(),
        }
    }
}
impl<T, TPre> ServerMiddleware<T, TPre, NoopActor>
where
    T: Action,
    TPre: Actor<T>,
{
    pub fn pre(middleware: TPre) -> Self {
        Self::new(middleware, NoopActor)
    }
}
impl<T, TPost> ServerMiddleware<T, NoopActor, TPost>
where
    T: Action,
    TPost: Actor<T>,
{
    pub fn post(middleware: TPost) -> Self {
        Self::new(NoopActor, middleware)
    }
}
impl<T> ServerMiddleware<T, NoopActor, NoopActor>
where
    T: Action,
{
    pub fn noop() -> Self {
        Self::new(NoopActor, NoopActor)
    }
}
