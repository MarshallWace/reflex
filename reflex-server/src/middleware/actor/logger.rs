// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::marker::PhantomData;

use reflex_dispatcher::{Action, Actor, HandlerContext, MessageData, StateTransition};

use crate::logger::ActionLogger;

pub struct LoggerMiddleware<T: ActionLogger<TAction>, TAction: Action> {
    logger: T,
    _action: PhantomData<TAction>,
}
impl<T: ActionLogger<TAction>, TAction: Action> LoggerMiddleware<T, TAction> {
    pub fn new(logger: T) -> Self {
        Self {
            logger,
            _action: Default::default(),
        }
    }
}
impl<T, TAction> Actor<TAction> for LoggerMiddleware<T, TAction>
where
    T: ActionLogger<TAction>,
    TAction: Action,
{
    fn handle(
        &mut self,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> StateTransition<TAction> {
        self.logger.log(action, Some(metadata), Some(context));
        None.unwrap_or_default()
    }
}
