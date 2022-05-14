// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::marker::PhantomData;

use reflex_dispatcher::{Action, Actor, ActorTransition, HandlerContext, MessageData};

use crate::logger::ActionLogger;

pub trait LoggerMiddlewareFactory<T: ActionLogger> {
    fn create(&self) -> T;
}
impl<T> LoggerMiddlewareFactory<T> for T
where
    T: ActionLogger + Clone,
{
    fn create(&self) -> T {
        self.clone()
    }
}

pub struct LoggerMiddleware<TFactory, TLogger>
where
    TFactory: LoggerMiddlewareFactory<TLogger>,
    TLogger: ActionLogger,
{
    factory: TFactory,
    _logger: PhantomData<TLogger>,
}
impl<TFactory, TLogger> LoggerMiddleware<TFactory, TLogger>
where
    TFactory: LoggerMiddlewareFactory<TLogger>,
    TLogger: ActionLogger,
{
    pub fn new(factory: TFactory) -> Self {
        Self {
            factory,
            _logger: Default::default(),
        }
    }
}

#[derive(Default)]
pub struct LoggerMiddlewareState<TLogger: ActionLogger> {
    logger: TLogger,
}

impl<TFactory, TLogger, TAction> Actor<TAction> for LoggerMiddleware<TFactory, TLogger>
where
    TFactory: LoggerMiddlewareFactory<TLogger>,
    TLogger: ActionLogger<Action = TAction>,
    TAction: Action,
{
    type State = LoggerMiddlewareState<TLogger>;
    fn init(&self) -> Self::State {
        LoggerMiddlewareState {
            logger: self.factory.create(),
        }
    }
    fn handle(
        &self,
        state: Self::State,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> ActorTransition<Self::State, TAction> {
        let mut state = state;
        state.logger.log(action, Some(metadata), Some(context));
        ActorTransition::new(state, Default::default())
    }
}
