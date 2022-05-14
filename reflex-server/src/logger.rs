// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex_dispatcher::{Action, HandlerContext, MessageData};

pub mod formatted;
pub mod json;

pub trait ActionLogger {
    type Action: Action;
    fn log(
        &mut self,
        action: &Self::Action,
        metadata: Option<&MessageData>,
        context: Option<&impl HandlerContext>,
    );
}

pub enum EitherLogger<T1, T2> {
    Left(T1),
    Right(T2),
}
impl<T1, T2> Clone for EitherLogger<T1, T2>
where
    T1: Clone,
    T2: Clone,
{
    fn clone(&self) -> Self {
        match self {
            Self::Left(inner) => Self::Left(inner.clone()),
            Self::Right(inner) => Self::Right(inner.clone()),
        }
    }
}

impl<T1: ActionLogger<Action = TAction>, T2: ActionLogger<Action = TAction>, TAction: Action>
    ActionLogger for EitherLogger<T1, T2>
{
    type Action = TAction;
    fn log(
        &mut self,
        action: &Self::Action,
        metadata: Option<&MessageData>,
        context: Option<&impl HandlerContext>,
    ) {
        match self {
            Self::Left(logger) => logger.log(action, metadata, context),
            Self::Right(logger) => logger.log(action, metadata, context),
        }
    }
}
