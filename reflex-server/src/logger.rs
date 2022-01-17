// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex_dispatcher::{Action, HandlerContext, MessageData};

pub mod formatted;
pub mod json;

pub trait ActionLogger<T: Action> {
    fn log(
        &mut self,
        action: &T,
        metadata: Option<&MessageData>,
        context: Option<&impl HandlerContext>,
    );
}

pub enum EitherLogger<T1, T2> {
    Left(T1),
    Right(T2),
}

impl<T1: ActionLogger<T>, T2: ActionLogger<T>, T: Action> ActionLogger<T> for EitherLogger<T1, T2> {
    fn log(
        &mut self,
        action: &T,
        metadata: Option<&MessageData>,
        context: Option<&impl HandlerContext>,
    ) {
        match self {
            Self::Left(logger) => logger.log(action, metadata, context),
            Self::Right(logger) => logger.log(action, metadata, context),
        }
    }
}
