// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{EvaluationResult, Expression, StateToken};
use reflex_dispatcher::{Action, MessageOffset, NamedAction, SerializableAction, SerializedAction};
use reflex_json::JsonValue;

use crate::{QueryEvaluationMode, QueryInvalidationStrategy};

#[derive(Clone, Debug)]
pub enum EvaluateAction<T: Expression> {
    Start(EvaluateStartAction<T>),
    Update(EvaluateUpdateAction<T>),
    Stop(EvaluateStopAction),
    Result(EvaluateResultAction<T>),
}
impl<T: Expression> Action for EvaluateAction<T> {}
impl<T: Expression> NamedAction for EvaluateAction<T> {
    fn name(&self) -> &'static str {
        match self {
            Self::Start(action) => action.name(),
            Self::Update(action) => action.name(),
            Self::Stop(action) => action.name(),
            Self::Result(action) => action.name(),
        }
    }
}
impl<T: Expression> SerializableAction for EvaluateAction<T> {
    fn serialize(&self) -> SerializedAction {
        match self {
            Self::Start(action) => action.serialize(),
            Self::Update(action) => action.serialize(),
            Self::Stop(action) => action.serialize(),
            Self::Result(action) => action.serialize(),
        }
    }
}

impl<T: Expression> From<EvaluateStartAction<T>> for EvaluateAction<T> {
    fn from(value: EvaluateStartAction<T>) -> Self {
        Self::Start(value)
    }
}
impl<T: Expression> From<EvaluateAction<T>> for Option<EvaluateStartAction<T>> {
    fn from(value: EvaluateAction<T>) -> Self {
        match value {
            EvaluateAction::Start(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a EvaluateAction<T>> for Option<&'a EvaluateStartAction<T>> {
    fn from(value: &'a EvaluateAction<T>) -> Self {
        match value {
            EvaluateAction::Start(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<EvaluateUpdateAction<T>> for EvaluateAction<T> {
    fn from(value: EvaluateUpdateAction<T>) -> Self {
        Self::Update(value)
    }
}
impl<T: Expression> From<EvaluateAction<T>> for Option<EvaluateUpdateAction<T>> {
    fn from(value: EvaluateAction<T>) -> Self {
        match value {
            EvaluateAction::Update(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a EvaluateAction<T>> for Option<&'a EvaluateUpdateAction<T>> {
    fn from(value: &'a EvaluateAction<T>) -> Self {
        match value {
            EvaluateAction::Update(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<EvaluateStopAction> for EvaluateAction<T> {
    fn from(value: EvaluateStopAction) -> Self {
        Self::Stop(value)
    }
}
impl<T: Expression> From<EvaluateAction<T>> for Option<EvaluateStopAction> {
    fn from(value: EvaluateAction<T>) -> Self {
        match value {
            EvaluateAction::Stop(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a EvaluateAction<T>> for Option<&'a EvaluateStopAction> {
    fn from(value: &'a EvaluateAction<T>) -> Self {
        match value {
            EvaluateAction::Stop(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<EvaluateResultAction<T>> for EvaluateAction<T> {
    fn from(value: EvaluateResultAction<T>) -> Self {
        Self::Result(value)
    }
}
impl<T: Expression> From<EvaluateAction<T>> for Option<EvaluateResultAction<T>> {
    fn from(value: EvaluateAction<T>) -> Self {
        match value {
            EvaluateAction::Result(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a EvaluateAction<T>> for Option<&'a EvaluateResultAction<T>> {
    fn from(value: &'a EvaluateAction<T>) -> Self {
        match value {
            EvaluateAction::Result(value) => Some(value),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct EvaluateStartAction<T: Expression> {
    pub cache_id: StateToken,
    pub query: T,
    pub evaluation_mode: QueryEvaluationMode,
    pub invalidation_strategy: QueryInvalidationStrategy,
}
impl<T: Expression> Action for EvaluateStartAction<T> {}
impl<T: Expression> NamedAction for EvaluateStartAction<T> {
    fn name(&self) -> &'static str {
        "EvaluateStartAction"
    }
}
impl<T: Expression> SerializableAction for EvaluateStartAction<T> {
    fn serialize(&self) -> SerializedAction {
        SerializedAction::from_iter([
            ("cache_id", JsonValue::from(self.cache_id)),
            ("query_id", JsonValue::from(self.query.id())),
            (
                "standalone",
                JsonValue::from(match self.evaluation_mode {
                    QueryEvaluationMode::Standalone => true,
                    QueryEvaluationMode::Query => false,
                }),
            ),
            (
                "batch_updates",
                JsonValue::from(match self.invalidation_strategy {
                    QueryInvalidationStrategy::CombineUpdateBatches => true,
                    QueryInvalidationStrategy::Exact => false,
                }),
            ),
        ])
    }
}

#[derive(Clone, Debug)]
pub struct EvaluateUpdateAction<T: Expression> {
    pub cache_id: StateToken,
    pub state_index: Option<MessageOffset>,
    pub state_updates: Vec<(StateToken, T)>,
}
impl<T: Expression> Action for EvaluateUpdateAction<T> {}
impl<T: Expression> NamedAction for EvaluateUpdateAction<T> {
    fn name(&self) -> &'static str {
        "EvaluateUpdateAction"
    }
}
impl<T: Expression> SerializableAction for EvaluateUpdateAction<T> {
    fn serialize(&self) -> SerializedAction {
        SerializedAction::from_iter([
            ("cache_id", JsonValue::from(self.cache_id)),
            (
                "state_index",
                match self.state_index {
                    None => JsonValue::Null,
                    Some(value) => value.into(),
                },
            ),
            (
                "num_updates",
                JsonValue::Number(self.state_updates.len().into()),
            ),
        ])
    }
}

#[derive(Clone, Debug)]
pub struct EvaluateStopAction {
    pub cache_id: StateToken,
}
impl Action for EvaluateStopAction {}
impl NamedAction for EvaluateStopAction {
    fn name(&self) -> &'static str {
        "EvaluateStopAction"
    }
}
impl SerializableAction for EvaluateStopAction {
    fn serialize(&self) -> SerializedAction {
        SerializedAction::from_iter([("cache_id", JsonValue::from(self.cache_id))])
    }
}

#[derive(Clone, Debug)]
pub struct EvaluateResultAction<T: Expression> {
    pub cache_id: StateToken,
    pub state_index: Option<MessageOffset>,
    pub result: EvaluationResult<T>,
}
impl<T: Expression> Action for EvaluateResultAction<T> {}
impl<T: Expression> NamedAction for EvaluateResultAction<T> {
    fn name(&self) -> &'static str {
        "EvaluateResultAction"
    }
}
impl<T: Expression> SerializableAction for EvaluateResultAction<T> {
    fn serialize(&self) -> SerializedAction {
        SerializedAction::from_iter([
            ("cache_id", JsonValue::from(self.cache_id)),
            (
                "state_index",
                match self.state_index {
                    None => JsonValue::Null,
                    Some(value) => value.into(),
                },
            ),
            ("result_id", JsonValue::from(self.result.result().id())),
        ])
    }
}
