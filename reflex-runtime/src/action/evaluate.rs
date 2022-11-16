// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{EvaluationResult, Expression, StateToken};
use reflex_dispatcher::{Action, MessageOffset, Named, SerializableAction, SerializedAction};
use reflex_json::JsonValue;
use serde::{Deserialize, Serialize};

use crate::{QueryEvaluationMode, QueryInvalidationStrategy};

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub enum EvaluateActions<T: Expression> {
    Start(EvaluateStartAction<T>),
    Update(EvaluateUpdateAction<T>),
    Stop(EvaluateStopAction),
    Result(EvaluateResultAction<T>),
}
impl<T: Expression> Action for EvaluateActions<T> {}
impl<T: Expression> Named for EvaluateActions<T> {
    fn name(&self) -> &'static str {
        match self {
            Self::Start(action) => action.name(),
            Self::Update(action) => action.name(),
            Self::Stop(action) => action.name(),
            Self::Result(action) => action.name(),
        }
    }
}
impl<T: Expression> SerializableAction for EvaluateActions<T> {
    fn to_json(&self) -> SerializedAction {
        match self {
            Self::Start(action) => action.to_json(),
            Self::Update(action) => action.to_json(),
            Self::Stop(action) => action.to_json(),
            Self::Result(action) => action.to_json(),
        }
    }
}

impl<T: Expression> From<EvaluateStartAction<T>> for EvaluateActions<T> {
    fn from(value: EvaluateStartAction<T>) -> Self {
        Self::Start(value)
    }
}
impl<T: Expression> From<EvaluateActions<T>> for Option<EvaluateStartAction<T>> {
    fn from(value: EvaluateActions<T>) -> Self {
        match value {
            EvaluateActions::Start(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a EvaluateActions<T>> for Option<&'a EvaluateStartAction<T>> {
    fn from(value: &'a EvaluateActions<T>) -> Self {
        match value {
            EvaluateActions::Start(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<EvaluateUpdateAction<T>> for EvaluateActions<T> {
    fn from(value: EvaluateUpdateAction<T>) -> Self {
        Self::Update(value)
    }
}
impl<T: Expression> From<EvaluateActions<T>> for Option<EvaluateUpdateAction<T>> {
    fn from(value: EvaluateActions<T>) -> Self {
        match value {
            EvaluateActions::Update(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a EvaluateActions<T>> for Option<&'a EvaluateUpdateAction<T>> {
    fn from(value: &'a EvaluateActions<T>) -> Self {
        match value {
            EvaluateActions::Update(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<EvaluateStopAction> for EvaluateActions<T> {
    fn from(value: EvaluateStopAction) -> Self {
        Self::Stop(value)
    }
}
impl<T: Expression> From<EvaluateActions<T>> for Option<EvaluateStopAction> {
    fn from(value: EvaluateActions<T>) -> Self {
        match value {
            EvaluateActions::Stop(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a EvaluateActions<T>> for Option<&'a EvaluateStopAction> {
    fn from(value: &'a EvaluateActions<T>) -> Self {
        match value {
            EvaluateActions::Stop(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<EvaluateResultAction<T>> for EvaluateActions<T> {
    fn from(value: EvaluateResultAction<T>) -> Self {
        Self::Result(value)
    }
}
impl<T: Expression> From<EvaluateActions<T>> for Option<EvaluateResultAction<T>> {
    fn from(value: EvaluateActions<T>) -> Self {
        match value {
            EvaluateActions::Result(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a EvaluateActions<T>> for Option<&'a EvaluateResultAction<T>> {
    fn from(value: &'a EvaluateActions<T>) -> Self {
        match value {
            EvaluateActions::Result(value) => Some(value),
            _ => None,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct EvaluateStartAction<T: Expression> {
    pub cache_id: StateToken,
    pub label: String,
    pub query: T,
    pub evaluation_mode: QueryEvaluationMode,
    pub invalidation_strategy: QueryInvalidationStrategy,
}
impl<T: Expression> Action for EvaluateStartAction<T> {}
impl<T: Expression> Named for EvaluateStartAction<T> {
    fn name(&self) -> &'static str {
        "EvaluateStartAction"
    }
}
impl<T: Expression> SerializableAction for EvaluateStartAction<T> {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([
            ("cache_id", JsonValue::from(self.cache_id)),
            ("label", JsonValue::from(self.label.clone())),
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

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct EvaluateUpdateAction<T: Expression> {
    pub cache_id: StateToken,
    pub state_index: Option<MessageOffset>,
    pub state_updates: Vec<(StateToken, T)>,
}
impl<T: Expression> Action for EvaluateUpdateAction<T> {}
impl<T: Expression> Named for EvaluateUpdateAction<T> {
    fn name(&self) -> &'static str {
        "EvaluateUpdateAction"
    }
}
impl<T: Expression> SerializableAction for EvaluateUpdateAction<T> {
    fn to_json(&self) -> SerializedAction {
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

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct EvaluateStopAction {
    pub cache_id: StateToken,
}
impl Action for EvaluateStopAction {}
impl Named for EvaluateStopAction {
    fn name(&self) -> &'static str {
        "EvaluateStopAction"
    }
}
impl SerializableAction for EvaluateStopAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([("cache_id", JsonValue::from(self.cache_id))])
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct EvaluateResultAction<T: Expression> {
    pub cache_id: StateToken,
    pub state_index: Option<MessageOffset>,
    pub result: EvaluationResult<T>,
}
impl<T: Expression> Action for EvaluateResultAction<T> {}
impl<T: Expression> Named for EvaluateResultAction<T> {
    fn name(&self) -> &'static str {
        "EvaluateResultAction"
    }
}
impl<T: Expression> SerializableAction for EvaluateResultAction<T> {
    fn to_json(&self) -> SerializedAction {
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
