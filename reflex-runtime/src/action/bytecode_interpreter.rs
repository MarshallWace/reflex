// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{EvaluationResult, Expression, StateToken};
use reflex_dispatcher::{Action, MessageOffset, NamedAction, SerializableAction, SerializedAction};
use reflex_json::JsonValue;
use serde::{Deserialize, Serialize};

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub enum BytecodeInterpreterActions<T: Expression> {
    #[serde(bound(
        serialize = "<T as Expression>::Signal<T>: Serialize",
        deserialize = "<T as Expression>::Signal<T>: Deserialize<'de>"
    ))]
    Evaluate(BytecodeInterpreterEvaluateAction<T>),
    Result(BytecodeInterpreterResultAction<T>),
    Gc(BytecodeInterpreterGcAction),
}
impl<T: Expression> Action for BytecodeInterpreterActions<T> {}
impl<T: Expression> NamedAction for BytecodeInterpreterActions<T> {
    fn name(&self) -> &'static str {
        match self {
            Self::Evaluate(action) => action.name(),
            Self::Result(action) => action.name(),
            Self::Gc(action) => action.name(),
        }
    }
}
impl<T: Expression> SerializableAction for BytecodeInterpreterActions<T> {
    fn to_json(&self) -> SerializedAction {
        match self {
            Self::Evaluate(action) => action.to_json(),
            Self::Result(action) => action.to_json(),
            Self::Gc(action) => action.to_json(),
        }
    }
}

impl<T: Expression> From<BytecodeInterpreterEvaluateAction<T>> for BytecodeInterpreterActions<T> {
    fn from(value: BytecodeInterpreterEvaluateAction<T>) -> Self {
        Self::Evaluate(value)
    }
}
impl<T: Expression> From<BytecodeInterpreterActions<T>>
    for Option<BytecodeInterpreterEvaluateAction<T>>
{
    fn from(value: BytecodeInterpreterActions<T>) -> Self {
        match value {
            BytecodeInterpreterActions::Evaluate(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a BytecodeInterpreterActions<T>>
    for Option<&'a BytecodeInterpreterEvaluateAction<T>>
{
    fn from(value: &'a BytecodeInterpreterActions<T>) -> Self {
        match value {
            BytecodeInterpreterActions::Evaluate(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<BytecodeInterpreterResultAction<T>> for BytecodeInterpreterActions<T> {
    fn from(value: BytecodeInterpreterResultAction<T>) -> Self {
        Self::Result(value)
    }
}
impl<T: Expression> From<BytecodeInterpreterActions<T>>
    for Option<BytecodeInterpreterResultAction<T>>
{
    fn from(value: BytecodeInterpreterActions<T>) -> Self {
        match value {
            BytecodeInterpreterActions::Result(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a BytecodeInterpreterActions<T>>
    for Option<&'a BytecodeInterpreterResultAction<T>>
{
    fn from(value: &'a BytecodeInterpreterActions<T>) -> Self {
        match value {
            BytecodeInterpreterActions::Result(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<BytecodeInterpreterGcAction> for BytecodeInterpreterActions<T> {
    fn from(value: BytecodeInterpreterGcAction) -> Self {
        Self::Gc(value)
    }
}
impl<T: Expression> From<BytecodeInterpreterActions<T>> for Option<BytecodeInterpreterGcAction> {
    fn from(value: BytecodeInterpreterActions<T>) -> Self {
        match value {
            BytecodeInterpreterActions::Gc(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a BytecodeInterpreterActions<T>>
    for Option<&'a BytecodeInterpreterGcAction>
{
    fn from(value: &'a BytecodeInterpreterActions<T>) -> Self {
        match value {
            BytecodeInterpreterActions::Gc(value) => Some(value),
            _ => None,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct BytecodeInterpreterEvaluateAction<T: Expression> {
    pub cache_id: StateToken,
    pub state_index: Option<MessageOffset>,
    pub state_updates: Vec<(StateToken, T)>,
}
impl<T: Expression> Action for BytecodeInterpreterEvaluateAction<T> {}
impl<T: Expression> NamedAction for BytecodeInterpreterEvaluateAction<T> {
    fn name(&self) -> &'static str {
        "BytecodeInterpreterEvaluateAction"
    }
}
impl<T: Expression> SerializableAction for BytecodeInterpreterEvaluateAction<T> {
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
pub struct BytecodeInterpreterResultAction<T: Expression> {
    pub cache_id: StateToken,
    pub state_index: Option<MessageOffset>,
    pub result: EvaluationResult<T>,
}
impl<T: Expression> Action for BytecodeInterpreterResultAction<T> {}
impl<T: Expression> NamedAction for BytecodeInterpreterResultAction<T> {
    fn name(&self) -> &'static str {
        "BytecodeInterpreterResultAction"
    }
}
impl<T: Expression> SerializableAction for BytecodeInterpreterResultAction<T> {
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

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct BytecodeInterpreterGcAction {
    pub cache_id: StateToken,
    pub state_index: Option<MessageOffset>,
}
impl Action for BytecodeInterpreterGcAction {}
impl NamedAction for BytecodeInterpreterGcAction {
    fn name(&self) -> &'static str {
        "BytecodeInterpreterGcAction"
    }
}
impl SerializableAction for BytecodeInterpreterGcAction {
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
        ])
    }
}
