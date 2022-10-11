// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::borrow::Cow;

use reflex::core::{ConditionType, Expression, ExpressionListType, RefType, StateToken};
use reflex_dispatcher::{Action, NamedAction, SerializableAction, SerializedAction};
use reflex_json::{JsonMap, JsonValue};
use serde::{Deserialize, Serialize};

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub enum EffectActions<T: Expression> {
    #[serde(bound(
        serialize = "<T as Expression>::Signal<T>: Serialize",
        deserialize = "<T as Expression>::Signal<T>: Deserialize<'de>"
    ))]
    Subscribe(EffectSubscribeAction<T>),
    Unsubscribe(EffectUnsubscribeAction<T>),
    Emit(EffectEmitAction<T>),
}
impl<T: Expression> Action for EffectActions<T> {}
impl<T: Expression> NamedAction for EffectActions<T> {
    fn name(&self) -> &'static str {
        match self {
            Self::Subscribe(action) => action.name(),
            Self::Unsubscribe(action) => action.name(),
            Self::Emit(action) => action.name(),
        }
    }
}
impl<T: Expression> SerializableAction for EffectActions<T> {
    fn to_json(&self) -> SerializedAction {
        match self {
            Self::Subscribe(action) => action.to_json(),
            Self::Unsubscribe(action) => action.to_json(),
            Self::Emit(action) => action.to_json(),
        }
    }
}

impl<T: Expression> From<EffectSubscribeAction<T>> for EffectActions<T> {
    fn from(value: EffectSubscribeAction<T>) -> Self {
        Self::Subscribe(value)
    }
}
impl<T: Expression> From<EffectActions<T>> for Option<EffectSubscribeAction<T>> {
    fn from(value: EffectActions<T>) -> Self {
        match value {
            EffectActions::Subscribe(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a EffectActions<T>> for Option<&'a EffectSubscribeAction<T>> {
    fn from(value: &'a EffectActions<T>) -> Self {
        match value {
            EffectActions::Subscribe(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<EffectUnsubscribeAction<T>> for EffectActions<T> {
    fn from(value: EffectUnsubscribeAction<T>) -> Self {
        Self::Unsubscribe(value)
    }
}
impl<T: Expression> From<EffectActions<T>> for Option<EffectUnsubscribeAction<T>> {
    fn from(value: EffectActions<T>) -> Self {
        match value {
            EffectActions::Unsubscribe(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a EffectActions<T>> for Option<&'a EffectUnsubscribeAction<T>> {
    fn from(value: &'a EffectActions<T>) -> Self {
        match value {
            EffectActions::Unsubscribe(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<EffectEmitAction<T>> for EffectActions<T> {
    fn from(value: EffectEmitAction<T>) -> Self {
        Self::Emit(value)
    }
}
impl<T: Expression> From<EffectActions<T>> for Option<EffectEmitAction<T>> {
    fn from(value: EffectActions<T>) -> Self {
        match value {
            EffectActions::Emit(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a EffectActions<T>> for Option<&'a EffectEmitAction<T>> {
    fn from(value: &'a EffectActions<T>) -> Self {
        match value {
            EffectActions::Emit(value) => Some(value),
            _ => None,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct EffectSubscribeAction<T: Expression> {
    pub effect_type: String,
    #[serde(bound(
        serialize = "<T as Expression>::Signal<T>: Serialize",
        deserialize = "<T as Expression>::Signal<T>: Deserialize<'de>"
    ))]
    pub effects: Vec<T::Signal<T>>,
}
impl<T: Expression> Action for EffectSubscribeAction<T> {}
impl<T: Expression> NamedAction for EffectSubscribeAction<T> {
    fn name(&self) -> &'static str {
        "EffectSubscribeAction"
    }
}
impl<T: Expression> SerializableAction for EffectSubscribeAction<T> {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([
            ("effect_type", JsonValue::from(self.effect_type.clone())),
            (
                "effects",
                JsonValue::from(
                    self.effects
                        .iter()
                        .map(|signal| {
                            JsonValue::Object(JsonMap::from_iter([
                                (String::from("id"), JsonValue::from(signal.id())),
                                (
                                    String::from("args"),
                                    JsonValue::Array(
                                        signal
                                            .args()
                                            .as_deref()
                                            .iter()
                                            .map(|item| item.as_deref())
                                            .map(sanitize_expression)
                                            .collect(),
                                    ),
                                ),
                            ]))
                        })
                        .collect::<Vec<_>>(),
                ),
            ),
        ])
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct EffectUnsubscribeAction<T: Expression> {
    pub effect_type: String,
    #[serde(bound(
        serialize = "<T as Expression>::Signal<T>: Serialize",
        deserialize = "<T as Expression>::Signal<T>: Deserialize<'de>"
    ))]
    pub effects: Vec<T::Signal<T>>,
}
impl<T: Expression> Action for EffectUnsubscribeAction<T> {}
impl<T: Expression> NamedAction for EffectUnsubscribeAction<T> {
    fn name(&self) -> &'static str {
        "EffectUnsubscribeAction"
    }
}
impl<T: Expression> SerializableAction for EffectUnsubscribeAction<T> {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([
            ("effect_type", JsonValue::from(self.effect_type.clone())),
            (
                "effects",
                JsonValue::from(
                    self.effects
                        .iter()
                        .map(|signal| {
                            JsonValue::Object(JsonMap::from_iter([
                                (String::from("id"), JsonValue::from(signal.id())),
                                (
                                    String::from("args"),
                                    JsonValue::Array(
                                        signal
                                            .args()
                                            .as_deref()
                                            .iter()
                                            .map(|item| item.as_deref())
                                            .map(sanitize_expression)
                                            .collect(),
                                    ),
                                ),
                            ]))
                        })
                        .collect::<Vec<_>>(),
                ),
            ),
        ])
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct EffectEmitAction<T: Expression> {
    pub effect_types: Vec<EffectUpdateBatch<T>>,
}
impl<T: Expression> Action for EffectEmitAction<T> {}
impl<T: Expression> NamedAction for EffectEmitAction<T> {
    fn name(&self) -> &'static str {
        "EffectEmitAction"
    }
}
impl<T: Expression> SerializableAction for EffectEmitAction<T> {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([(
            "effect_types",
            JsonValue::from(
                self.effect_types
                    .iter()
                    .map(|batch| {
                        JsonValue::Object(JsonMap::from_iter([
                            (
                                String::from("effect_type"),
                                JsonValue::String(batch.effect_type.to_string()),
                            ),
                            (
                                String::from("updates"),
                                JsonValue::from(
                                    batch
                                        .updates
                                        .iter()
                                        .map(|(state_token, value)| {
                                            JsonValue::Object(JsonMap::from_iter([
                                                (String::from("id"), JsonValue::from(*state_token)),
                                                (
                                                    String::from("value"),
                                                    JsonValue::from(value.id()),
                                                ),
                                            ]))
                                        })
                                        .collect::<Vec<_>>(),
                                ),
                            ),
                        ]))
                    })
                    .collect::<Vec<_>>(),
            ),
        )])
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct EffectUpdateBatch<T: Expression> {
    pub effect_type: Cow<'static, str>,
    pub updates: Vec<(StateToken, T)>,
}

fn sanitize_expression<T: Expression>(arg: &T) -> JsonValue {
    reflex_json::sanitize(arg).unwrap_or_else(|_| {
        JsonValue::Object(JsonMap::from_iter([(
            String::from("__hash"),
            JsonValue::from(arg.id()),
        )]))
    })
}
