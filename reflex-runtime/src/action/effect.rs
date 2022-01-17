// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{Expression, Signal, StateToken};
use reflex_dispatcher::{
    Action, NamedAction, SerializableAction, SerializedAction,
};
use reflex_json::{JsonMap, JsonValue};

use crate::StateUpdate;

#[derive(Clone, Debug)]
pub enum EffectAction<T: Expression> {
    Subscribe(EffectSubscribeAction<T>),
    Unsubscribe(EffectUnsubscribeAction<T>),
    Emit(EffectEmitAction<T>),
}
impl<T: Expression> Action for EffectAction<T> {}
impl<T: Expression> NamedAction for EffectAction<T> {
    fn name(&self) -> &'static str {
        match self {
            Self::Subscribe(action) => action.name(),
            Self::Unsubscribe(action) => action.name(),
            Self::Emit(action) => action.name(),
        }
    }
}
impl<T: Expression> SerializableAction for EffectAction<T> {
    fn serialize(&self) -> SerializedAction {
        match self {
            Self::Subscribe(action) => action.serialize(),
            Self::Unsubscribe(action) => action.serialize(),
            Self::Emit(action) => action.serialize(),
        }
    }
}

impl<T: Expression> From<EffectSubscribeAction<T>> for EffectAction<T> {
    fn from(value: EffectSubscribeAction<T>) -> Self {
        Self::Subscribe(value)
    }
}
impl<T: Expression> From<EffectAction<T>> for Option<EffectSubscribeAction<T>> {
    fn from(value: EffectAction<T>) -> Self {
        match value {
            EffectAction::Subscribe(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a EffectAction<T>> for Option<&'a EffectSubscribeAction<T>> {
    fn from(value: &'a EffectAction<T>) -> Self {
        match value {
            EffectAction::Subscribe(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<EffectUnsubscribeAction<T>> for EffectAction<T> {
    fn from(value: EffectUnsubscribeAction<T>) -> Self {
        Self::Unsubscribe(value)
    }
}
impl<T: Expression> From<EffectAction<T>> for Option<EffectUnsubscribeAction<T>> {
    fn from(value: EffectAction<T>) -> Self {
        match value {
            EffectAction::Unsubscribe(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a EffectAction<T>> for Option<&'a EffectUnsubscribeAction<T>> {
    fn from(value: &'a EffectAction<T>) -> Self {
        match value {
            EffectAction::Unsubscribe(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<EffectEmitAction<T>> for EffectAction<T> {
    fn from(value: EffectEmitAction<T>) -> Self {
        Self::Emit(value)
    }
}
impl<T: Expression> From<EffectAction<T>> for Option<EffectEmitAction<T>> {
    fn from(value: EffectAction<T>) -> Self {
        match value {
            EffectAction::Emit(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a EffectAction<T>> for Option<&'a EffectEmitAction<T>> {
    fn from(value: &'a EffectAction<T>) -> Self {
        match value {
            EffectAction::Emit(value) => Some(value),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct EffectSubscribeAction<T: Expression> {
    pub effect_type: String,
    pub effects: Vec<Signal<T>>,
}
impl<T: Expression> Action for EffectSubscribeAction<T> {}
impl<T: Expression> NamedAction for EffectSubscribeAction<T> {
    fn name(&self) -> &'static str {
        "EffectSubscribeAction"
    }
}
impl<T: Expression> SerializableAction for EffectSubscribeAction<T> {
    fn serialize(&self) -> SerializedAction {
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
                                        signal.args().iter().map(sanitize_expression).collect(),
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

#[derive(Clone, Debug)]
pub struct EffectUnsubscribeAction<T: Expression> {
    pub effect_type: String,
    pub effects: Vec<Signal<T>>,
}
impl<T: Expression> Action for EffectUnsubscribeAction<T> {}
impl<T: Expression> NamedAction for EffectUnsubscribeAction<T> {
    fn name(&self) -> &'static str {
        "EffectUnsubscribeAction"
    }
}
impl<T: Expression> SerializableAction for EffectUnsubscribeAction<T> {
    fn serialize(&self) -> SerializedAction {
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
                                        signal.args().iter().map(sanitize_expression).collect(),
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

#[derive(Clone, Debug)]
pub struct EffectEmitAction<T: Expression> {
    pub updates: Vec<(StateToken, StateUpdate<T>)>,
}
impl<T: Expression> Action for EffectEmitAction<T> {}
impl<T: Expression> NamedAction for EffectEmitAction<T> {
    fn name(&self) -> &'static str {
        "EffectEmitAction"
    }
}
impl<T: Expression> SerializableAction for EffectEmitAction<T> {
    fn serialize(&self) -> SerializedAction {
        SerializedAction::from_iter([(
            "updates",
            JsonValue::from(
                self.updates
                    .iter()
                    .map(|(state_token, update)| {
                        JsonValue::Object(JsonMap::from_iter([
                            (String::from("id"), JsonValue::from(*state_token)),
                            (
                                String::from("update"),
                                match update {
                                    StateUpdate::Value(value) => {
                                        JsonValue::Object(JsonMap::from_iter([
                                            (String::from("type"), JsonValue::from("value")),
                                            (String::from("id"), JsonValue::from(value.id())),
                                        ]))
                                    }
                                    StateUpdate::Patch(_) => JsonValue::Object(JsonMap::from_iter(
                                        [(String::from("type"), JsonValue::from("patch"))],
                                    )),
                                },
                            ),
                        ]))
                    })
                    .collect::<Vec<_>>(),
            ),
        )])
    }
}

fn sanitize_expression<T: Expression>(arg: &T) -> JsonValue {
    reflex_json::sanitize(arg).unwrap_or_else(|_| {
        JsonValue::Object(JsonMap::from_iter([(
            String::from("__hash"),
            JsonValue::from(arg.id()),
        )]))
    })
}
