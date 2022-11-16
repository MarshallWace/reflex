// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::Uuid;
use reflex_dispatcher::{Action, Named, SerializableAction, SerializedAction};
use reflex_json::JsonValue;
use serde::{Deserialize, Serialize};

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub enum TimeoutHandlerActions {
    Timeout(TimeoutHandlerTimeoutAction),
}
impl Action for TimeoutHandlerActions {}
impl Named for TimeoutHandlerActions {
    fn name(&self) -> &'static str {
        match self {
            Self::Timeout(action) => action.name(),
        }
    }
}
impl SerializableAction for TimeoutHandlerActions {
    fn to_json(&self) -> SerializedAction {
        match self {
            Self::Timeout(action) => action.to_json(),
        }
    }
}

impl From<TimeoutHandlerTimeoutAction> for TimeoutHandlerActions {
    fn from(value: TimeoutHandlerTimeoutAction) -> Self {
        Self::Timeout(value)
    }
}
impl From<TimeoutHandlerActions> for Option<TimeoutHandlerTimeoutAction> {
    fn from(value: TimeoutHandlerActions) -> Self {
        match value {
            TimeoutHandlerActions::Timeout(value) => Some(value),
        }
    }
}
impl<'a> From<&'a TimeoutHandlerActions> for Option<&'a TimeoutHandlerTimeoutAction> {
    fn from(value: &'a TimeoutHandlerActions) -> Self {
        match value {
            TimeoutHandlerActions::Timeout(value) => Some(value),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct TimeoutHandlerTimeoutAction {
    pub operation_id: Uuid,
}
impl Action for TimeoutHandlerTimeoutAction {}
impl Named for TimeoutHandlerTimeoutAction {
    fn name(&self) -> &'static str {
        "TimeoutHandlerTimeoutAction"
    }
}
impl SerializableAction for TimeoutHandlerTimeoutAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([(
            "operation_id",
            JsonValue::from(self.operation_id.to_string()),
        )])
    }
}
