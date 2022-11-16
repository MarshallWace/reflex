// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::time::SystemTime;

use reflex::core::Uuid;
use reflex_dispatcher::{Action, Named, SerializableAction, SerializedAction};
use reflex_json::JsonValue;
use serde::{Deserialize, Serialize};

use crate::utils::timestamp::get_timestamp_millis;

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub enum TimestampHandlerActions {
    Update(TimestampHandlerUpdateAction),
}
impl Action for TimestampHandlerActions {}
impl Named for TimestampHandlerActions {
    fn name(&self) -> &'static str {
        match self {
            Self::Update(action) => action.name(),
        }
    }
}
impl SerializableAction for TimestampHandlerActions {
    fn to_json(&self) -> SerializedAction {
        match self {
            Self::Update(action) => action.to_json(),
        }
    }
}

impl From<TimestampHandlerUpdateAction> for TimestampHandlerActions {
    fn from(value: TimestampHandlerUpdateAction) -> Self {
        Self::Update(value)
    }
}
impl From<TimestampHandlerActions> for Option<TimestampHandlerUpdateAction> {
    fn from(value: TimestampHandlerActions) -> Self {
        match value {
            TimestampHandlerActions::Update(value) => Some(value),
        }
    }
}
impl<'a> From<&'a TimestampHandlerActions> for Option<&'a TimestampHandlerUpdateAction> {
    fn from(value: &'a TimestampHandlerActions) -> Self {
        match value {
            TimestampHandlerActions::Update(value) => Some(value),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct TimestampHandlerUpdateAction {
    pub operation_id: Uuid,
    pub timestamp: SystemTime,
}
impl Action for TimestampHandlerUpdateAction {}
impl Named for TimestampHandlerUpdateAction {
    fn name(&self) -> &'static str {
        "TimestampHandlerUpdateAction"
    }
}
impl SerializableAction for TimestampHandlerUpdateAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([
            (
                "operation_id",
                JsonValue::from(self.operation_id.to_string()),
            ),
            (
                "timestamp",
                JsonValue::from(get_timestamp_millis(self.timestamp)),
            ),
        ])
    }
}
