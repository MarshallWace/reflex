// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex_dispatcher::{Action, NamedAction, SerializableAction, SerializedAction};
use reflex_json::JsonValue;
use serde::{Deserialize, Serialize};

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub enum OpenTelemetryMiddlewareActions {
    Error(OpenTelemetryMiddlewareErrorAction),
}
impl Action for OpenTelemetryMiddlewareActions {}
impl NamedAction for OpenTelemetryMiddlewareActions {
    fn name(&self) -> &'static str {
        match self {
            Self::Error(action) => action.name(),
        }
    }
}
impl SerializableAction for OpenTelemetryMiddlewareActions {
    fn to_json(&self) -> SerializedAction {
        match self {
            Self::Error(action) => action.to_json(),
        }
    }
}

impl From<OpenTelemetryMiddlewareErrorAction> for OpenTelemetryMiddlewareActions {
    fn from(value: OpenTelemetryMiddlewareErrorAction) -> Self {
        Self::Error(value)
    }
}
impl From<OpenTelemetryMiddlewareActions> for Option<OpenTelemetryMiddlewareErrorAction> {
    fn from(value: OpenTelemetryMiddlewareActions) -> Self {
        match value {
            OpenTelemetryMiddlewareActions::Error(value) => Some(value),
        }
    }
}
impl<'a> From<&'a OpenTelemetryMiddlewareActions>
    for Option<&'a OpenTelemetryMiddlewareErrorAction>
{
    fn from(value: &'a OpenTelemetryMiddlewareActions) -> Self {
        match value {
            OpenTelemetryMiddlewareActions::Error(value) => Some(value),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct OpenTelemetryMiddlewareErrorAction {
    pub error: String,
}
impl Action for OpenTelemetryMiddlewareErrorAction {}
impl NamedAction for OpenTelemetryMiddlewareErrorAction {
    fn name(&self) -> &'static str {
        "OpenTelemetryMiddlewareErrorAction"
    }
}
impl SerializableAction for OpenTelemetryMiddlewareErrorAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([("error", JsonValue::String(self.error.clone()))])
    }
}
