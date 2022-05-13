// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex_dispatcher::{Action, NamedAction, SerializableAction, SerializedAction};
use reflex_json::JsonValue;

#[derive(Clone, Debug)]
pub enum OpenTelemetryMiddlewareAction {
    Error(OpenTelemetryMiddlewareErrorAction),
}
impl Action for OpenTelemetryMiddlewareAction {}
impl NamedAction for OpenTelemetryMiddlewareAction {
    fn name(&self) -> &'static str {
        match self {
            Self::Error(action) => action.name(),
        }
    }
}
impl SerializableAction for OpenTelemetryMiddlewareAction {
    fn serialize(&self) -> SerializedAction {
        match self {
            Self::Error(action) => action.serialize(),
        }
    }
}

impl From<OpenTelemetryMiddlewareErrorAction> for OpenTelemetryMiddlewareAction {
    fn from(value: OpenTelemetryMiddlewareErrorAction) -> Self {
        Self::Error(value)
    }
}
impl From<OpenTelemetryMiddlewareAction> for Option<OpenTelemetryMiddlewareErrorAction> {
    fn from(value: OpenTelemetryMiddlewareAction) -> Self {
        match value {
            OpenTelemetryMiddlewareAction::Error(value) => Some(value),
        }
    }
}
impl<'a> From<&'a OpenTelemetryMiddlewareAction>
    for Option<&'a OpenTelemetryMiddlewareErrorAction>
{
    fn from(value: &'a OpenTelemetryMiddlewareAction) -> Self {
        match value {
            OpenTelemetryMiddlewareAction::Error(value) => Some(value),
        }
    }
}

#[derive(Clone, Debug)]
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
    fn serialize(&self) -> SerializedAction {
        SerializedAction::from_iter([("error", JsonValue::String(self.error.clone()))])
    }
}
