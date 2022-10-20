// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use bytes::Bytes;
use http::StatusCode;
use reflex::core::Uuid;
use reflex_dispatcher::{Action, NamedAction, SerializableAction, SerializedAction};
use reflex_json::JsonValue;
use serde::{Deserialize, Serialize};

use crate::utils::serialize::SerializedBytes;

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub enum FetchHandlerActions {
    FetchComplete(FetchHandlerFetchCompleteAction),
    ConnectionError(FetchHandlerConnectionErrorAction),
}
impl Action for FetchHandlerActions {}
impl NamedAction for FetchHandlerActions {
    fn name(&self) -> &'static str {
        match self {
            Self::FetchComplete(action) => action.name(),
            Self::ConnectionError(action) => action.name(),
        }
    }
}
impl SerializableAction for FetchHandlerActions {
    fn to_json(&self) -> SerializedAction {
        match self {
            Self::FetchComplete(action) => action.to_json(),
            Self::ConnectionError(action) => action.to_json(),
        }
    }
}

impl From<FetchHandlerFetchCompleteAction> for FetchHandlerActions {
    fn from(value: FetchHandlerFetchCompleteAction) -> Self {
        Self::FetchComplete(value)
    }
}
impl From<FetchHandlerActions> for Option<FetchHandlerFetchCompleteAction> {
    fn from(value: FetchHandlerActions) -> Self {
        match value {
            FetchHandlerActions::FetchComplete(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a FetchHandlerActions> for Option<&'a FetchHandlerFetchCompleteAction> {
    fn from(value: &'a FetchHandlerActions) -> Self {
        match value {
            FetchHandlerActions::FetchComplete(value) => Some(value),
            _ => None,
        }
    }
}

impl From<FetchHandlerConnectionErrorAction> for FetchHandlerActions {
    fn from(value: FetchHandlerConnectionErrorAction) -> Self {
        Self::ConnectionError(value)
    }
}
impl From<FetchHandlerActions> for Option<FetchHandlerConnectionErrorAction> {
    fn from(value: FetchHandlerActions) -> Self {
        match value {
            FetchHandlerActions::ConnectionError(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a FetchHandlerActions> for Option<&'a FetchHandlerConnectionErrorAction> {
    fn from(value: &'a FetchHandlerActions) -> Self {
        match value {
            FetchHandlerActions::ConnectionError(value) => Some(value),
            _ => None,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct FetchHandlerFetchCompleteAction {
    pub operation_id: Uuid,
    pub url: String,
    pub status_code: StatusCode,
    pub body: Bytes,
}
impl Action for FetchHandlerFetchCompleteAction {}
impl NamedAction for FetchHandlerFetchCompleteAction {
    fn name(&self) -> &'static str {
        "FetchHandlerFetchCompleteAction"
    }
}
impl SerializableAction for FetchHandlerFetchCompleteAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([
            (
                "operation_id",
                JsonValue::from(self.operation_id.to_string()),
            ),
            ("url", JsonValue::from(self.url.clone())),
            ("status_code", JsonValue::from(self.status_code.as_u16())),
            ("content_length", JsonValue::from(self.body.len())),
        ])
    }
}
impl Serialize for FetchHandlerFetchCompleteAction {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        SerializedFetchHandlerFetchCompleteAction::from(self).serialize(serializer)
    }
}
impl<'de> Deserialize<'de> for FetchHandlerFetchCompleteAction {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        SerializedFetchHandlerFetchCompleteAction::deserialize(deserializer).map(Into::into)
    }
}
#[derive(Clone, Serialize, Deserialize)]
struct SerializedFetchHandlerFetchCompleteAction {
    operation_id: u128,
    url: String,
    status_code: u16,
    body: SerializedBytes,
}
impl<'a> From<&'a FetchHandlerFetchCompleteAction> for SerializedFetchHandlerFetchCompleteAction {
    fn from(value: &'a FetchHandlerFetchCompleteAction) -> Self {
        let FetchHandlerFetchCompleteAction {
            operation_id,
            url,
            status_code,
            body,
        } = value;
        Self {
            operation_id: operation_id.as_u128(),
            url: url.into(),
            status_code: status_code.as_u16(),
            body: body.into(),
        }
    }
}
impl From<SerializedFetchHandlerFetchCompleteAction> for FetchHandlerFetchCompleteAction {
    fn from(value: SerializedFetchHandlerFetchCompleteAction) -> Self {
        let SerializedFetchHandlerFetchCompleteAction {
            operation_id,
            url,
            status_code,
            body,
        } = value;
        Self {
            operation_id: Uuid::from_u128(operation_id),
            url: url.into(),
            status_code: StatusCode::from_u16(status_code).unwrap_or_default(),
            body: body.into(),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct FetchHandlerConnectionErrorAction {
    pub operation_id: Uuid,
    pub url: String,
    pub message: String,
}
impl Action for FetchHandlerConnectionErrorAction {}
impl NamedAction for FetchHandlerConnectionErrorAction {
    fn name(&self) -> &'static str {
        "FetchHandlerConnectionErrorAction"
    }
}
impl SerializableAction for FetchHandlerConnectionErrorAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([
            (
                "operation_id",
                JsonValue::from(self.operation_id.to_string()),
            ),
            ("url", JsonValue::from(self.url.clone())),
            ("message", JsonValue::from(self.message.clone())),
        ])
    }
}
