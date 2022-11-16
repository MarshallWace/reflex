// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use bytes::Bytes;
use http::{Request, Response};
use reflex::core::Uuid;
use reflex_dispatcher::{Action, Named, SerializableAction, SerializedAction};
use reflex_json::{JsonMap, JsonValue};
use reflex_macros::Named;
use serde::{Deserialize, Serialize};

use crate::{
    server::utils::{clone_http_request_wrapper, clone_http_response_wrapper},
    utils::serialize::{SerializedRequest, SerializedResponse},
};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum HttpServerActions {
    Request(HttpServerRequestAction),
    Response(HttpServerResponseAction),
}
impl Named for HttpServerActions {
    fn name(&self) -> &'static str {
        match self {
            Self::Request(action) => action.name(),
            Self::Response(action) => action.name(),
        }
    }
}
impl Action for HttpServerActions {}
impl SerializableAction for HttpServerActions {
    fn to_json(&self) -> SerializedAction {
        match self {
            Self::Request(action) => action.to_json(),
            Self::Response(action) => action.to_json(),
        }
    }
}

impl From<HttpServerRequestAction> for HttpServerActions {
    fn from(value: HttpServerRequestAction) -> Self {
        Self::Request(value)
    }
}
impl From<HttpServerActions> for Option<HttpServerRequestAction> {
    fn from(value: HttpServerActions) -> Self {
        match value {
            HttpServerActions::Request(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a HttpServerActions> for Option<&'a HttpServerRequestAction> {
    fn from(value: &'a HttpServerActions) -> Self {
        match value {
            HttpServerActions::Request(value) => Some(value),
            _ => None,
        }
    }
}

impl From<HttpServerResponseAction> for HttpServerActions {
    fn from(value: HttpServerResponseAction) -> Self {
        Self::Response(value)
    }
}
impl From<HttpServerActions> for Option<HttpServerResponseAction> {
    fn from(value: HttpServerActions) -> Self {
        match value {
            HttpServerActions::Response(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a HttpServerActions> for Option<&'a HttpServerResponseAction> {
    fn from(value: &'a HttpServerActions) -> Self {
        match value {
            HttpServerActions::Response(value) => Some(value),
            _ => None,
        }
    }
}

#[derive(Named, Debug)]
pub struct HttpServerRequestAction {
    pub request_id: Uuid,
    pub request: Request<Bytes>,
}
impl Clone for HttpServerRequestAction {
    fn clone(&self) -> Self {
        Self {
            request_id: self.request_id.clone(),
            request: clone_http_request_wrapper(&self.request).map(|_| self.request.body().clone()),
        }
    }
}
impl Action for HttpServerRequestAction {}
impl SerializableAction for HttpServerRequestAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([
            (
                "request_id",
                JsonValue::from(format!("{}", self.request_id.as_hyphenated())),
            ),
            (
                "request",
                JsonValue::Object(JsonMap::from_iter([
                    (
                        String::from("headers"),
                        JsonValue::Object(JsonMap::from_iter(
                            self.request.headers().iter().filter_map(|(key, value)| {
                                String::from_utf8(value.as_bytes().iter().copied().collect())
                                    .ok()
                                    .map(|value| {
                                        (String::from(key.as_str()), JsonValue::from(value))
                                    })
                            }),
                        )),
                    ),
                    (
                        String::from("body"),
                        if self.request.body().is_empty() {
                            JsonValue::Null
                        } else {
                            match String::from_utf8(self.request.body().iter().copied().collect())
                                .ok()
                            {
                                Some(body) => JsonValue::from(body),
                                None => JsonValue::Null,
                            }
                        },
                    ),
                ])),
            ),
        ])
    }
}
impl Serialize for HttpServerRequestAction {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        SerializedHttpServerRequestAction::from(self).serialize(serializer)
    }
}
impl<'de> Deserialize<'de> for HttpServerRequestAction {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        SerializedHttpServerRequestAction::deserialize(deserializer).map(Into::into)
    }
}
#[derive(Clone, Serialize, Deserialize)]
struct SerializedHttpServerRequestAction {
    request_id: Uuid,
    request: SerializedRequest,
}
impl<'a> From<&'a HttpServerRequestAction> for SerializedHttpServerRequestAction {
    fn from(value: &'a HttpServerRequestAction) -> Self {
        let HttpServerRequestAction {
            request_id,
            request,
        } = value;
        Self {
            request_id: *request_id,
            request: request.into(),
        }
    }
}
impl From<SerializedHttpServerRequestAction> for HttpServerRequestAction {
    fn from(value: SerializedHttpServerRequestAction) -> Self {
        let SerializedHttpServerRequestAction {
            request_id,
            request,
        } = value;
        Self {
            request_id,
            request: request.into(),
        }
    }
}

#[derive(Named, Debug)]
pub struct HttpServerResponseAction {
    pub request_id: Uuid,
    pub response: Response<Bytes>,
}
impl Clone for HttpServerResponseAction {
    fn clone(&self) -> Self {
        Self {
            request_id: self.request_id.clone(),
            response: clone_http_response_wrapper(&self.response)
                .map(|_| self.response.body().clone()),
        }
    }
}
impl Action for HttpServerResponseAction {}
impl SerializableAction for HttpServerResponseAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([
            (
                "request_id",
                JsonValue::from(format!("{}", self.request_id.as_hyphenated())),
            ),
            (
                "response",
                JsonValue::Object(JsonMap::from_iter([
                    (
                        String::from("status"),
                        JsonValue::from(self.response.status().as_u16()),
                    ),
                    (
                        String::from("headers"),
                        JsonValue::Object(JsonMap::from_iter(
                            self.response.headers().iter().filter_map(|(key, value)| {
                                String::from_utf8(value.as_bytes().iter().copied().collect())
                                    .ok()
                                    .map(|value| {
                                        (String::from(key.as_str()), JsonValue::from(value))
                                    })
                            }),
                        )),
                    ),
                    (
                        String::from("body"),
                        if self.response.body().is_empty() {
                            JsonValue::Null
                        } else {
                            match String::from_utf8(self.response.body().iter().copied().collect())
                                .ok()
                            {
                                Some(body) => JsonValue::from(body),
                                None => JsonValue::Null,
                            }
                        },
                    ),
                ])),
            ),
        ])
    }
}
impl Serialize for HttpServerResponseAction {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        SerializedHttpServerResponseAction::from(self).serialize(serializer)
    }
}
impl<'de> Deserialize<'de> for HttpServerResponseAction {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        SerializedHttpServerResponseAction::deserialize(deserializer).map(Into::into)
    }
}
#[derive(Clone, Serialize, Deserialize)]
struct SerializedHttpServerResponseAction {
    request_id: Uuid,
    response: SerializedResponse,
}
impl<'a> From<&'a HttpServerResponseAction> for SerializedHttpServerResponseAction {
    fn from(value: &'a HttpServerResponseAction) -> Self {
        let HttpServerResponseAction {
            request_id,
            response,
        } = value;
        Self {
            request_id: *request_id,
            response: response.into(),
        }
    }
}
impl From<SerializedHttpServerResponseAction> for HttpServerResponseAction {
    fn from(value: SerializedHttpServerResponseAction) -> Self {
        let SerializedHttpServerResponseAction {
            request_id,
            response,
        } = value;
        Self {
            request_id,
            response: response.into(),
        }
    }
}
