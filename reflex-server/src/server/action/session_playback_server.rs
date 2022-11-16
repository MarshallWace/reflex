// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use bytes::Bytes;
use http::{Request, Response};
use reflex_dispatcher::{Action, Named, SerializableAction, SerializedAction};
use reflex_json::{JsonMap, JsonValue};
use reflex_macros::Named;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use crate::{
    server::utils::{clone_http_request_wrapper, clone_http_response_wrapper},
    utils::serialize::{SerializedRequest, SerializedResponse},
};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum SessionPlaybackServerActions {
    HttpRequest(SessionPlaybackServerHttpRequestAction),
    HttpResponse(SessionPlaybackServerHttpResponseAction),
}
impl Named for SessionPlaybackServerActions {
    fn name(&self) -> &'static str {
        match self {
            Self::HttpRequest(action) => action.name(),
            Self::HttpResponse(action) => action.name(),
        }
    }
}
impl Action for SessionPlaybackServerActions {}
impl SerializableAction for SessionPlaybackServerActions {
    fn to_json(&self) -> SerializedAction {
        match self {
            Self::HttpRequest(action) => action.to_json(),
            Self::HttpResponse(action) => action.to_json(),
        }
    }
}

impl From<SessionPlaybackServerHttpRequestAction> for SessionPlaybackServerActions {
    fn from(value: SessionPlaybackServerHttpRequestAction) -> Self {
        Self::HttpRequest(value)
    }
}
impl From<SessionPlaybackServerActions> for Option<SessionPlaybackServerHttpRequestAction> {
    fn from(value: SessionPlaybackServerActions) -> Self {
        match value {
            SessionPlaybackServerActions::HttpRequest(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a SessionPlaybackServerActions>
    for Option<&'a SessionPlaybackServerHttpRequestAction>
{
    fn from(value: &'a SessionPlaybackServerActions) -> Self {
        match value {
            SessionPlaybackServerActions::HttpRequest(value) => Some(value),
            _ => None,
        }
    }
}

impl From<SessionPlaybackServerHttpResponseAction> for SessionPlaybackServerActions {
    fn from(value: SessionPlaybackServerHttpResponseAction) -> Self {
        Self::HttpResponse(value)
    }
}
impl From<SessionPlaybackServerActions> for Option<SessionPlaybackServerHttpResponseAction> {
    fn from(value: SessionPlaybackServerActions) -> Self {
        match value {
            SessionPlaybackServerActions::HttpResponse(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a SessionPlaybackServerActions>
    for Option<&'a SessionPlaybackServerHttpResponseAction>
{
    fn from(value: &'a SessionPlaybackServerActions) -> Self {
        match value {
            SessionPlaybackServerActions::HttpResponse(value) => Some(value),
            _ => None,
        }
    }
}

#[derive(Named, Debug)]
pub struct SessionPlaybackServerHttpRequestAction {
    pub request_id: Uuid,
    pub request: Request<Bytes>,
}
impl Clone for SessionPlaybackServerHttpRequestAction {
    fn clone(&self) -> Self {
        Self {
            request_id: self.request_id.clone(),
            request: clone_http_request_wrapper(&self.request).map(|_| self.request.body().clone()),
        }
    }
}
impl Action for SessionPlaybackServerHttpRequestAction {}
impl SerializableAction for SessionPlaybackServerHttpRequestAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([(
            "request_id",
            JsonValue::from(format!("{}", self.request_id.as_hyphenated())),
        )])
    }
}
impl Serialize for SessionPlaybackServerHttpRequestAction {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        SerializedSessionPlaybackServerHttpRequestAction::from(self).serialize(serializer)
    }
}
impl<'de> Deserialize<'de> for SessionPlaybackServerHttpRequestAction {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        SerializedSessionPlaybackServerHttpRequestAction::deserialize(deserializer).map(Into::into)
    }
}
#[derive(Clone, Serialize, Deserialize)]
struct SerializedSessionPlaybackServerHttpRequestAction {
    request_id: Uuid,
    request: SerializedRequest,
}
impl<'a> From<&'a SessionPlaybackServerHttpRequestAction>
    for SerializedSessionPlaybackServerHttpRequestAction
{
    fn from(value: &'a SessionPlaybackServerHttpRequestAction) -> Self {
        let SessionPlaybackServerHttpRequestAction {
            request_id,
            request,
        } = value;
        Self {
            request_id: *request_id,
            request: request.into(),
        }
    }
}
impl From<SerializedSessionPlaybackServerHttpRequestAction>
    for SessionPlaybackServerHttpRequestAction
{
    fn from(value: SerializedSessionPlaybackServerHttpRequestAction) -> Self {
        let SerializedSessionPlaybackServerHttpRequestAction {
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
pub struct SessionPlaybackServerHttpResponseAction {
    pub request_id: Uuid,
    pub response: Response<Bytes>,
}
impl Clone for SessionPlaybackServerHttpResponseAction {
    fn clone(&self) -> Self {
        Self {
            request_id: self.request_id.clone(),
            response: clone_http_response_wrapper(&self.response)
                .map(|_| self.response.body().clone()),
        }
    }
}
impl Action for SessionPlaybackServerHttpResponseAction {}
impl SerializableAction for SessionPlaybackServerHttpResponseAction {
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
                        match String::from_utf8(self.response.body().iter().copied().collect()).ok()
                        {
                            Some(body) => JsonValue::from(body),
                            None => JsonValue::Null,
                        },
                    ),
                ])),
            ),
        ])
    }
}
impl Serialize for SessionPlaybackServerHttpResponseAction {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        SerializedSessionPlaybackServerHttpResponseAction::from(self).serialize(serializer)
    }
}
impl<'de> Deserialize<'de> for SessionPlaybackServerHttpResponseAction {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        SerializedSessionPlaybackServerHttpResponseAction::deserialize(deserializer).map(Into::into)
    }
}
#[derive(Clone, Serialize, Deserialize)]
struct SerializedSessionPlaybackServerHttpResponseAction {
    request_id: Uuid,
    response: SerializedResponse,
}
impl<'a> From<&'a SessionPlaybackServerHttpResponseAction>
    for SerializedSessionPlaybackServerHttpResponseAction
{
    fn from(value: &'a SessionPlaybackServerHttpResponseAction) -> Self {
        let SessionPlaybackServerHttpResponseAction {
            request_id,
            response,
        } = value;
        Self {
            request_id: *request_id,
            response: response.into(),
        }
    }
}
impl From<SerializedSessionPlaybackServerHttpResponseAction>
    for SessionPlaybackServerHttpResponseAction
{
    fn from(value: SerializedSessionPlaybackServerHttpResponseAction) -> Self {
        let SerializedSessionPlaybackServerHttpResponseAction {
            request_id,
            response,
        } = value;
        Self {
            request_id,
            response: response.into(),
        }
    }
}
