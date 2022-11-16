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
pub enum QueryInspectorServerActions {
    HttpRequest(QueryInspectorServerHttpRequestAction),
    HttpResponse(QueryInspectorServerHttpResponseAction),
}
impl Named for QueryInspectorServerActions {
    fn name(&self) -> &'static str {
        match self {
            Self::HttpRequest(action) => action.name(),
            Self::HttpResponse(action) => action.name(),
        }
    }
}
impl Action for QueryInspectorServerActions {}
impl SerializableAction for QueryInspectorServerActions {
    fn to_json(&self) -> SerializedAction {
        match self {
            Self::HttpRequest(action) => action.to_json(),
            Self::HttpResponse(action) => action.to_json(),
        }
    }
}

impl From<QueryInspectorServerHttpRequestAction> for QueryInspectorServerActions {
    fn from(value: QueryInspectorServerHttpRequestAction) -> Self {
        Self::HttpRequest(value)
    }
}
impl From<QueryInspectorServerActions> for Option<QueryInspectorServerHttpRequestAction> {
    fn from(value: QueryInspectorServerActions) -> Self {
        match value {
            QueryInspectorServerActions::HttpRequest(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a QueryInspectorServerActions>
    for Option<&'a QueryInspectorServerHttpRequestAction>
{
    fn from(value: &'a QueryInspectorServerActions) -> Self {
        match value {
            QueryInspectorServerActions::HttpRequest(value) => Some(value),
            _ => None,
        }
    }
}

impl From<QueryInspectorServerHttpResponseAction> for QueryInspectorServerActions {
    fn from(value: QueryInspectorServerHttpResponseAction) -> Self {
        Self::HttpResponse(value)
    }
}
impl From<QueryInspectorServerActions> for Option<QueryInspectorServerHttpResponseAction> {
    fn from(value: QueryInspectorServerActions) -> Self {
        match value {
            QueryInspectorServerActions::HttpResponse(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a QueryInspectorServerActions>
    for Option<&'a QueryInspectorServerHttpResponseAction>
{
    fn from(value: &'a QueryInspectorServerActions) -> Self {
        match value {
            QueryInspectorServerActions::HttpResponse(value) => Some(value),
            _ => None,
        }
    }
}

#[derive(Named, Debug)]
pub struct QueryInspectorServerHttpRequestAction {
    pub request_id: Uuid,
    pub request: Request<Bytes>,
}
impl Clone for QueryInspectorServerHttpRequestAction {
    fn clone(&self) -> Self {
        Self {
            request_id: self.request_id.clone(),
            request: clone_http_request_wrapper(&self.request).map(|_| self.request.body().clone()),
        }
    }
}
impl Action for QueryInspectorServerHttpRequestAction {}
impl SerializableAction for QueryInspectorServerHttpRequestAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([(
            "request_id",
            JsonValue::from(format!("{}", self.request_id.as_hyphenated())),
        )])
    }
}
impl Serialize for QueryInspectorServerHttpRequestAction {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        SerializedQueryInspectorServerHttpRequestAction::from(self).serialize(serializer)
    }
}
impl<'de> Deserialize<'de> for QueryInspectorServerHttpRequestAction {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        SerializedQueryInspectorServerHttpRequestAction::deserialize(deserializer).map(Into::into)
    }
}
#[derive(Clone, Serialize, Deserialize)]
struct SerializedQueryInspectorServerHttpRequestAction {
    request_id: Uuid,
    request: SerializedRequest,
}
impl<'a> From<&'a QueryInspectorServerHttpRequestAction>
    for SerializedQueryInspectorServerHttpRequestAction
{
    fn from(value: &'a QueryInspectorServerHttpRequestAction) -> Self {
        let QueryInspectorServerHttpRequestAction {
            request_id,
            request,
        } = value;
        Self {
            request_id: *request_id,
            request: request.into(),
        }
    }
}
impl From<SerializedQueryInspectorServerHttpRequestAction>
    for QueryInspectorServerHttpRequestAction
{
    fn from(value: SerializedQueryInspectorServerHttpRequestAction) -> Self {
        let SerializedQueryInspectorServerHttpRequestAction {
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
pub struct QueryInspectorServerHttpResponseAction {
    pub request_id: Uuid,
    pub response: Response<Bytes>,
}
impl Clone for QueryInspectorServerHttpResponseAction {
    fn clone(&self) -> Self {
        Self {
            request_id: self.request_id.clone(),
            response: clone_http_response_wrapper(&self.response)
                .map(|_| self.response.body().clone()),
        }
    }
}
impl Action for QueryInspectorServerHttpResponseAction {}
impl SerializableAction for QueryInspectorServerHttpResponseAction {
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
impl Serialize for QueryInspectorServerHttpResponseAction {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        SerializedQueryInspectorServerHttpResponseAction::from(self).serialize(serializer)
    }
}
impl<'de> Deserialize<'de> for QueryInspectorServerHttpResponseAction {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        SerializedQueryInspectorServerHttpResponseAction::deserialize(deserializer).map(Into::into)
    }
}
#[derive(Clone, Serialize, Deserialize)]
struct SerializedQueryInspectorServerHttpResponseAction {
    request_id: Uuid,
    response: SerializedResponse,
}
impl<'a> From<&'a QueryInspectorServerHttpResponseAction>
    for SerializedQueryInspectorServerHttpResponseAction
{
    fn from(value: &'a QueryInspectorServerHttpResponseAction) -> Self {
        let QueryInspectorServerHttpResponseAction {
            request_id,
            response,
        } = value;
        Self {
            request_id: *request_id,
            response: response.into(),
        }
    }
}
impl From<SerializedQueryInspectorServerHttpResponseAction>
    for QueryInspectorServerHttpResponseAction
{
    fn from(value: SerializedQueryInspectorServerHttpResponseAction) -> Self {
        let SerializedQueryInspectorServerHttpResponseAction {
            request_id,
            response,
        } = value;
        Self {
            request_id,
            response: response.into(),
        }
    }
}
