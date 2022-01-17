// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use bytes::Bytes;
use http::{Request, Response};
use reflex::core::Uuid;
use reflex_dispatcher::{Action, NamedAction, SerializableAction, SerializedAction};
use reflex_json::{JsonMap, JsonValue};

use crate::server::utils::{clone_request_wrapper, clone_response_wrapper};

#[derive(Clone, Debug)]
pub enum HttpServerAction {
    Request(HttpServerRequestAction),
    Response(HttpServerResponseAction),
}
impl Action for HttpServerAction {}
impl NamedAction for HttpServerAction {
    fn name(&self) -> &'static str {
        match self {
            Self::Request(action) => action.name(),
            Self::Response(action) => action.name(),
        }
    }
}
impl SerializableAction for HttpServerAction {
    fn serialize(&self) -> SerializedAction {
        match self {
            Self::Request(action) => action.serialize(),
            Self::Response(action) => action.serialize(),
        }
    }
}

impl From<HttpServerRequestAction> for HttpServerAction {
    fn from(value: HttpServerRequestAction) -> Self {
        Self::Request(value)
    }
}
impl From<HttpServerAction> for Option<HttpServerRequestAction> {
    fn from(value: HttpServerAction) -> Self {
        match value {
            HttpServerAction::Request(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a HttpServerAction> for Option<&'a HttpServerRequestAction> {
    fn from(value: &'a HttpServerAction) -> Self {
        match value {
            HttpServerAction::Request(value) => Some(value),
            _ => None,
        }
    }
}

impl From<HttpServerResponseAction> for HttpServerAction {
    fn from(value: HttpServerResponseAction) -> Self {
        Self::Response(value)
    }
}
impl From<HttpServerAction> for Option<HttpServerResponseAction> {
    fn from(value: HttpServerAction) -> Self {
        match value {
            HttpServerAction::Response(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a HttpServerAction> for Option<&'a HttpServerResponseAction> {
    fn from(value: &'a HttpServerAction) -> Self {
        match value {
            HttpServerAction::Response(value) => Some(value),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct HttpServerRequestAction {
    pub request_id: Uuid,
    pub request: Request<Bytes>,
}
impl Clone for HttpServerRequestAction {
    fn clone(&self) -> Self {
        Self {
            request_id: self.request_id.clone(),
            request: clone_request_wrapper(&self.request).map(|_| self.request.body().clone()),
        }
    }
}
impl Action for HttpServerRequestAction {}
impl NamedAction for HttpServerRequestAction {
    fn name(&self) -> &'static str {
        "HttpGraphQlServerRequestAction"
    }
}
impl SerializableAction for HttpServerRequestAction {
    fn serialize(&self) -> SerializedAction {
        SerializedAction::from_iter([(
            "request_id",
            JsonValue::from(format!("{}", self.request_id.as_hyphenated())),
        )])
    }
}

#[derive(Debug)]
pub struct HttpServerResponseAction {
    pub request_id: Uuid,
    pub response: Response<Bytes>,
}
impl Clone for HttpServerResponseAction {
    fn clone(&self) -> Self {
        Self {
            request_id: self.request_id.clone(),
            response: clone_response_wrapper(&self.response).map(|_| self.response.body().clone()),
        }
    }
}
impl Action for HttpServerResponseAction {}
impl NamedAction for HttpServerResponseAction {
    fn name(&self) -> &'static str {
        "HttpGraphQlServerResponseAction"
    }
}
impl SerializableAction for HttpServerResponseAction {
    fn serialize(&self) -> SerializedAction {
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
