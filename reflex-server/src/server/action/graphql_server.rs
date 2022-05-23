// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{collections::HashMap, marker::PhantomData};

use reflex::core::{Expression, Uuid};
use reflex_dispatcher::{Action, NamedAction, SerializableAction, SerializedAction};
use reflex_graphql::GraphQlOperation;
use reflex_json::JsonValue;

#[derive(Clone, Debug)]
pub enum GraphQlServerAction<T: Expression> {
    Subscribe(GraphQlServerSubscribeAction<T>),
    Unsubscribe(GraphQlServerUnsubscribeAction<T>),
    Modify(GraphQlServerModifyAction<T>),
    ParseSuccess(GraphQlServerParseSuccessAction<T>),
    ParseError(GraphQlServerParseErrorAction<T>),
    Emit(GraphQlServerEmitAction<T>),
}
impl<T: Expression> Action for GraphQlServerAction<T> {}
impl<T: Expression> NamedAction for GraphQlServerAction<T> {
    fn name(&self) -> &'static str {
        match self {
            Self::Subscribe(action) => action.name(),
            Self::Unsubscribe(action) => action.name(),
            Self::Modify(action) => action.name(),
            Self::ParseSuccess(action) => action.name(),
            Self::ParseError(action) => action.name(),
            Self::Emit(action) => action.name(),
        }
    }
}
impl<T: Expression> SerializableAction for GraphQlServerAction<T> {
    fn serialize(&self) -> SerializedAction {
        match self {
            Self::Subscribe(action) => action.serialize(),
            Self::Unsubscribe(action) => action.serialize(),
            Self::Modify(action) => action.serialize(),
            Self::ParseSuccess(action) => action.serialize(),
            Self::ParseError(action) => action.serialize(),
            Self::Emit(action) => action.serialize(),
        }
    }
}

impl<T: Expression> From<GraphQlServerSubscribeAction<T>> for GraphQlServerAction<T> {
    fn from(value: GraphQlServerSubscribeAction<T>) -> Self {
        Self::Subscribe(value)
    }
}
impl<T: Expression> From<GraphQlServerAction<T>> for Option<GraphQlServerSubscribeAction<T>> {
    fn from(value: GraphQlServerAction<T>) -> Self {
        match value {
            GraphQlServerAction::Subscribe(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a GraphQlServerAction<T>>
    for Option<&'a GraphQlServerSubscribeAction<T>>
{
    fn from(value: &'a GraphQlServerAction<T>) -> Self {
        match value {
            GraphQlServerAction::Subscribe(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<GraphQlServerUnsubscribeAction<T>> for GraphQlServerAction<T> {
    fn from(value: GraphQlServerUnsubscribeAction<T>) -> Self {
        Self::Unsubscribe(value)
    }
}
impl<T: Expression> From<GraphQlServerAction<T>> for Option<GraphQlServerUnsubscribeAction<T>> {
    fn from(value: GraphQlServerAction<T>) -> Self {
        match value {
            GraphQlServerAction::Unsubscribe(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a GraphQlServerAction<T>>
    for Option<&'a GraphQlServerUnsubscribeAction<T>>
{
    fn from(value: &'a GraphQlServerAction<T>) -> Self {
        match value {
            GraphQlServerAction::Unsubscribe(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<GraphQlServerModifyAction<T>> for GraphQlServerAction<T> {
    fn from(value: GraphQlServerModifyAction<T>) -> Self {
        Self::Modify(value)
    }
}
impl<T: Expression> From<GraphQlServerAction<T>> for Option<GraphQlServerModifyAction<T>> {
    fn from(value: GraphQlServerAction<T>) -> Self {
        match value {
            GraphQlServerAction::Modify(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a GraphQlServerAction<T>>
    for Option<&'a GraphQlServerModifyAction<T>>
{
    fn from(value: &'a GraphQlServerAction<T>) -> Self {
        match value {
            GraphQlServerAction::Modify(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<GraphQlServerParseSuccessAction<T>> for GraphQlServerAction<T> {
    fn from(value: GraphQlServerParseSuccessAction<T>) -> Self {
        Self::ParseSuccess(value)
    }
}
impl<T: Expression> From<GraphQlServerAction<T>> for Option<GraphQlServerParseSuccessAction<T>> {
    fn from(value: GraphQlServerAction<T>) -> Self {
        match value {
            GraphQlServerAction::ParseSuccess(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a GraphQlServerAction<T>>
    for Option<&'a GraphQlServerParseSuccessAction<T>>
{
    fn from(value: &'a GraphQlServerAction<T>) -> Self {
        match value {
            GraphQlServerAction::ParseSuccess(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<GraphQlServerParseErrorAction<T>> for GraphQlServerAction<T> {
    fn from(value: GraphQlServerParseErrorAction<T>) -> Self {
        Self::ParseError(value)
    }
}
impl<T: Expression> From<GraphQlServerAction<T>> for Option<GraphQlServerParseErrorAction<T>> {
    fn from(value: GraphQlServerAction<T>) -> Self {
        match value {
            GraphQlServerAction::ParseError(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a GraphQlServerAction<T>>
    for Option<&'a GraphQlServerParseErrorAction<T>>
{
    fn from(value: &'a GraphQlServerAction<T>) -> Self {
        match value {
            GraphQlServerAction::ParseError(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<GraphQlServerEmitAction<T>> for GraphQlServerAction<T> {
    fn from(value: GraphQlServerEmitAction<T>) -> Self {
        Self::Emit(value)
    }
}
impl<T: Expression> From<GraphQlServerAction<T>> for Option<GraphQlServerEmitAction<T>> {
    fn from(value: GraphQlServerAction<T>) -> Self {
        match value {
            GraphQlServerAction::Emit(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a GraphQlServerAction<T>>
    for Option<&'a GraphQlServerEmitAction<T>>
{
    fn from(value: &'a GraphQlServerAction<T>) -> Self {
        match value {
            GraphQlServerAction::Emit(value) => Some(value),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct GraphQlServerSubscribeAction<T: Expression> {
    pub subscription_id: Uuid,
    pub operation: GraphQlOperation,
    pub _expression: PhantomData<T>,
}
impl<T: Expression> Action for GraphQlServerSubscribeAction<T> {}
impl<T: Expression> NamedAction for GraphQlServerSubscribeAction<T> {
    fn name(&self) -> &'static str {
        "GraphQlServerSubscribeAction"
    }
}
impl<T: Expression> SerializableAction for GraphQlServerSubscribeAction<T> {
    fn serialize(&self) -> SerializedAction {
        SerializedAction::from_iter([
            (
                "subscription_id",
                JsonValue::from(format!("{}", self.subscription_id.as_hyphenated())),
            ),
            (
                "query",
                JsonValue::from(format!("{}", self.operation.query())),
            ),
            (
                "operation_name",
                match self.operation.operation_name() {
                    None => JsonValue::Null,
                    Some(value) => JsonValue::from(value.clone()),
                },
            ),
            (
                "variables",
                JsonValue::from_iter(
                    self.operation
                        .variables()
                        .map(|(key, value)| (key.clone(), value.clone())),
                ),
            ),
            (
                "extensions",
                JsonValue::from_iter(
                    self.operation
                        .extensions()
                        .map(|(key, value)| (key.clone(), value.clone())),
                ),
            ),
        ])
    }
}

#[derive(Clone, Debug)]
pub struct GraphQlServerUnsubscribeAction<T: Expression> {
    pub subscription_id: Uuid,
    pub _expression: PhantomData<T>,
}
impl<T: Expression> Action for GraphQlServerUnsubscribeAction<T> {}
impl<T: Expression> NamedAction for GraphQlServerUnsubscribeAction<T> {
    fn name(&self) -> &'static str {
        "GraphQlServerUnsubscribeAction"
    }
}
impl<T: Expression> SerializableAction for GraphQlServerUnsubscribeAction<T> {
    fn serialize(&self) -> SerializedAction {
        SerializedAction::from_iter([(
            "subscription_id",
            JsonValue::from(format!("{}", self.subscription_id.as_hyphenated())),
        )])
    }
}

#[derive(Clone, Debug)]
pub struct GraphQlServerModifyAction<T: Expression> {
    pub subscription_id: Uuid,
    pub variables: HashMap<String, JsonValue>,
    pub _expression: PhantomData<T>,
}
impl<T: Expression> Action for GraphQlServerModifyAction<T> {}
impl<T: Expression> NamedAction for GraphQlServerModifyAction<T> {
    fn name(&self) -> &'static str {
        "GraphQlServerModifyAction"
    }
}
impl<T: Expression> SerializableAction for GraphQlServerModifyAction<T> {
    fn serialize(&self) -> SerializedAction {
        SerializedAction::from_iter([
            (
                "subscription_id",
                JsonValue::from(format!("{}", self.subscription_id.as_hyphenated())),
            ),
            (
                "variables",
                JsonValue::from_iter(
                    self.variables
                        .iter()
                        .map(|(key, value)| (key.clone(), value.clone())),
                ),
            ),
        ])
    }
}

#[derive(Clone, Debug)]
pub struct GraphQlServerParseSuccessAction<T: Expression> {
    pub subscription_id: Uuid,
    pub query: T,
}
impl<T: Expression> Action for GraphQlServerParseSuccessAction<T> {}
impl<T: Expression> NamedAction for GraphQlServerParseSuccessAction<T> {
    fn name(&self) -> &'static str {
        "GraphQlServerParseSuccessAction"
    }
}
impl<T: Expression> SerializableAction for GraphQlServerParseSuccessAction<T> {
    fn serialize(&self) -> SerializedAction {
        SerializedAction::from_iter([
            (
                "subscription_id",
                JsonValue::from(format!("{}", self.subscription_id.as_hyphenated())),
            ),
            ("query_id", JsonValue::from(self.query.id())),
        ])
    }
}

#[derive(Clone, Debug)]
pub struct GraphQlServerParseErrorAction<T: Expression> {
    pub subscription_id: Uuid,
    pub message: String,
    pub operation: GraphQlOperation,
    pub _expression: PhantomData<T>,
}
impl<T: Expression> Action for GraphQlServerParseErrorAction<T> {}
impl<T: Expression> NamedAction for GraphQlServerParseErrorAction<T> {
    fn name(&self) -> &'static str {
        "GraphQlServerParseErrorAction"
    }
}
impl<T: Expression> SerializableAction for GraphQlServerParseErrorAction<T> {
    fn serialize(&self) -> SerializedAction {
        SerializedAction::from_iter([
            ("message", JsonValue::from(self.message.clone())),
            (
                "subscription_id",
                JsonValue::from(format!("{}", self.subscription_id.as_hyphenated())),
            ),
            (
                "query",
                JsonValue::from(format!("{}", self.operation.query())),
            ),
            (
                "operation_name",
                match self.operation.operation_name() {
                    None => JsonValue::Null,
                    Some(value) => JsonValue::from(value.clone()),
                },
            ),
            (
                "variables",
                JsonValue::from_iter(
                    self.operation
                        .variables()
                        .map(|(key, value)| (key.clone(), value.clone())),
                ),
            ),
            (
                "extensions",
                JsonValue::from_iter(
                    self.operation
                        .extensions()
                        .map(|(key, value)| (key.clone(), value.clone())),
                ),
            ),
        ])
    }
}

#[derive(Clone, Debug)]
pub struct GraphQlServerEmitAction<T: Expression> {
    pub subscription_id: Uuid,
    pub result: T,
}
impl<T: Expression> Action for GraphQlServerEmitAction<T> {}
impl<T: Expression> NamedAction for GraphQlServerEmitAction<T> {
    fn name(&self) -> &'static str {
        "GraphQlServerEmitAction"
    }
}
impl<T: Expression> SerializableAction for GraphQlServerEmitAction<T> {
    fn serialize(&self) -> SerializedAction {
        SerializedAction::from_iter([
            (
                "subscription_id",
                JsonValue::from(format!("{}", self.subscription_id.as_hyphenated())),
            ),
            ("result_id", JsonValue::from(self.result.id())),
        ])
    }
}
