// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::marker::PhantomData;

use reflex::core::{Expression, Uuid};
use reflex_dispatcher::{Action, NamedAction, SerializableAction, SerializedAction};
use reflex_graphql::{GraphQlOperation, GraphQlVariables};
use reflex_json::JsonValue;
use serde::{Deserialize, Serialize};

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub enum GraphQlServerActions<T: Expression> {
    Subscribe(GraphQlServerSubscribeAction<T>),
    Unsubscribe(GraphQlServerUnsubscribeAction<T>),
    Modify(GraphQlServerModifyAction<T>),
    ParseSuccess(GraphQlServerParseSuccessAction<T>),
    ParseError(GraphQlServerParseErrorAction<T>),
    Emit(GraphQlServerEmitAction<T>),
}
impl<T: Expression> Action for GraphQlServerActions<T> {}
impl<T: Expression> NamedAction for GraphQlServerActions<T> {
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
impl<T: Expression> SerializableAction for GraphQlServerActions<T> {
    fn to_json(&self) -> SerializedAction {
        match self {
            Self::Subscribe(action) => action.to_json(),
            Self::Unsubscribe(action) => action.to_json(),
            Self::Modify(action) => action.to_json(),
            Self::ParseSuccess(action) => action.to_json(),
            Self::ParseError(action) => action.to_json(),
            Self::Emit(action) => action.to_json(),
        }
    }
}

impl<T: Expression> From<GraphQlServerSubscribeAction<T>> for GraphQlServerActions<T> {
    fn from(value: GraphQlServerSubscribeAction<T>) -> Self {
        Self::Subscribe(value)
    }
}
impl<T: Expression> From<GraphQlServerActions<T>> for Option<GraphQlServerSubscribeAction<T>> {
    fn from(value: GraphQlServerActions<T>) -> Self {
        match value {
            GraphQlServerActions::Subscribe(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a GraphQlServerActions<T>>
    for Option<&'a GraphQlServerSubscribeAction<T>>
{
    fn from(value: &'a GraphQlServerActions<T>) -> Self {
        match value {
            GraphQlServerActions::Subscribe(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<GraphQlServerUnsubscribeAction<T>> for GraphQlServerActions<T> {
    fn from(value: GraphQlServerUnsubscribeAction<T>) -> Self {
        Self::Unsubscribe(value)
    }
}
impl<T: Expression> From<GraphQlServerActions<T>> for Option<GraphQlServerUnsubscribeAction<T>> {
    fn from(value: GraphQlServerActions<T>) -> Self {
        match value {
            GraphQlServerActions::Unsubscribe(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a GraphQlServerActions<T>>
    for Option<&'a GraphQlServerUnsubscribeAction<T>>
{
    fn from(value: &'a GraphQlServerActions<T>) -> Self {
        match value {
            GraphQlServerActions::Unsubscribe(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<GraphQlServerModifyAction<T>> for GraphQlServerActions<T> {
    fn from(value: GraphQlServerModifyAction<T>) -> Self {
        Self::Modify(value)
    }
}
impl<T: Expression> From<GraphQlServerActions<T>> for Option<GraphQlServerModifyAction<T>> {
    fn from(value: GraphQlServerActions<T>) -> Self {
        match value {
            GraphQlServerActions::Modify(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a GraphQlServerActions<T>>
    for Option<&'a GraphQlServerModifyAction<T>>
{
    fn from(value: &'a GraphQlServerActions<T>) -> Self {
        match value {
            GraphQlServerActions::Modify(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<GraphQlServerParseSuccessAction<T>> for GraphQlServerActions<T> {
    fn from(value: GraphQlServerParseSuccessAction<T>) -> Self {
        Self::ParseSuccess(value)
    }
}
impl<T: Expression> From<GraphQlServerActions<T>> for Option<GraphQlServerParseSuccessAction<T>> {
    fn from(value: GraphQlServerActions<T>) -> Self {
        match value {
            GraphQlServerActions::ParseSuccess(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a GraphQlServerActions<T>>
    for Option<&'a GraphQlServerParseSuccessAction<T>>
{
    fn from(value: &'a GraphQlServerActions<T>) -> Self {
        match value {
            GraphQlServerActions::ParseSuccess(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<GraphQlServerParseErrorAction<T>> for GraphQlServerActions<T> {
    fn from(value: GraphQlServerParseErrorAction<T>) -> Self {
        Self::ParseError(value)
    }
}
impl<T: Expression> From<GraphQlServerActions<T>> for Option<GraphQlServerParseErrorAction<T>> {
    fn from(value: GraphQlServerActions<T>) -> Self {
        match value {
            GraphQlServerActions::ParseError(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a GraphQlServerActions<T>>
    for Option<&'a GraphQlServerParseErrorAction<T>>
{
    fn from(value: &'a GraphQlServerActions<T>) -> Self {
        match value {
            GraphQlServerActions::ParseError(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<GraphQlServerEmitAction<T>> for GraphQlServerActions<T> {
    fn from(value: GraphQlServerEmitAction<T>) -> Self {
        Self::Emit(value)
    }
}
impl<T: Expression> From<GraphQlServerActions<T>> for Option<GraphQlServerEmitAction<T>> {
    fn from(value: GraphQlServerActions<T>) -> Self {
        match value {
            GraphQlServerActions::Emit(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a GraphQlServerActions<T>>
    for Option<&'a GraphQlServerEmitAction<T>>
{
    fn from(value: &'a GraphQlServerActions<T>) -> Self {
        match value {
            GraphQlServerActions::Emit(value) => Some(value),
            _ => None,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
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
    fn to_json(&self) -> SerializedAction {
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
                JsonValue::Object(self.operation.variables().clone()),
            ),
            (
                "extensions",
                JsonValue::Object(self.operation.extensions().clone()),
            ),
        ])
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
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
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([(
            "subscription_id",
            JsonValue::from(format!("{}", self.subscription_id.as_hyphenated())),
        )])
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct GraphQlServerModifyAction<T: Expression> {
    pub subscription_id: Uuid,
    pub variables: GraphQlVariables,
    pub _expression: PhantomData<T>,
}
impl<T: Expression> Action for GraphQlServerModifyAction<T> {}
impl<T: Expression> NamedAction for GraphQlServerModifyAction<T> {
    fn name(&self) -> &'static str {
        "GraphQlServerModifyAction"
    }
}
impl<T: Expression> SerializableAction for GraphQlServerModifyAction<T> {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([
            (
                "subscription_id",
                JsonValue::from(format!("{}", self.subscription_id.as_hyphenated())),
            ),
            ("variables", JsonValue::Object(self.variables.clone())),
        ])
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
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
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([
            (
                "subscription_id",
                JsonValue::from(format!("{}", self.subscription_id.as_hyphenated())),
            ),
            ("query_id", JsonValue::from(self.query.id())),
        ])
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
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
    fn to_json(&self) -> SerializedAction {
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
                JsonValue::Object(self.operation.variables().clone()),
            ),
            (
                "extensions",
                JsonValue::Object(self.operation.extensions().clone()),
            ),
        ])
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
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
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([
            (
                "subscription_id",
                JsonValue::from(format!("{}", self.subscription_id.as_hyphenated())),
            ),
            ("result_id", JsonValue::from(self.result.id())),
        ])
    }
}
