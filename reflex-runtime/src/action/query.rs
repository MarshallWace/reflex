// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{EvaluationResult, Expression};
use reflex_dispatcher::{Action, Named, SerializableAction, SerializedAction};
use reflex_json::JsonValue;
use reflex_macros::Named;
use serde::{Deserialize, Serialize};

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub enum QueryActions<T: Expression> {
    Subscribe(QuerySubscribeAction<T>),
    Unsubscribe(QueryUnsubscribeAction<T>),
    Emit(QueryEmitAction<T>),
}
impl<T: Expression> Named for QueryActions<T> {
    fn name(&self) -> &'static str {
        match self {
            Self::Subscribe(action) => action.name(),
            Self::Unsubscribe(action) => action.name(),
            Self::Emit(action) => action.name(),
        }
    }
}
impl<T: Expression> Action for QueryActions<T> {}
impl<T: Expression> SerializableAction for QueryActions<T> {
    fn to_json(&self) -> SerializedAction {
        match self {
            Self::Subscribe(action) => action.to_json(),
            Self::Unsubscribe(action) => action.to_json(),
            Self::Emit(action) => action.to_json(),
        }
    }
}

impl<T: Expression> From<QuerySubscribeAction<T>> for QueryActions<T> {
    fn from(value: QuerySubscribeAction<T>) -> Self {
        Self::Subscribe(value)
    }
}
impl<T: Expression> From<QueryActions<T>> for Option<QuerySubscribeAction<T>> {
    fn from(value: QueryActions<T>) -> Self {
        match value {
            QueryActions::Subscribe(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a QueryActions<T>> for Option<&'a QuerySubscribeAction<T>> {
    fn from(value: &'a QueryActions<T>) -> Self {
        match value {
            QueryActions::Subscribe(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<QueryUnsubscribeAction<T>> for QueryActions<T> {
    fn from(value: QueryUnsubscribeAction<T>) -> Self {
        Self::Unsubscribe(value)
    }
}
impl<T: Expression> From<QueryActions<T>> for Option<QueryUnsubscribeAction<T>> {
    fn from(value: QueryActions<T>) -> Self {
        match value {
            QueryActions::Unsubscribe(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a QueryActions<T>> for Option<&'a QueryUnsubscribeAction<T>> {
    fn from(value: &'a QueryActions<T>) -> Self {
        match value {
            QueryActions::Unsubscribe(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<QueryEmitAction<T>> for QueryActions<T> {
    fn from(value: QueryEmitAction<T>) -> Self {
        Self::Emit(value)
    }
}
impl<T: Expression> From<QueryActions<T>> for Option<QueryEmitAction<T>> {
    fn from(value: QueryActions<T>) -> Self {
        match value {
            QueryActions::Emit(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a QueryActions<T>> for Option<&'a QueryEmitAction<T>> {
    fn from(value: &'a QueryActions<T>) -> Self {
        match value {
            QueryActions::Emit(value) => Some(value),
            _ => None,
        }
    }
}

#[derive(Named, PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct QuerySubscribeAction<T: Expression> {
    pub query: T,
    pub label: String,
}
impl<T: Expression> Action for QuerySubscribeAction<T> {}
impl<T: Expression> SerializableAction for QuerySubscribeAction<T> {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([
            ("query_id", JsonValue::from(self.query.id())),
            ("label", JsonValue::from(self.label.clone())),
        ])
    }
}

#[derive(Named, PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct QueryUnsubscribeAction<T: Expression> {
    pub query: T,
    pub label: String,
}
impl<T: Expression> Action for QueryUnsubscribeAction<T> {}
impl<T: Expression> SerializableAction for QueryUnsubscribeAction<T> {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([
            ("query_id", JsonValue::from(self.query.id())),
            ("label", JsonValue::from(self.label.clone())),
        ])
    }
}

#[derive(Named, PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct QueryEmitAction<T: Expression> {
    pub query: T,
    pub result: EvaluationResult<T>,
}
impl<T: Expression> Action for QueryEmitAction<T> {}
impl<T: Expression> SerializableAction for QueryEmitAction<T> {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([
            ("query_id", JsonValue::from(self.query.id())),
            ("result_id", JsonValue::from(self.result.result().id())),
        ])
    }
}
