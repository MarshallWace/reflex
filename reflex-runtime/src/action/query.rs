// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{EvaluationResult, Expression};
use reflex_dispatcher::{Action, NamedAction, SerializableAction, SerializedAction};
use reflex_json::JsonValue;

#[derive(Clone, Debug)]
pub enum QueryAction<T: Expression> {
    Subscribe(QuerySubscribeAction<T>),
    Unsubscribe(QueryUnsubscribeAction<T>),
    Emit(QueryEmitAction<T>),
}
impl<T: Expression> Action for QueryAction<T> {}
impl<T: Expression> NamedAction for QueryAction<T> {
    fn name(&self) -> &'static str {
        match self {
            Self::Subscribe(action) => action.name(),
            Self::Unsubscribe(action) => action.name(),
            Self::Emit(action) => action.name(),
        }
    }
}
impl<T: Expression> SerializableAction for QueryAction<T> {
    fn serialize(&self) -> SerializedAction {
        match self {
            Self::Subscribe(action) => action.serialize(),
            Self::Unsubscribe(action) => action.serialize(),
            Self::Emit(action) => action.serialize(),
        }
    }
}

impl<T: Expression> From<QuerySubscribeAction<T>> for QueryAction<T> {
    fn from(value: QuerySubscribeAction<T>) -> Self {
        Self::Subscribe(value)
    }
}
impl<T: Expression> From<QueryAction<T>> for Option<QuerySubscribeAction<T>> {
    fn from(value: QueryAction<T>) -> Self {
        match value {
            QueryAction::Subscribe(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a QueryAction<T>> for Option<&'a QuerySubscribeAction<T>> {
    fn from(value: &'a QueryAction<T>) -> Self {
        match value {
            QueryAction::Subscribe(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<QueryUnsubscribeAction<T>> for QueryAction<T> {
    fn from(value: QueryUnsubscribeAction<T>) -> Self {
        Self::Unsubscribe(value)
    }
}
impl<T: Expression> From<QueryAction<T>> for Option<QueryUnsubscribeAction<T>> {
    fn from(value: QueryAction<T>) -> Self {
        match value {
            QueryAction::Unsubscribe(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a QueryAction<T>> for Option<&'a QueryUnsubscribeAction<T>> {
    fn from(value: &'a QueryAction<T>) -> Self {
        match value {
            QueryAction::Unsubscribe(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<QueryEmitAction<T>> for QueryAction<T> {
    fn from(value: QueryEmitAction<T>) -> Self {
        Self::Emit(value)
    }
}
impl<T: Expression> From<QueryAction<T>> for Option<QueryEmitAction<T>> {
    fn from(value: QueryAction<T>) -> Self {
        match value {
            QueryAction::Emit(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a QueryAction<T>> for Option<&'a QueryEmitAction<T>> {
    fn from(value: &'a QueryAction<T>) -> Self {
        match value {
            QueryAction::Emit(value) => Some(value),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct QuerySubscribeAction<T: Expression> {
    pub query: T,
    pub label: String,
}
impl<T: Expression> Action for QuerySubscribeAction<T> {}
impl<T: Expression> NamedAction for QuerySubscribeAction<T> {
    fn name(&self) -> &'static str {
        "QuerySubscribeAction"
    }
}
impl<T: Expression> SerializableAction for QuerySubscribeAction<T> {
    fn serialize(&self) -> SerializedAction {
        SerializedAction::from_iter([
            ("query_id", JsonValue::from(self.query.id())),
            ("label", JsonValue::from(self.label.clone())),
        ])
    }
}

#[derive(Clone, Debug)]
pub struct QueryUnsubscribeAction<T: Expression> {
    pub query: T,
    pub label: String,
}
impl<T: Expression> Action for QueryUnsubscribeAction<T> {}
impl<T: Expression> NamedAction for QueryUnsubscribeAction<T> {
    fn name(&self) -> &'static str {
        "QueryUnsubscribeAction"
    }
}
impl<T: Expression> SerializableAction for QueryUnsubscribeAction<T> {
    fn serialize(&self) -> SerializedAction {
        SerializedAction::from_iter([
            ("query_id", JsonValue::from(self.query.id())),
            ("label", JsonValue::from(self.label.clone())),
        ])
    }
}

#[derive(Clone, Debug)]
pub struct QueryEmitAction<T: Expression> {
    pub query: T,
    pub result: EvaluationResult<T>,
}
impl<T: Expression> Action for QueryEmitAction<T> {}
impl<T: Expression> NamedAction for QueryEmitAction<T> {
    fn name(&self) -> &'static str {
        "QueryEmitAction"
    }
}
impl<T: Expression> SerializableAction for QueryEmitAction<T> {
    fn serialize(&self) -> SerializedAction {
        SerializedAction::from_iter([
            ("query_id", JsonValue::from(self.query.id())),
            ("result_id", JsonValue::from(self.result.result().id())),
        ])
    }
}
