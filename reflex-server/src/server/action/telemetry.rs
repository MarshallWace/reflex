// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex_dispatcher::{Action, Named, SerializableAction, SerializedAction};
use reflex_json::{JsonMap, JsonValue};
use reflex_macros::Named;
use serde::{Deserialize, Serialize};

use crate::utils::traceparent::Traceparent;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct TelemetryTransaction {
    pub transaction_id: Traceparent,
    pub parent_ids: Vec<Traceparent>,
    pub name: String,
    pub attributes: Vec<(String, String)>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum TelemetryMiddlewareActions {
    TransactionStart(TelemetryMiddlewareTransactionStartAction),
    TransactionEnd(TelemetryMiddlewareTransactionEndAction),
}
impl Named for TelemetryMiddlewareActions {
    fn name(&self) -> &'static str {
        match self {
            Self::TransactionStart(action) => action.name(),
            Self::TransactionEnd(action) => action.name(),
        }
    }
}
impl Action for TelemetryMiddlewareActions {}
impl SerializableAction for TelemetryMiddlewareActions {
    fn to_json(&self) -> SerializedAction {
        match self {
            Self::TransactionStart(action) => action.to_json(),
            Self::TransactionEnd(action) => action.to_json(),
        }
    }
}

impl From<TelemetryMiddlewareTransactionStartAction> for TelemetryMiddlewareActions {
    fn from(value: TelemetryMiddlewareTransactionStartAction) -> Self {
        Self::TransactionStart(value)
    }
}
impl From<TelemetryMiddlewareActions> for Option<TelemetryMiddlewareTransactionStartAction> {
    fn from(value: TelemetryMiddlewareActions) -> Self {
        match value {
            TelemetryMiddlewareActions::TransactionStart(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a TelemetryMiddlewareActions>
    for Option<&'a TelemetryMiddlewareTransactionStartAction>
{
    fn from(value: &'a TelemetryMiddlewareActions) -> Self {
        match value {
            TelemetryMiddlewareActions::TransactionStart(value) => Some(value),
            _ => None,
        }
    }
}

impl From<TelemetryMiddlewareTransactionEndAction> for TelemetryMiddlewareActions {
    fn from(value: TelemetryMiddlewareTransactionEndAction) -> Self {
        Self::TransactionEnd(value)
    }
}
impl From<TelemetryMiddlewareActions> for Option<TelemetryMiddlewareTransactionEndAction> {
    fn from(value: TelemetryMiddlewareActions) -> Self {
        match value {
            TelemetryMiddlewareActions::TransactionEnd(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a TelemetryMiddlewareActions>
    for Option<&'a TelemetryMiddlewareTransactionEndAction>
{
    fn from(value: &'a TelemetryMiddlewareActions) -> Self {
        match value {
            TelemetryMiddlewareActions::TransactionEnd(value) => Some(value),
            _ => None,
        }
    }
}

#[derive(Named, Clone, Debug, Serialize, Deserialize)]
pub struct TelemetryMiddlewareTransactionStartAction {
    pub transactions: Vec<TelemetryTransaction>,
}
impl Action for TelemetryMiddlewareTransactionStartAction {}
impl SerializableAction for TelemetryMiddlewareTransactionStartAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([(
            "transactions",
            JsonValue::Array(
                self.transactions
                    .iter()
                    .map(|transaction| {
                        JsonValue::Object(JsonMap::from_iter([
                            (
                                "transaction_id".into(),
                                JsonValue::from(format!("{}", transaction.transaction_id)),
                            ),
                            (
                                "parent_ids".into(),
                                JsonValue::Array(
                                    transaction
                                        .parent_ids
                                        .iter()
                                        .map(|parent_id| JsonValue::from(format!("{}", parent_id)))
                                        .collect(),
                                ),
                            ),
                            ("name".into(), JsonValue::from(transaction.name.clone())),
                            (
                                "attributes".into(),
                                JsonValue::Object(JsonMap::from_iter(
                                    transaction.attributes.iter().map(|(key, value)| {
                                        (String::from(key), JsonValue::from(String::from(value)))
                                    }),
                                )),
                            ),
                        ]))
                    })
                    .collect(),
            ),
        )])
    }
}

#[derive(Named, Clone, Debug, Serialize, Deserialize)]
pub struct TelemetryMiddlewareTransactionEndAction {
    pub transaction_ids: Vec<Traceparent>,
}
impl Action for TelemetryMiddlewareTransactionEndAction {}
impl SerializableAction for TelemetryMiddlewareTransactionEndAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([(
            "transaction_ids",
            JsonValue::Array(
                self.transaction_ids
                    .iter()
                    .map(|transaction_id| JsonValue::from(format!("{}", transaction_id)))
                    .collect(),
            ),
        )])
    }
}
