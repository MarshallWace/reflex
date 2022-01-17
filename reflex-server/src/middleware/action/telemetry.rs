// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex_dispatcher::{Action, NamedAction, SerializableAction, SerializedAction};
use reflex_json::{JsonMap, JsonValue};

use crate::utils::traceparent::Traceparent;

#[derive(Clone, Debug)]
pub struct TelemetryTransaction {
    pub transaction_id: Traceparent,
    pub parent_ids: Vec<Traceparent>,
    pub name: String,
    pub attributes: Vec<(String, String)>,
}

#[derive(Clone, Debug)]
pub enum TelemetryMiddlewareAction {
    TransactionStart(TelemetryMiddlewareTransactionStartAction),
    TransactionEnd(TelemetryMiddlewareTransactionEndAction),
}
impl Action for TelemetryMiddlewareAction {}
impl NamedAction for TelemetryMiddlewareAction {
    fn name(&self) -> &'static str {
        match self {
            Self::TransactionStart(action) => action.name(),
            Self::TransactionEnd(action) => action.name(),
        }
    }
}
impl SerializableAction for TelemetryMiddlewareAction {
    fn serialize(&self) -> SerializedAction {
        match self {
            Self::TransactionStart(action) => action.serialize(),
            Self::TransactionEnd(action) => action.serialize(),
        }
    }
}

impl From<TelemetryMiddlewareTransactionStartAction> for TelemetryMiddlewareAction {
    fn from(value: TelemetryMiddlewareTransactionStartAction) -> Self {
        Self::TransactionStart(value)
    }
}
impl From<TelemetryMiddlewareAction> for Option<TelemetryMiddlewareTransactionStartAction> {
    fn from(value: TelemetryMiddlewareAction) -> Self {
        match value {
            TelemetryMiddlewareAction::TransactionStart(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a TelemetryMiddlewareAction>
    for Option<&'a TelemetryMiddlewareTransactionStartAction>
{
    fn from(value: &'a TelemetryMiddlewareAction) -> Self {
        match value {
            TelemetryMiddlewareAction::TransactionStart(value) => Some(value),
            _ => None,
        }
    }
}

impl From<TelemetryMiddlewareTransactionEndAction> for TelemetryMiddlewareAction {
    fn from(value: TelemetryMiddlewareTransactionEndAction) -> Self {
        Self::TransactionEnd(value)
    }
}
impl From<TelemetryMiddlewareAction> for Option<TelemetryMiddlewareTransactionEndAction> {
    fn from(value: TelemetryMiddlewareAction) -> Self {
        match value {
            TelemetryMiddlewareAction::TransactionEnd(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a TelemetryMiddlewareAction>
    for Option<&'a TelemetryMiddlewareTransactionEndAction>
{
    fn from(value: &'a TelemetryMiddlewareAction) -> Self {
        match value {
            TelemetryMiddlewareAction::TransactionEnd(value) => Some(value),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct TelemetryMiddlewareTransactionStartAction {
    pub transactions: Vec<TelemetryTransaction>,
}
impl Action for TelemetryMiddlewareTransactionStartAction {}
impl NamedAction for TelemetryMiddlewareTransactionStartAction {
    fn name(&self) -> &'static str {
        "TelemetryMiddlewareTransactionStartAction"
    }
}
impl SerializableAction for TelemetryMiddlewareTransactionStartAction {
    fn serialize(&self) -> SerializedAction {
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

#[derive(Clone, Debug)]
pub struct TelemetryMiddlewareTransactionEndAction {
    pub transaction_ids: Vec<Traceparent>,
}
impl Action for TelemetryMiddlewareTransactionEndAction {}
impl NamedAction for TelemetryMiddlewareTransactionEndAction {
    fn name(&self) -> &'static str {
        "TelemetryMiddlewareTransactionEndAction"
    }
}
impl SerializableAction for TelemetryMiddlewareTransactionEndAction {
    fn serialize(&self) -> SerializedAction {
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
