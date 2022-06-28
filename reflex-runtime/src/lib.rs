// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::iter::once;

use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator, SignalType, StringValue},
    lang::{
        term::{FloatValue, IntValue},
        ValueTerm,
    },
};
use serde::{Deserialize, Serialize};

pub mod action;
pub mod actor;
pub mod worker;

pub trait AsyncExpression: Expression + Send + 'static {}
impl<T> AsyncExpression for T
where
    T: Expression + Send + 'static,
    T::String: StringValue + Send,
{
}
pub trait AsyncExpressionFactory<T: AsyncExpression>:
    ExpressionFactory<T> + Send + Clone + 'static
{
}
impl<T, E: AsyncExpression> AsyncExpressionFactory<E> for T
where
    T: ExpressionFactory<E> + Send + Clone + 'static,
    E::String: StringValue + Send,
{
}
pub trait AsyncHeapAllocator<T: AsyncExpression>:
    HeapAllocator<T> + Send + Clone + 'static
{
}
impl<T, E: AsyncExpression> AsyncHeapAllocator<E> for T
where
    T: HeapAllocator<E> + Send + Clone + 'static,
    E::String: StringValue + Send,
{
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub enum StatePatch {
    Increment,
    Decrement,
}
impl StatePatch {
    pub fn apply<T: Expression>(
        &self,
        existing: Option<&T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> T {
        let existing_value = existing
            .map(|existing| {
                parse_numeric_value(existing, factory).map_err(|err| {
                    err.unwrap_or_else(|| {
                        create_error_expression(
                            format!("Unable to increment non-numeric value: {}", existing),
                            factory,
                            allocator,
                        )
                    })
                })
            })
            .transpose();
        match existing_value {
            Err(err) => err,
            Ok(existing_value) => match self {
                Self::Increment => apply_atomic_integer_operation(existing_value, 1, factory),
                Self::Decrement => apply_atomic_integer_operation(existing_value, -1, factory),
            },
        }
    }
}
impl std::fmt::Display for StatePatch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(self, f)
    }
}

fn apply_atomic_integer_operation<T: Expression>(
    existing_value: Option<NumericValue>,
    delta: i32,
    factory: &impl ExpressionFactory<T>,
) -> T {
    match existing_value {
        None => factory.create_value_term(ValueTerm::Int(delta)),
        Some(NumericValue::Int(value)) => factory.create_value_term(ValueTerm::Int(value + delta)),
        Some(NumericValue::Float(value)) => {
            factory.create_value_term(ValueTerm::Float(value + (delta as f64)))
        }
    }
}

enum NumericValue {
    Int(IntValue),
    Float(FloatValue),
}
fn parse_numeric_value<'a, T: Expression>(
    value: &'a T,
    factory: &'a impl ExpressionFactory<T>,
) -> Result<NumericValue, Option<T>> {
    if let Some(value) = factory.match_value_term(value) {
        if let Some(value) = value.match_int() {
            Ok(NumericValue::Int(value))
        } else if let Some(value) = value.match_float() {
            Ok(NumericValue::Float(value))
        } else {
            Err(None)
        }
    } else if let Some(_) = factory.match_signal_term(value) {
        Err(Some(value.clone()))
    } else {
        Err(None)
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum StateUpdate<T: Expression> {
    Value(T),
    Patch(StatePatch),
}
impl<T: Expression> StateUpdate<T> {
    pub fn value(value: T) -> Self {
        Self::Value(value)
    }
    pub fn patch(operation: StatePatch) -> Self {
        Self::Patch(operation)
    }
}
impl<T: Expression> std::fmt::Display for StateUpdate<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Value(value) => write!(f, "{}", value),
            Self::Patch(operation) => write!(f, "<{}>", operation),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, Serialize, Deserialize)]
pub enum QueryEvaluationMode {
    /// Evaluate the expression as a query function to be applied to the graph root
    Query,
    /// Evaluate the expression as a standalone expression unrelated to the graph root
    Standalone,
}
impl QueryEvaluationMode {
    pub(crate) fn serialize<T: Expression>(&self, factory: &impl ExpressionFactory<T>) -> T {
        factory.create_value_term(ValueTerm::Boolean(match self {
            Self::Query => false,
            Self::Standalone => true,
        }))
    }
    pub(crate) fn deserialize<T: Expression>(
        value: &T,
        factory: &impl ExpressionFactory<T>,
    ) -> Option<Self> {
        factory
            .match_value_term(value)
            .and_then(|value| value.match_boolean())
            .map(|value| match value {
                false => Self::Query,
                true => Self::Standalone,
            })
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, Serialize, Deserialize)]
pub enum QueryInvalidationStrategy {
    /// Group sequential async state updates into combined batches and process simultaneously (better performance)
    CombineUpdateBatches,
    /// Evaluate a new result for every individual state update batch (useful when watching incremental queries)
    Exact,
}
impl Default for QueryInvalidationStrategy {
    fn default() -> Self {
        Self::CombineUpdateBatches
    }
}
impl QueryInvalidationStrategy {
    pub(crate) fn serialize<T: Expression>(&self, factory: &impl ExpressionFactory<T>) -> T {
        factory.create_value_term(ValueTerm::Boolean(match self {
            Self::CombineUpdateBatches => false,
            Self::Exact => true,
        }))
    }
    pub(crate) fn deserialize<T: Expression>(
        value: &T,
        factory: &impl ExpressionFactory<T>,
    ) -> Option<Self> {
        factory
            .match_value_term(value)
            .and_then(|value| value.match_boolean())
            .map(|value| match value {
                false => Self::CombineUpdateBatches,
                true => Self::Exact,
            })
    }
}

fn create_error_expression<T: Expression>(
    message: String,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_signal_term(allocator.create_signal_list(once(allocator.create_signal(
        SignalType::Error,
        allocator.create_unit_list(factory.create_value_term(ValueTerm::String(message.into()))),
    ))))
}
