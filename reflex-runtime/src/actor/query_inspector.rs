// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{hash_map::Entry, HashMap},
    marker::PhantomData,
};

use reflex::core::{
    ConditionListType, ConditionType, DependencyList, EvaluationResult, Expression,
    ExpressionFactory, ExpressionListType, RefType, SignalTermType, SignalType, StateToken,
};
use reflex_dispatcher::{
    Action, Actor, ActorTransition, HandlerContext, InboundAction, MessageData, StateTransition,
};
use reflex_json::{json, JsonValue};

use crate::{
    action::{
        effect::{EffectEmitAction, EffectSubscribeAction, EffectUnsubscribeAction},
        evaluate::{EvaluateResultAction, EvaluateStartAction, EvaluateStopAction},
    },
    QueryEvaluationMode, QueryInvalidationStrategy,
};

pub trait QueryInspectorAction<T: Expression>:
    Action
    + InboundAction<EvaluateStartAction<T>>
    + InboundAction<EvaluateStopAction>
    + InboundAction<EvaluateResultAction<T>>
    + InboundAction<EffectSubscribeAction<T>>
    + InboundAction<EffectEmitAction<T>>
    + InboundAction<EffectUnsubscribeAction<T>>
{
}
impl<T: Expression, TAction> QueryInspectorAction<T> for TAction where
    Self: Action
        + InboundAction<EvaluateStartAction<T>>
        + InboundAction<EvaluateStopAction>
        + InboundAction<EvaluateResultAction<T>>
        + InboundAction<EffectSubscribeAction<T>>
        + InboundAction<EffectEmitAction<T>>
        + InboundAction<EffectUnsubscribeAction<T>>
{
}

pub struct QueryInspector<T: Expression> {
    _expression: PhantomData<T>,
}
impl<T: Expression> Default for QueryInspector<T> {
    fn default() -> Self {
        Self {
            _expression: Default::default(),
        }
    }
}

pub struct QueryInspectorState<T: Expression> {
    active_workers: HashMap<StateToken, QueryInspectorWorkerState<T>>,
    active_effects: HashMap<StateToken, QueryInspectorEffectState<T>>,
}
impl<T: Expression> Default for QueryInspectorState<T> {
    fn default() -> Self {
        Self {
            active_workers: Default::default(),
            active_effects: Default::default(),
        }
    }
}
impl<T: Expression> QueryInspectorState<T> {
    pub fn to_json(&self, factory: &impl ExpressionFactory<T>) -> JsonValue {
        let queries = self.active_workers.iter().map(|(cache_id, worker_state)| {
            worker_state.to_json(*cache_id, &self.active_effects, factory)
        });
        let effects = self
            .active_effects
            .values()
            .filter(|effect_state| is_unresolved_effect_state(effect_state, factory))
            .map(|effect_state| {
                json!({
                    "effect": serialize_effect(&effect_state.effect),
                    "value": serialize_effect_result(effect_state.value.as_ref(), factory),
                })
            });
        json!({
            "numEffects": &self.active_effects.len(),
            "queries": queries.collect::<Vec<_>>(),
            "effects": effects.collect::<Vec<_>>()
        })
    }
}

fn serialize_query_result<T: Expression>(
    result: Option<&EvaluationResult<T>>,
    active_effects: &HashMap<StateToken, QueryInspectorEffectState<T>>,
    factory: &impl ExpressionFactory<T>,
) -> JsonValue {
    match result {
        None => JsonValue::Null,
        Some(result) => json!({
            "value": serialize_value(result.result(), factory),
            "dependencies": JsonValue::Array(get_unresolved_dependencies(result.dependencies(), active_effects, factory).map(JsonValue::from).collect()),
        }),
    }
}

fn get_unresolved_dependencies<'a, T: Expression>(
    dependencies: &'a DependencyList,
    active_effects: &'a HashMap<StateToken, QueryInspectorEffectState<T>>,
    factory: &'a impl ExpressionFactory<T>,
) -> impl Iterator<Item = StateToken> + 'a {
    dependencies
        .iter()
        .filter(|state_token| is_unresolved_dependency(state_token, active_effects, factory))
}

fn is_unresolved_dependency<T: Expression>(
    state_token: &StateToken,
    active_effects: &HashMap<StateToken, QueryInspectorEffectState<T>>,
    factory: &impl ExpressionFactory<T>,
) -> bool {
    match active_effects.get(state_token) {
        None => true,
        Some(effect_state) => is_unresolved_effect_state(effect_state, factory),
    }
}

fn is_unresolved_effect_state<T: Expression>(
    effect_state: &QueryInspectorEffectState<T>,
    factory: &impl ExpressionFactory<T>,
) -> bool {
    match effect_state.value.as_ref() {
        None => true,
        Some(value) => factory
            .match_signal_term(value)
            .map(|term| {
                term.signals()
                    .as_deref()
                    .iter()
                    .map(|item| item.as_deref())
                    .any(is_unresolved_effect)
            })
            .unwrap_or(false),
    }
}

fn is_unresolved_effect<T: Expression>(effect: &impl ConditionType<T>) -> bool {
    match effect.signal_type() {
        SignalType::Error => false,
        SignalType::Pending | SignalType::Custom(_) => true,
    }
}

fn serialize_effect_result<T: Expression>(
    result: Option<&T>,
    factory: &impl ExpressionFactory<T>,
) -> JsonValue {
    match result {
        None => JsonValue::Null,
        Some(value) => serialize_value(value, factory),
    }
}

fn serialize_value<T: Expression>(value: &T, factory: &impl ExpressionFactory<T>) -> JsonValue {
    if let Some(value) = factory.match_signal_term(value) {
        JsonValue::Array(
            value
                .signals()
                .as_deref()
                .iter()
                .map(|item| item.as_deref())
                .map(serialize_effect)
                .collect(),
        )
    } else {
        JsonValue::Number(value.id().into())
    }
}

pub struct QueryInspectorEffectState<T: Expression> {
    effect: T::Signal<T>,
    value: Option<T>,
}

struct QueryInspectorWorkerState<T: Expression> {
    label: String,
    #[allow(dead_code)]
    query: T,
    #[allow(dead_code)]
    evaluation_mode: QueryEvaluationMode,
    #[allow(dead_code)]
    invalidation_strategy: QueryInvalidationStrategy,
    latest_result: Option<EvaluationResult<T>>,
}
impl<T: Expression> QueryInspectorWorkerState<T> {
    fn to_json(
        &self,
        cache_id: StateToken,
        active_effects: &HashMap<StateToken, QueryInspectorEffectState<T>>,
        factory: &impl ExpressionFactory<T>,
    ) -> JsonValue {
        json!({
            "id": cache_id,
            "label": &self.label,
            "result": serialize_query_result(self.latest_result.as_ref(), active_effects, factory),
        })
    }
}

fn serialize_effect<T: Expression>(effect: &impl ConditionType<T>) -> JsonValue {
    json!({
        "id": JsonValue::Number(effect.id().into()),
        "type": match effect.signal_type() {
            SignalType::Custom(signal_type) => JsonValue::String(String::from(signal_type)),
            SignalType::Error => JsonValue::String(String::from("error")),
            SignalType::Pending => JsonValue::String(String::from("pending")),
        },
        "args": serialize_json_list(effect.args().as_deref())
    })
}

fn serialize_json_list<T: Expression>(items: &impl ExpressionListType<T>) -> JsonValue {
    JsonValue::Array(
        items
            .iter()
            .map(|item| item.as_deref())
            .map(|value| match reflex_json::sanitize(value) {
                Ok(value) => value,
                Err(_) => json!({}),
            })
            .collect(),
    )
}

impl<T: Expression, TAction> Actor<TAction> for QueryInspector<T>
where
    TAction: QueryInspectorAction<T>,
{
    type State = QueryInspectorState<T>;
    fn init(&self) -> Self::State {
        Default::default()
    }
    fn handle(
        &self,
        state: Self::State,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> ActorTransition<Self::State, TAction> {
        let mut state = state;
        let actions = if let Some(action) = action.match_type() {
            self.handle_evaluate_start(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_evaluate_stop(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_evaluate_result(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_effect_subscribe(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_effect_unsubscribe(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_effect_emit(&mut state, action, metadata, context)
        } else {
            None
        }
        .unwrap_or_default();
        ActorTransition::new(state, actions)
    }
}
impl<T: Expression> QueryInspector<T> {
    fn handle_evaluate_start<TAction>(
        &self,
        state: &mut QueryInspectorState<T>,
        action: &EvaluateStartAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action,
    {
        let EvaluateStartAction {
            cache_id,
            label,
            query,
            evaluation_mode,
            invalidation_strategy,
        } = action;
        match state.active_workers.entry(*cache_id) {
            Entry::Occupied(_) => None,
            Entry::Vacant(entry) => {
                entry.insert(QueryInspectorWorkerState {
                    label: label.clone(),
                    query: query.clone(),
                    evaluation_mode: *evaluation_mode,
                    invalidation_strategy: *invalidation_strategy,
                    latest_result: Default::default(),
                });
                None
            }
        }
    }
    fn handle_evaluate_stop<TAction>(
        &self,
        state: &mut QueryInspectorState<T>,
        action: &EvaluateStopAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action,
    {
        let EvaluateStopAction { cache_id } = action;
        match state.active_workers.entry(*cache_id) {
            Entry::Occupied(entry) => {
                entry.remove();
                None
            }
            Entry::Vacant(_) => None,
        }
    }
    fn handle_evaluate_result<TAction>(
        &self,
        state: &mut QueryInspectorState<T>,
        action: &EvaluateResultAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action,
    {
        let EvaluateResultAction {
            cache_id,
            state_index: _,
            result,
        } = action;
        let worker_state = state.active_workers.get_mut(cache_id)?;
        worker_state.latest_result.replace(result.clone());
        None
    }
    fn handle_effect_subscribe<TAction>(
        &self,
        state: &mut QueryInspectorState<T>,
        action: &EffectSubscribeAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action,
    {
        let EffectSubscribeAction {
            effect_type: _,
            effects,
        } = action;
        state.active_effects.extend(effects.iter().map(|effect| {
            (
                effect.id(),
                QueryInspectorEffectState {
                    effect: effect.clone(),
                    value: None,
                },
            )
        }));
        None
    }
    fn handle_effect_unsubscribe<TAction>(
        &self,
        state: &mut QueryInspectorState<T>,
        action: &EffectUnsubscribeAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action,
    {
        let EffectUnsubscribeAction {
            effect_type: _,
            effects,
        } = action;
        for state_token in effects.iter().map(|effect| effect.id()) {
            state.active_effects.remove(&state_token);
        }
        None
    }
    fn handle_effect_emit<TAction>(
        &self,
        state: &mut QueryInspectorState<T>,
        action: &EffectEmitAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action,
    {
        let EffectEmitAction { updates } = action;
        for (state_token, value) in updates.iter() {
            if let Some(effect_state) = state.active_effects.get_mut(state_token) {
                effect_state.value.replace(value.clone());
            }
        }
        None
    }
}
