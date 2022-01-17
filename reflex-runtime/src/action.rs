// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::Expression;

use reflex_dispatcher::{Action, NamedAction, SerializableAction, SerializedAction};

pub mod effect;
pub mod evaluate;
pub mod query;

use self::effect::*;
use self::evaluate::*;
use self::query::*;

#[derive(Clone, Debug)]
pub enum RuntimeAction<T: Expression> {
    Effect(EffectAction<T>),
    Evaluate(EvaluateAction<T>),
    Query(QueryAction<T>),
}
impl<T: Expression> Action for RuntimeAction<T> {}
impl<T: Expression> NamedAction for RuntimeAction<T> {
    fn name(&self) -> &'static str {
        match self {
            Self::Effect(action) => action.name(),
            Self::Evaluate(action) => action.name(),
            Self::Query(action) => action.name(),
        }
    }
}
impl<T: Expression> SerializableAction for RuntimeAction<T> {
    fn serialize(&self) -> SerializedAction {
        match self {
            Self::Effect(action) => action.serialize(),
            Self::Evaluate(action) => action.serialize(),
            Self::Query(action) => action.serialize(),
        }
    }
}

impl<T: Expression> From<EffectAction<T>> for RuntimeAction<T> {
    fn from(value: EffectAction<T>) -> Self {
        Self::Effect(value)
    }
}
impl<T: Expression> From<RuntimeAction<T>> for Option<EffectAction<T>> {
    fn from(value: RuntimeAction<T>) -> Self {
        match value {
            RuntimeAction::Effect(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a RuntimeAction<T>> for Option<&'a EffectAction<T>> {
    fn from(value: &'a RuntimeAction<T>) -> Self {
        match value {
            RuntimeAction::Effect(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<EvaluateAction<T>> for RuntimeAction<T> {
    fn from(value: EvaluateAction<T>) -> Self {
        Self::Evaluate(value)
    }
}
impl<T: Expression> From<RuntimeAction<T>> for Option<EvaluateAction<T>> {
    fn from(value: RuntimeAction<T>) -> Self {
        match value {
            RuntimeAction::Evaluate(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a RuntimeAction<T>> for Option<&'a EvaluateAction<T>> {
    fn from(value: &'a RuntimeAction<T>) -> Self {
        match value {
            RuntimeAction::Evaluate(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<QueryAction<T>> for RuntimeAction<T> {
    fn from(value: QueryAction<T>) -> Self {
        Self::Query(value)
    }
}
impl<T: Expression> From<RuntimeAction<T>> for Option<QueryAction<T>> {
    fn from(value: RuntimeAction<T>) -> Self {
        match value {
            RuntimeAction::Query(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a RuntimeAction<T>> for Option<&'a QueryAction<T>> {
    fn from(value: &'a RuntimeAction<T>) -> Self {
        match value {
            RuntimeAction::Query(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<EffectSubscribeAction<T>> for RuntimeAction<T> {
    fn from(value: EffectSubscribeAction<T>) -> Self {
        EffectAction::from(value).into()
    }
}
impl<T: Expression> From<RuntimeAction<T>> for Option<EffectSubscribeAction<T>> {
    fn from(value: RuntimeAction<T>) -> Self {
        Option::<EffectAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a RuntimeAction<T>> for Option<&'a EffectSubscribeAction<T>> {
    fn from(value: &'a RuntimeAction<T>) -> Self {
        Option::<&'a EffectAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EffectUnsubscribeAction<T>> for RuntimeAction<T> {
    fn from(value: EffectUnsubscribeAction<T>) -> Self {
        EffectAction::from(value).into()
    }
}
impl<T: Expression> From<RuntimeAction<T>> for Option<EffectUnsubscribeAction<T>> {
    fn from(value: RuntimeAction<T>) -> Self {
        Option::<EffectAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a RuntimeAction<T>> for Option<&'a EffectUnsubscribeAction<T>> {
    fn from(value: &'a RuntimeAction<T>) -> Self {
        Option::<&'a EffectAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EffectEmitAction<T>> for RuntimeAction<T> {
    fn from(value: EffectEmitAction<T>) -> Self {
        EffectAction::from(value).into()
    }
}
impl<T: Expression> From<RuntimeAction<T>> for Option<EffectEmitAction<T>> {
    fn from(value: RuntimeAction<T>) -> Self {
        Option::<EffectAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a RuntimeAction<T>> for Option<&'a EffectEmitAction<T>> {
    fn from(value: &'a RuntimeAction<T>) -> Self {
        Option::<&'a EffectAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EvaluateStartAction<T>> for RuntimeAction<T> {
    fn from(value: EvaluateStartAction<T>) -> Self {
        EvaluateAction::from(value).into()
    }
}
impl<T: Expression> From<RuntimeAction<T>> for Option<EvaluateStartAction<T>> {
    fn from(value: RuntimeAction<T>) -> Self {
        Option::<EvaluateAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a RuntimeAction<T>> for Option<&'a EvaluateStartAction<T>> {
    fn from(value: &'a RuntimeAction<T>) -> Self {
        Option::<&'a EvaluateAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EvaluateStopAction<T>> for RuntimeAction<T> {
    fn from(value: EvaluateStopAction<T>) -> Self {
        EvaluateAction::from(value).into()
    }
}
impl<T: Expression> From<RuntimeAction<T>> for Option<EvaluateStopAction<T>> {
    fn from(value: RuntimeAction<T>) -> Self {
        Option::<EvaluateAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a RuntimeAction<T>> for Option<&'a EvaluateStopAction<T>> {
    fn from(value: &'a RuntimeAction<T>) -> Self {
        Option::<&'a EvaluateAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EvaluateResultAction<T>> for RuntimeAction<T> {
    fn from(value: EvaluateResultAction<T>) -> Self {
        EvaluateAction::from(value).into()
    }
}
impl<T: Expression> From<RuntimeAction<T>> for Option<EvaluateResultAction<T>> {
    fn from(value: RuntimeAction<T>) -> Self {
        Option::<EvaluateAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a RuntimeAction<T>> for Option<&'a EvaluateResultAction<T>> {
    fn from(value: &'a RuntimeAction<T>) -> Self {
        Option::<&'a EvaluateAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<QuerySubscribeAction<T>> for RuntimeAction<T> {
    fn from(value: QuerySubscribeAction<T>) -> Self {
        QueryAction::from(value).into()
    }
}
impl<T: Expression> From<RuntimeAction<T>> for Option<QuerySubscribeAction<T>> {
    fn from(value: RuntimeAction<T>) -> Self {
        Option::<QueryAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a RuntimeAction<T>> for Option<&'a QuerySubscribeAction<T>> {
    fn from(value: &'a RuntimeAction<T>) -> Self {
        Option::<&'a QueryAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<QueryUnsubscribeAction<T>> for RuntimeAction<T> {
    fn from(value: QueryUnsubscribeAction<T>) -> Self {
        QueryAction::from(value).into()
    }
}
impl<T: Expression> From<RuntimeAction<T>> for Option<QueryUnsubscribeAction<T>> {
    fn from(value: RuntimeAction<T>) -> Self {
        Option::<QueryAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a RuntimeAction<T>> for Option<&'a QueryUnsubscribeAction<T>> {
    fn from(value: &'a RuntimeAction<T>) -> Self {
        Option::<&'a QueryAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<QueryEmitAction<T>> for RuntimeAction<T> {
    fn from(value: QueryEmitAction<T>) -> Self {
        QueryAction::from(value).into()
    }
}
impl<T: Expression> From<RuntimeAction<T>> for Option<QueryEmitAction<T>> {
    fn from(value: RuntimeAction<T>) -> Self {
        Option::<QueryAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a RuntimeAction<T>> for Option<&'a QueryEmitAction<T>> {
    fn from(value: &'a RuntimeAction<T>) -> Self {
        Option::<&'a QueryAction<T>>::from(value).and_then(|value| value.into())
    }
}
