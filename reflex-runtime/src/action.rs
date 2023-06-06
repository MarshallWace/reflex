// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::Expression;
use reflex_dispatcher::{Action, Named, SerializableAction, SerializedAction};
use serde::Deserialize;
use serde::Serialize;

pub mod bytecode_interpreter;
pub mod effect;
pub mod evaluate;
pub mod query;

use self::effect::*;
use self::evaluate::*;
use self::query::*;

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub enum RuntimeActions<T: Expression> {
    #[serde(bound(
        serialize = "<T as Expression>::Signal: Serialize",
        deserialize = "<T as Expression>::Signal: Deserialize<'de>"
    ))]
    Effect(EffectActions<T>),
    Evaluate(EvaluateActions<T>),
    Query(QueryActions<T>),
}
impl<T: Expression> Named for RuntimeActions<T> {
    fn name(&self) -> &'static str {
        match self {
            Self::Effect(action) => action.name(),
            Self::Evaluate(action) => action.name(),
            Self::Query(action) => action.name(),
        }
    }
}
impl<T: Expression> Action for RuntimeActions<T> {}
impl<T: Expression> SerializableAction for RuntimeActions<T> {
    fn to_json(&self) -> SerializedAction {
        match self {
            Self::Effect(action) => action.to_json(),
            Self::Evaluate(action) => action.to_json(),
            Self::Query(action) => action.to_json(),
        }
    }
}

impl<T: Expression> From<EffectActions<T>> for RuntimeActions<T> {
    fn from(value: EffectActions<T>) -> Self {
        Self::Effect(value)
    }
}
impl<T: Expression> From<RuntimeActions<T>> for Option<EffectActions<T>> {
    fn from(value: RuntimeActions<T>) -> Self {
        match value {
            RuntimeActions::Effect(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a RuntimeActions<T>> for Option<&'a EffectActions<T>> {
    fn from(value: &'a RuntimeActions<T>) -> Self {
        match value {
            RuntimeActions::Effect(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<EvaluateActions<T>> for RuntimeActions<T> {
    fn from(value: EvaluateActions<T>) -> Self {
        Self::Evaluate(value)
    }
}
impl<T: Expression> From<RuntimeActions<T>> for Option<EvaluateActions<T>> {
    fn from(value: RuntimeActions<T>) -> Self {
        match value {
            RuntimeActions::Evaluate(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a RuntimeActions<T>> for Option<&'a EvaluateActions<T>> {
    fn from(value: &'a RuntimeActions<T>) -> Self {
        match value {
            RuntimeActions::Evaluate(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<QueryActions<T>> for RuntimeActions<T> {
    fn from(value: QueryActions<T>) -> Self {
        Self::Query(value)
    }
}
impl<T: Expression> From<RuntimeActions<T>> for Option<QueryActions<T>> {
    fn from(value: RuntimeActions<T>) -> Self {
        match value {
            RuntimeActions::Query(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a RuntimeActions<T>> for Option<&'a QueryActions<T>> {
    fn from(value: &'a RuntimeActions<T>) -> Self {
        match value {
            RuntimeActions::Query(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<EffectSubscribeAction<T>> for RuntimeActions<T> {
    fn from(value: EffectSubscribeAction<T>) -> Self {
        EffectActions::from(value).into()
    }
}
impl<T: Expression> From<RuntimeActions<T>> for Option<EffectSubscribeAction<T>> {
    fn from(value: RuntimeActions<T>) -> Self {
        Option::<EffectActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a RuntimeActions<T>> for Option<&'a EffectSubscribeAction<T>> {
    fn from(value: &'a RuntimeActions<T>) -> Self {
        Option::<&'a EffectActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EffectUnsubscribeAction<T>> for RuntimeActions<T> {
    fn from(value: EffectUnsubscribeAction<T>) -> Self {
        EffectActions::from(value).into()
    }
}
impl<T: Expression> From<RuntimeActions<T>> for Option<EffectUnsubscribeAction<T>> {
    fn from(value: RuntimeActions<T>) -> Self {
        Option::<EffectActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a RuntimeActions<T>> for Option<&'a EffectUnsubscribeAction<T>> {
    fn from(value: &'a RuntimeActions<T>) -> Self {
        Option::<&'a EffectActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EffectEmitAction<T>> for RuntimeActions<T> {
    fn from(value: EffectEmitAction<T>) -> Self {
        EffectActions::from(value).into()
    }
}
impl<T: Expression> From<RuntimeActions<T>> for Option<EffectEmitAction<T>> {
    fn from(value: RuntimeActions<T>) -> Self {
        Option::<EffectActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a RuntimeActions<T>> for Option<&'a EffectEmitAction<T>> {
    fn from(value: &'a RuntimeActions<T>) -> Self {
        Option::<&'a EffectActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EvaluateStartAction<T>> for RuntimeActions<T> {
    fn from(value: EvaluateStartAction<T>) -> Self {
        EvaluateActions::from(value).into()
    }
}
impl<T: Expression> From<RuntimeActions<T>> for Option<EvaluateStartAction<T>> {
    fn from(value: RuntimeActions<T>) -> Self {
        Option::<EvaluateActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a RuntimeActions<T>> for Option<&'a EvaluateStartAction<T>> {
    fn from(value: &'a RuntimeActions<T>) -> Self {
        Option::<&'a EvaluateActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EvaluateUpdateAction<T>> for RuntimeActions<T> {
    fn from(value: EvaluateUpdateAction<T>) -> Self {
        EvaluateActions::from(value).into()
    }
}
impl<T: Expression> From<RuntimeActions<T>> for Option<EvaluateUpdateAction<T>> {
    fn from(value: RuntimeActions<T>) -> Self {
        Option::<EvaluateActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a RuntimeActions<T>> for Option<&'a EvaluateUpdateAction<T>> {
    fn from(value: &'a RuntimeActions<T>) -> Self {
        Option::<&'a EvaluateActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EvaluateStopAction> for RuntimeActions<T> {
    fn from(value: EvaluateStopAction) -> Self {
        EvaluateActions::from(value).into()
    }
}
impl<T: Expression> From<RuntimeActions<T>> for Option<EvaluateStopAction> {
    fn from(value: RuntimeActions<T>) -> Self {
        Option::<EvaluateActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a RuntimeActions<T>> for Option<&'a EvaluateStopAction> {
    fn from(value: &'a RuntimeActions<T>) -> Self {
        Option::<&'a EvaluateActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EvaluateResultAction<T>> for RuntimeActions<T> {
    fn from(value: EvaluateResultAction<T>) -> Self {
        EvaluateActions::from(value).into()
    }
}
impl<T: Expression> From<RuntimeActions<T>> for Option<EvaluateResultAction<T>> {
    fn from(value: RuntimeActions<T>) -> Self {
        Option::<EvaluateActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a RuntimeActions<T>> for Option<&'a EvaluateResultAction<T>> {
    fn from(value: &'a RuntimeActions<T>) -> Self {
        Option::<&'a EvaluateActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<QuerySubscribeAction<T>> for RuntimeActions<T> {
    fn from(value: QuerySubscribeAction<T>) -> Self {
        QueryActions::from(value).into()
    }
}
impl<T: Expression> From<RuntimeActions<T>> for Option<QuerySubscribeAction<T>> {
    fn from(value: RuntimeActions<T>) -> Self {
        Option::<QueryActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a RuntimeActions<T>> for Option<&'a QuerySubscribeAction<T>> {
    fn from(value: &'a RuntimeActions<T>) -> Self {
        Option::<&'a QueryActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<QueryUnsubscribeAction<T>> for RuntimeActions<T> {
    fn from(value: QueryUnsubscribeAction<T>) -> Self {
        QueryActions::from(value).into()
    }
}
impl<T: Expression> From<RuntimeActions<T>> for Option<QueryUnsubscribeAction<T>> {
    fn from(value: RuntimeActions<T>) -> Self {
        Option::<QueryActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a RuntimeActions<T>> for Option<&'a QueryUnsubscribeAction<T>> {
    fn from(value: &'a RuntimeActions<T>) -> Self {
        Option::<&'a QueryActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<QueryEmitAction<T>> for RuntimeActions<T> {
    fn from(value: QueryEmitAction<T>) -> Self {
        QueryActions::from(value).into()
    }
}
impl<T: Expression> From<RuntimeActions<T>> for Option<QueryEmitAction<T>> {
    fn from(value: RuntimeActions<T>) -> Self {
        Option::<QueryActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a RuntimeActions<T>> for Option<&'a QueryEmitAction<T>> {
    fn from(value: &'a RuntimeActions<T>) -> Self {
        Option::<&'a QueryActions<T>>::from(value).and_then(|value| value.into())
    }
}
