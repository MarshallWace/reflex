// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use serde_json::Value;

use crate::{
    compiler::Compile,
    core::{Expression, StringValue},
};

use super::*;

pub trait SerializeJson {
    fn to_json(&self) -> Result<Value, String>;
}

impl<TString: StringValue> SerializeJson for ValueTerm<TString> {
    fn to_json(&self) -> Result<Value, String> {
        match self {
            ValueTerm::Null => Ok(Value::Null),
            ValueTerm::Boolean(boolean) => Ok(Value::Bool(*boolean)),
            ValueTerm::Int(integer) => Ok(Value::Number((*integer).into())),
            ValueTerm::String(string) => Ok(Value::String(string.as_str().to_owned())),
            ValueTerm::Float(float) => match serde_json::Number::from_f64(*float) {
                Some(number) => Ok(Value::Number(number)),
                None => Err(format!(
                    "Unable to serialize float as it is NaN or infinite: {}",
                    self
                )),
            },
            _ => Err(format!("Unable to serialize term: {}", self)),
        }
    }
}

impl<T: Expression> SerializeJson for TupleTerm<T> {
    fn to_json(&self) -> Result<Value, String> {
        self.fields()
            .iter()
            .map(|key| key.to_json())
            .collect::<Result<Vec<Value>, String>>()
            .map(|values| Value::Array(values))
    }
}

impl<T: Expression> SerializeJson for CollectionTerm<T> {
    fn to_json(&self) -> Result<Value, String> {
        match self {
            CollectionTerm::Vector(term) => term.to_json(),
            CollectionTerm::HashMap(term) => term.to_json(),
            CollectionTerm::HashSet(term) => term.to_json(),
        }
    }
}

impl<T: Expression> SerializeJson for VectorTerm<T> {
    fn to_json(&self) -> Result<Value, String> {
        self.items()
            .iter()
            .map(|key| key.to_json())
            .collect::<Result<Vec<Value>, String>>()
            .map(|values| Value::Array(values))
    }
}

impl<T: Expression> SerializeJson for HashSetTerm<T> {
    fn to_json(&self) -> Result<Value, String> {
        Err(format!("Unable to serialize term: {}", self))
    }
}

impl<T: Expression> SerializeJson for HashMapTerm<T> {
    fn to_json(&self) -> Result<Value, String> {
        Err(format!("Unable to serialize term: {}", self))
    }
}

impl<T: Expression> SerializeJson for StructTerm<T> {
    fn to_json(&self) -> Result<Value, String> {
        let map: Result<serde_json::Map<String, Value>, String> = self
            .prototype()
            .keys()
            .iter()
            .zip(self.fields().iter())
            .map(|(key, value)| {
                let value = value.to_json()?;
                Ok((key.clone(), value))
            })
            .collect();

        Ok(map?.into())
    }
}

impl SerializeJson for SharedTerm {
    fn to_json(&self) -> Result<Value, String> {
        (*self.value).to_json()
    }
}

impl<T: Expression> SerializeJson for CachedTerm<T> {
    fn to_json(&self) -> Result<Value, String> {
        self.value().to_json()
    }
}

impl<'a, T: Expression + Compile<T>> SerializeJson for Term<T> {
    fn to_json(&self) -> Result<Value, String> {
        match self {
            Term::Value(term) => term.to_json(),
            Term::Variable(term) => term.to_json(),
            Term::Let(term) => term.to_json(),
            Term::Lambda(term) => term.to_json(),
            Term::Application(term) => term.to_json(),
            Term::PartialApplication(term) => term.to_json(),
            Term::Recursive(term) => term.to_json(),
            Term::CompiledFunction(term) => term.to_json(),
            Term::Builtin(term) => term.to_json(),
            Term::Native(term) => term.to_json(),
            Term::Tuple(term) => term.to_json(),
            Term::Struct(term) => term.to_json(),
            Term::Constructor(term) => term.to_json(),
            Term::Collection(term) => term.to_json(),
            Term::Signal(term) => term.to_json(),
            Term::SignalTransformer(term) => term.to_json(),
        }
    }
}

impl<T: Expression> SerializeJson for VariableTerm<T> {
    fn to_json(&self) -> Result<Value, String> {
        match self {
            Self::Static(term) => term.to_json(),
            Self::Dynamic(term) => term.to_json(),
        }
    }
}

impl SerializeJson for StaticVariableTerm {
    fn to_json(&self) -> Result<Value, String> {
        Err(format!("Unable to serialize term: {}", self))
    }
}

impl<T: Expression> SerializeJson for DynamicVariableTerm<T> {
    fn to_json(&self) -> Result<Value, String> {
        Err(format!("Unable to serialize term: {}", self))
    }
}

impl<T: Expression> SerializeJson for LetTerm<T> {
    fn to_json(&self) -> Result<Value, String> {
        Err(format!("Unable to serialize term: {}", self))
    }
}
impl<T: Expression> SerializeJson for LambdaTerm<T> {
    fn to_json(&self) -> Result<Value, String> {
        Err(format!("Unable to serialize term: {}", self))
    }
}
impl<T: Expression> SerializeJson for ApplicationTerm<T> {
    fn to_json(&self) -> Result<Value, String> {
        Err(format!("Unable to serialize term: {}", self))
    }
}
impl<T: Expression> SerializeJson for PartialApplicationTerm<T> {
    fn to_json(&self) -> Result<Value, String> {
        Err(format!("Unable to serialize term: {}", self))
    }
}
impl<T: Expression> SerializeJson for RecursiveTerm<T> {
    fn to_json(&self) -> Result<Value, String> {
        Err(format!("Unable to serialize term: {}", self))
    }
}
impl SerializeJson for CompiledFunctionTerm {
    fn to_json(&self) -> Result<Value, String> {
        Err(format!("Unable to serialize term: {}", self))
    }
}
impl SerializeJson for BuiltinTerm {
    fn to_json(&self) -> Result<Value, String> {
        Err(format!("Unable to serialize term: {}", self))
    }
}
impl<T: Expression> SerializeJson for NativeFunctionTerm<T> {
    fn to_json(&self) -> Result<Value, String> {
        Err(format!("Unable to serialize term: {}", self))
    }
}
impl SerializeJson for ConstructorTerm {
    fn to_json(&self) -> Result<Value, String> {
        Err(format!("Unable to serialize term: {}", self))
    }
}
impl<T: Expression> SerializeJson for SignalTerm<T> {
    fn to_json(&self) -> Result<Value, String> {
        Err(format!("Unable to serialize term: {}", self))
    }
}
impl<T: Expression> SerializeJson for SignalTransformerTerm<T> {
    fn to_json(&self) -> Result<Value, String> {
        Err(format!("Unable to serialize term: {}", self))
    }
}
