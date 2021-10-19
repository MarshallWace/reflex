// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{
        Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory, FunctionArity,
        HeapAllocator, StringValue, StructFieldOffset,
    },
    lang::{as_integer, ValueTerm},
};

pub struct Get {}
impl Get {
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
}
impl<T: Expression> Applicable<T> for Get {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(&Self::ARITY))
    }
    fn apply(
        &self,
        mut args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let target = args.next().unwrap();
        let key = args.next().unwrap();
        if let Some(target) = factory.match_tuple_term(&target) {
            let field_offset = match factory.match_value_term(&key) {
                Some(ValueTerm::Int(value)) if *value >= 0 => Some(*value as StructFieldOffset),
                Some(ValueTerm::Float(value)) => match as_integer(*value) {
                    Some(value) if value >= 0 => Some(value as StructFieldOffset),
                    _ => None,
                },
                _ => None,
            };
            let value = field_offset.and_then(|field_offset| target.get(field_offset));
            match value {
                Some(value) => Ok(value.clone()),
                None => Err(format!("Invalid field offset: {} on tuple {}", key, target)),
            }
        } else if let Some(target) = factory.match_struct_term(&target) {
            let field_name = match factory.match_value_term(&key) {
                Some(ValueTerm::String(key)) => Some(key),
                _ => None,
            };
            let value = field_name.and_then(|key| target.get(key.as_str()));
            match value {
                Some(value) => Ok(value.clone()),
                None => Err(format!(
                    "Invalid field access: {} on struct {}",
                    key, target
                )),
            }
        } else if let Some(target) = factory.match_vector_term(&target) {
            let index = match factory.match_value_term(&key) {
                Some(ValueTerm::Int(value)) if *value >= 0 => Some(*value as usize),
                Some(ValueTerm::Float(value)) => match as_integer(*value) {
                    Some(value) if value >= 0 => Some(value as usize),
                    _ => None,
                },
                _ => None,
            };
            match index {
                None => Err(format!(
                    "Invalid array index: Expected integer, received {}",
                    key,
                )),
                Some(index) => match target.items().get(index) {
                    Some(value) => Ok(value.clone()),
                    None => Ok(factory.create_value_term(ValueTerm::Null)),
                },
            }
        } else if let Some(target) = factory.match_hashmap_term(&target) {
            match target.get(&key) {
                Some(value) => Ok(value.clone()),
                None => Ok(factory.create_value_term(ValueTerm::Null)),
            }
        } else {
            Err(format!("Unable to access field {} on {}", key, target,))
        }
    }
}
