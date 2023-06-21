// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use crate::{
    core::{
        uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
        FunctionArity, HeapAllocator, StringValue, StructFieldOffset, Uid, Uuid,
    },
    lang::as_integer,
};

pub struct Get {}
impl Get {
    pub(crate) const UUID: Uuid = uuid!("391eba98-8955-4158-bc3f-2c07dee394dc");
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Get {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Get {
    fn arity(&self) -> Option<Arity> {
        Some(Self::arity())
    }
    fn should_parallelize(&self, _args: &[T]) -> bool {
        false
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
            let field_offset = match factory.match_int_term(&key) {
                Some(term) if term.value >= 0 => Some(term.value as StructFieldOffset),
                _ => match factory.match_float_term(&key) {
                    Some(term) => match as_integer(term.value) {
                        Some(value) if value >= 0 => Some(value as StructFieldOffset),
                        _ => None,
                    },
                    _ => None,
                },
            };
            let value = field_offset.and_then(|field_offset| target.get(field_offset));
            match value {
                Some(value) => Ok(value.clone()),
                None => Err(format!("Invalid field offset: {} on tuple {}", key, target)),
            }
        } else if let Some(target) = factory.match_record_term(&target) {
            let field_name = match factory.match_string_term(&key) {
                Some(key) => Some(&key.value),
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
        } else if let Some(target) = factory.match_list_term(&target) {
            let index = match factory.match_int_term(&key) {
                Some(term) if term.value >= 0 => Some(term.value as usize),
                _ => match factory.match_float_term(&key) {
                    Some(term) => match as_integer(term.value) {
                        Some(value) if value >= 0 => Some(value as usize),
                        _ => None,
                    },
                    _ => None,
                },
            };
            match index {
                None => Err(format!(
                    "Invalid array index: Expected integer, received {}",
                    key,
                )),
                Some(index) => match target.items().get(index) {
                    Some(value) => Ok(value.clone()),
                    None => Ok(factory.create_nil_term()),
                },
            }
        } else if let Some(target) = factory.match_hashmap_term(&target) {
            match target.get(&key) {
                Some(value) => Ok(value.clone()),
                None => Ok(factory.create_nil_term()),
            }
        } else {
            Err(format!("Unable to access field {} on {}", key, target,))
        }
    }
}
