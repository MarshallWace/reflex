// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::core::{
    match_typed_expression_list, Applicable, Arity, EvaluationCache, Expression, ExpressionFactory,
    HeapAllocator, StringValue,
};

pub struct Struct {}
impl<T: Expression> Applicable<T> for Struct {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(2, 0, None))
    }
    fn apply(
        &self,
        args: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
        if args.len() != 2 {
            return Err(format!("Expected 2 arguments, received {}", args.len()));
        }
        let keys = args.next().unwrap();
        let values = args.next().unwrap();
        let (num_keys, keys) = match factory.match_vector_term(&keys) {
            Some(keys) => {
                let num_keys = keys.items().len();
                match_typed_expression_list(
                    keys.items().iter(),
                    |key| {
                        factory
                            .match_value_term(key)
                            .and_then(|key| match key.match_string() {
                                Some(key) => Some(String::from(key.as_str())),
                                None => None,
                            })
                    },
                    |key| format!("Invalid property key: {}", key),
                )
                .map(|keys| (num_keys, keys))
            }
            None => Err(format!("Invalid property keys: {}", keys)),
        }?;
        match factory.match_vector_term(&values) {
            Some(values) => {
                if values.items().len() != num_keys {
                    Err(format!(
                        "Invalid property entries: received {} keys and {} values",
                        num_keys,
                        values.items().len(),
                    ))
                } else {
                    Ok(factory.create_struct_term(
                        allocator.create_struct_prototype(keys),
                        allocator.clone_list(values.items()),
                    ))
                }
            }
            None => Err(format!("Invalid property values: {}", values)),
        }
    }
}
