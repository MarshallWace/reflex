// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{collections::HashMap, fmt, iter::once, rc::Rc};

use crate::{
    core::{
        capture_depth_multiple, dynamic_dependencies_multiple, optimize_multiple,
        substitute_multiple, ApplicationTerm, DependencyList, Expression, Rewritable, Signal,
        SignalTerm, StackOffset, StructTerm, Substitutions, Term,
    },
    hash::{combine_hashes, hash_unordered_sequence, HashId, Hashable},
    stdlib::{builtin::BuiltinTerm, signal::SignalType, value::ValueTerm},
};

use super::CollectionTerm;

#[derive(PartialEq, Clone, Debug)]
pub struct HashMapTerm {
    lookup: Rc<HashMap<HashId, usize>>,
    keys: Rc<Vec<Expression>>,
    values: Vec<Expression>,
}
impl Hashable for HashMapTerm {
    fn hash(&self) -> HashId {
        hash_unordered_sequence(
            self.keys
                .iter()
                .zip(self.values.iter())
                .map(|(key, value)| combine_hashes(key.hash(), value.hash())),
        )
    }
}
impl HashMapTerm {
    pub fn new(items: impl IntoIterator<Item = (Expression, Expression)>) -> Self {
        let (keys, values): (Vec<_>, Vec<_>) = items.into_iter().unzip();
        Self {
            lookup: Rc::new(build_lookup_table(&keys)),
            keys: Rc::new(keys),
            values,
        }
    }
    pub fn keys(&self) -> &[Expression] {
        &self.keys
    }
    pub fn values(&self) -> &[Expression] {
        &self.values
    }
    pub fn get(&self, key: &Expression) -> Option<&Expression> {
        match self.lookup.get(&key.hash()) {
            None => None,
            Some(index) => self.values.get(*index),
        }
    }
    pub fn set(&self, key: Expression, value: Expression) -> Option<Expression> {
        let updated_entries = match self.lookup.get(&key.hash()) {
            Some(index) => {
                let existing_value = self.values.get(*index).unwrap();
                if existing_value.hash() == value.hash() {
                    None
                } else {
                    let values = self
                        .values
                        .iter()
                        .take(*index)
                        .map(Expression::clone)
                        .chain(once(value))
                        .chain(self.values.iter().skip(index + 1).map(Expression::clone))
                        .collect();
                    Some((None, values))
                }
            }
            None => {
                let keys = self
                    .keys
                    .iter()
                    .map(Expression::clone)
                    .chain(once(key))
                    .collect();
                let values = self
                    .values
                    .iter()
                    .map(Expression::clone)
                    .chain(once(value))
                    .collect();
                Some((Some(keys), values))
            }
        };
        match updated_entries {
            None => None,
            Some((updated_keys, values)) => Some(Expression::new(Term::Collection(
                CollectionTerm::HashMap({
                    match updated_keys {
                        Some(keys) => Self {
                            lookup: Rc::new(build_lookup_table(&keys)),
                            keys: Rc::new(keys),
                            values,
                        },
                        None => Self {
                            lookup: Rc::clone(&self.lookup),
                            keys: Rc::clone(&self.keys),
                            values,
                        },
                    }
                }),
            ))),
        }
    }
    pub fn iterate(&self) -> impl IntoIterator<Item = Expression> + ExactSizeIterator + '_ {
        self.keys
            .iter()
            .zip(self.values.iter())
            .map(|(key, value)| {
                Expression::new(Term::Struct(StructTerm::new(
                    None,
                    vec![Expression::clone(key), Expression::clone(value)],
                )))
            })
    }
    pub fn collect(entries: impl IntoIterator<Item = Expression>) -> Expression {
        Expression::new(Term::Collection(CollectionTerm::HashMap(Self::new(
            entries.into_iter().map(|entry| {
                let static_entry = match entry.value() {
                    Term::Struct(entry) => match (entry.get(0), entry.get(1)) {
                        (Some(key), Some(value)) => {
                            Ok(Some((Expression::clone(key), Expression::clone(value))))
                        }
                        _ => Err(format!("Invalid Map entry: {}", entry)),
                    },
                    Term::Collection(CollectionTerm::Vector(entry)) => {
                        match (entry.get(0), entry.get(1)) {
                            (Some(key), Some(value)) => {
                                Ok(Some((Expression::clone(key), Expression::clone(value))))
                            }
                            _ => Err(format!("Invalid Map entry: {}", entry)),
                        }
                    }
                    _ => Ok(None),
                };
                match static_entry {
                    Ok(Some((key, value))) => (key, value),
                    Ok(None) => (
                        Expression::new(Term::Application(ApplicationTerm::new(
                            Expression::new(Term::Builtin(BuiltinTerm::Get)),
                            vec![
                                Expression::clone(&entry),
                                Expression::new(Term::Value(ValueTerm::Int(0))),
                            ],
                        ))),
                        Expression::new(Term::Application(ApplicationTerm::new(
                            Expression::new(Term::Builtin(BuiltinTerm::Get)),
                            vec![entry, Expression::new(Term::Value(ValueTerm::Int(1)))],
                        ))),
                    ),
                    Err(error) => {
                        let error = Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                            SignalType::Error,
                            vec![ValueTerm::String(error)],
                        ))));
                        (Expression::clone(&error), error)
                    }
                }
            }),
        ))))
    }
    fn update(
        &self,
        keys: Option<Vec<Expression>>,
        values: Option<Vec<Expression>>,
    ) -> Option<Self> {
        match keys {
            None => match values {
                None => None,
                Some(values) => Some(Self {
                    lookup: Rc::clone(&self.lookup),
                    keys: Rc::clone(&self.keys),
                    values,
                }),
            },
            Some(keys) => Some(Self {
                lookup: Rc::new(build_lookup_table(&keys)),
                keys: Rc::new(keys),
                values: values.unwrap_or_else(|| self.values.clone()),
            }),
        }
    }
}
impl Rewritable for HashMapTerm {
    fn capture_depth(&self) -> StackOffset {
        let keys = capture_depth_multiple(&self.keys);
        let values = capture_depth_multiple(&self.values);
        keys.max(values)
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        let keys = dynamic_dependencies_multiple(&self.keys);
        let values = dynamic_dependencies_multiple(&self.values);
        keys.extend(values)
    }
    fn substitute(&self, substitutions: &Substitutions) -> Option<Expression> {
        let keys = substitute_multiple(&self.keys, substitutions);
        let values = substitute_multiple(&self.values, substitutions);
        self.update(keys, values)
            .map(|updated| Expression::new(Term::Collection(CollectionTerm::HashMap(updated))))
    }
    fn optimize(&self) -> Option<Expression> {
        let keys = optimize_multiple(&self.keys);
        let values = optimize_multiple(&self.values);
        self.update(keys, values)
            .map(|updated| Expression::new(Term::Collection(CollectionTerm::HashMap(updated))))
    }
}
impl fmt::Display for HashMapTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<hashmap:{}>", self.keys.len())
    }
}

fn build_lookup_table<'a>(
    keys: impl IntoIterator<Item = &'a Expression>,
) -> HashMap<HashId, usize> {
    keys.into_iter()
        .enumerate()
        .map(|(index, key)| (key.hash(), index))
        .collect()
}
