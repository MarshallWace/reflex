// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{hash_map::Entry, BTreeMap, HashMap},
    fmt,
    hash::Hash,
    iter::once,
};

use crate::{
    cache::EvaluationCache,
    core::{
        capture_depth_multiple, dynamic_dependencies_multiple, normalize_multiple,
        substitute_dynamic_multiple, substitute_static_multiple, ApplicationTerm, DependencyList,
        DynamicState, Expression, Rewritable, Signal, SignalTerm, StackOffset, StructTerm,
        Substitutions, Term,
    },
    hash::hash_object,
    stdlib::{builtin::BuiltinTerm, signal::SignalType, value::ValueTerm},
};

use super::CollectionTerm;

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct HashMapTerm {
    lookup: BTreeMap<Expression, usize>,
    keys: Vec<Expression>,
    values: Vec<Expression>,
}
impl Hash for HashMapTerm {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let values = &self.values;
        for (key, index) in self.lookup.iter() {
            let value = unsafe { values.get_unchecked(*index) };
            key.hash(state);
            value.hash(state);
        }
    }
}
impl HashMapTerm {
    pub fn new(items: impl IntoIterator<Item = (Expression, Expression)>) -> Self {
        let (keys, values) = get_unique_entries(items).into_iter().unzip();
        let lookup = build_lookup_table(&keys);
        Self {
            lookup,
            keys,
            values,
        }
    }
    pub fn keys(&self) -> &[Expression] {
        &self.keys
    }
    pub fn values(&self) -> &[Expression] {
        &self.values
    }
    pub fn len(&self) -> usize {
        self.values.len()
    }
    pub fn get(&self, key: &Expression) -> Option<&Expression> {
        match self.lookup.get(key) {
            None => None,
            Some(index) => self.values.get(*index),
        }
    }
    pub fn set(&self, key: Expression, value: Expression) -> Option<Expression> {
        let updated_entries = match self.lookup.get(&key) {
            Some(index) => {
                let existing_value = self.values.get(*index).unwrap();
                if hash_object(&value) == hash_object(&existing_value) {
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
                            lookup: build_lookup_table(&keys),
                            keys,
                            values,
                        },
                        None => Self {
                            lookup: self.lookup.clone(),
                            keys: self.keys.clone(),
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
                    Term::Struct(entry) if entry.prototype().is_none() => {
                        match (entry.get(0), entry.get(1)) {
                            (Some(key), Some(value)) => {
                                Ok(Some((Expression::clone(key), Expression::clone(value))))
                            }
                            _ => Err(format!("Invalid Map entry: {}", entry)),
                        }
                    }
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
                            vec![Expression::new(Term::Value(ValueTerm::String(error)))],
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
                    lookup: self.lookup.clone(),
                    keys: self.keys.clone(),
                    values,
                }),
            },
            Some(keys) => Some(Self {
                lookup: build_lookup_table(&keys),
                keys: keys,
                values: values.unwrap_or_else(|| self.values.clone()),
            }),
        }
    }
}
impl Rewritable for HashMapTerm {
    fn capture_depth(&self) -> StackOffset {
        capture_depth_multiple(&self.keys).max(capture_depth_multiple(&self.values))
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        dynamic_dependencies_multiple(&self.keys)
    }
    fn substitute_static(
        &self,
        substitutions: &Substitutions,
        cache: &mut impl EvaluationCache,
    ) -> Option<Expression> {
        let keys = substitute_static_multiple(&self.keys, substitutions, cache);
        let values = substitute_static_multiple(&self.values, substitutions, cache);
        self.update(keys, values)
            .map(|updated| Expression::new(Term::Collection(CollectionTerm::HashMap(updated))))
    }
    fn substitute_dynamic(
        &self,
        state: &DynamicState,
        cache: &mut impl EvaluationCache,
    ) -> Option<Expression> {
        let keys = substitute_dynamic_multiple(&self.keys, state, cache);
        let values = substitute_dynamic_multiple(&self.values, state, cache);
        self.update(keys, values)
            .map(|updated| Expression::new(Term::Collection(CollectionTerm::HashMap(updated))))
    }
    fn normalize(&self, cache: &mut impl EvaluationCache) -> Option<Expression> {
        let keys = normalize_multiple(&self.keys, cache);
        let values = normalize_multiple(&self.values, cache);
        self.update(keys, values)
            .map(|updated| Expression::new(Term::Collection(CollectionTerm::HashMap(updated))))
    }
}
impl fmt::Display for HashMapTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<hashmap:{}>", self.keys.len())
    }
}

fn get_unique_entries(
    items: impl IntoIterator<Item = (Expression, Expression)>,
) -> impl IntoIterator<Item = (Expression, Expression)> {
    let items = items.into_iter().collect::<Vec<_>>();
    let num_entries = items.len();
    let (entries, mut lookup) = items.into_iter().fold(
        (
            Vec::with_capacity(num_entries),
            HashMap::with_capacity(num_entries),
        ),
        |(mut entries, mut lookup), (key, value)| {
            match lookup.entry(Expression::clone(&key)) {
                Entry::Occupied(mut entry) => {
                    entry.insert(value);
                }
                Entry::Vacant(entry) => {
                    entry.insert(Expression::clone(&value));
                    entries.push((key, value));
                }
            }
            (entries, lookup)
        },
    );
    if entries.len() == num_entries {
        entries
    } else {
        entries
            .into_iter()
            .map(|(key, _)| {
                let value = lookup.remove(&key).unwrap();
                (key, value)
            })
            .collect::<Vec<_>>()
    }
}

fn build_lookup_table<'a>(
    keys: impl IntoIterator<Item = &'a Expression>,
) -> BTreeMap<Expression, usize> {
    keys.into_iter()
        .enumerate()
        .map(|(index, key)| (Expression::clone(key), index))
        .collect()
}
