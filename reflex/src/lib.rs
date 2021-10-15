// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{
    collections::VecDeque,
    convert::identity,
    iter::{once, FromIterator},
};

use fnv::FnvHashMap;

pub mod allocator;
pub mod cache;
pub mod core;
pub mod hash;

pub mod compiler;
pub mod interpreter;

pub mod lang {
    mod factory;
    pub mod serialization;
    pub use factory::*;
    pub mod term;
    pub(crate) use term::*;
    pub use term::{
        as_integer, create_struct, deduplicate_hashmap_entries, deduplicate_hashset_entries,
        is_integer, is_truthy, BuiltinTerm, NativeFunction, ValueTerm,
    };
}

pub mod parser {
    pub mod sexpr;
}

pub struct DependencyCache<K, V> {
    cache: FnvHashMap<K, DependencyCacheEntry<K, V>>,
}
impl<K: std::fmt::Debug, V> std::fmt::Debug for DependencyCache<K, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.cache
                .iter()
                .filter(|(_, value)| value.retain_count > 0)
                .map(|(key, entry)| {
                    format!(
                        "\n {:?}: [{}]{}",
                        key,
                        entry.retain_count,
                        entry
                            .children
                            .iter()
                            .map(|child| format!("\n  {:?}", child))
                            .collect::<String>()
                    )
                })
                .collect::<String>()
        )
    }
}
impl<K, V> Default for DependencyCache<K, V> {
    fn default() -> Self {
        Self {
            cache: FnvHashMap::default(),
        }
    }
}
impl<K: Eq + Copy + std::fmt::Debug + std::hash::Hash + 'static, V> DependencyCache<K, V> {
    pub fn new(entries: impl IntoIterator<Item = (K, V, Vec<K>)>) -> Self {
        Self {
            cache: entries
                .into_iter()
                .map(|(key, value, children)| {
                    (
                        key,
                        DependencyCacheEntry {
                            retain_count: 0,
                            value,
                            children,
                        },
                    )
                })
                .collect(),
        }
    }

    pub fn len(&self) -> usize {
        self.cache.len()
    }
    pub fn contains_key(&self, key: &K) -> bool {
        self.cache.contains_key(key)
    }
    pub fn get(&self, key: &K) -> Option<&V> {
        self.cache.get(key).map(|entry| entry.value())
    }
    pub fn set(&mut self, key: K, value: V, children: Vec<K>) {
        let (retain_count, existing_children) = match self.cache.remove(&key) {
            Some(existing) => (existing.retain_count, existing.children),
            None => (0, Vec::new()),
        };
        if retain_count > 0 {
            self.retain(children.iter());
        }
        let mut combined_children = existing_children;
        combined_children.extend(children);
        self.cache.insert(
            key,
            DependencyCacheEntry {
                retain_count,
                value,
                children: combined_children,
            },
        );
    }
    pub fn replace_value(&mut self, key: &K, value: V) -> Option<V> {
        self.cache
            .get_mut(key)
            .map(|entry| std::mem::replace(&mut entry.value, value))
    }
    pub fn add_child(&mut self, key: &K, child: &K) -> bool {
        match self.cache.get_mut(key) {
            Some(entry) => {
                entry.children.push(*child);
                if entry.retain_count > 0 {
                    self.retain(once(child))
                }
                true
            }
            _ => false,
        }
    }
    pub fn remove_child(&mut self, key: &K, child: &K) -> bool {
        match self.cache.get_mut(key) {
            Some(entry) => {
                match entry
                    .children
                    .iter()
                    .position(|existing_child| existing_child == child)
                {
                    Some(index) => {
                        entry.children.remove(index);
                        if entry.retain_count > 0 {
                            self.release(once(child))
                        }
                        true
                    }
                    _ => false,
                }
            }
            _ => false,
        }
    }
    pub fn replace_children(&mut self, key: &K, children: Vec<K>) -> bool {
        let updates = match self.cache.get_mut(key) {
            Some(entry) => {
                if entry.retain_count > 0 {
                    let previous_children =
                        std::mem::replace(&mut entry.children, children.clone());
                    Some((children, previous_children))
                } else {
                    entry.children = children;
                    Some((Vec::new(), Vec::new()))
                }
            }
            _ => None,
        };
        match updates {
            Some((updated_children, previous_children)) => {
                self.retain(updated_children.iter());
                self.release(previous_children.iter());
                true
            }
            None => false,
        }
    }
    pub fn retain<'a>(&mut self, keys: impl IntoIterator<Item = &'a K>) {
        let mut queue = VecDeque::from_iter(keys.into_iter().copied());
        while let Some(key) = queue.pop_front() {
            let entry = match self.cache.get_mut(&key) {
                Some(entry) => entry,
                None => panic!("Invalid cache key: {:?}", key),
            };
            entry.retain_count += 1;
            if entry.retain_count == 1 {
                queue.extend(entry.children.iter().copied());
            }
        }
    }
    pub fn release<'a>(&mut self, keys: impl IntoIterator<Item = &'a K>) {
        let mut queue = VecDeque::from_iter(keys.into_iter().copied());
        while let Some(key) = queue.pop_front() {
            let entry = match self.cache.get_mut(&key) {
                Some(entry) => entry,
                None => panic!("Invalid cache key: {:?}", key),
            };
            if entry.retain_count == 0 {
                continue;
            }
            entry.retain_count -= 1;
            if entry.retain_count == 0 {
                queue.extend(entry.children.iter().copied());
            }
        }
    }
    pub fn gc<I: FromIterator<(K, V)>>(&mut self) -> I {
        let disposed_keys = self
            .cache
            .iter()
            .filter_map(|(key, value)| {
                if value.retain_count == 0 {
                    Some(*key)
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();
        disposed_keys
            .into_iter()
            .map(|key| {
                self.cache
                    .remove(&key)
                    .map(|entry| (key, entry.into_value()))
            })
            .filter_map(identity)
            .collect::<I>()
    }
    pub(crate) fn extend(&mut self, entries: impl IntoIterator<Item = (K, V, Vec<K>)>) {
        for (key, value, children) in entries {
            self.set(key, value, children);
        }
    }
}
struct DependencyCacheEntry<K, V> {
    value: V,
    retain_count: usize,
    children: Vec<K>,
}
impl<K, V> DependencyCacheEntry<K, V> {
    fn value(&self) -> &V {
        &self.value
    }
    fn into_value(self) -> V {
        self.value
    }
}
