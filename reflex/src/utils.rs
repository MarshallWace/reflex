// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::VecDeque,
    iter::{once, FromIterator},
};

use fnv::FnvHashMap;

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
        self.cache.get(key).map(|entry| &entry.value)
    }
    pub fn set(&mut self, key: K, value: V, children: Vec<K>) {
        let (retain_count, existing_children, added_children) = match self.cache.remove(&key) {
            Some(existing) => {
                let existing_children = existing.children;
                let added_children = if children.is_empty() || existing_children.is_empty() {
                    children
                } else {
                    children
                        .into_iter()
                        .filter(|child| !existing_children.contains(child))
                        .collect::<Vec<_>>()
                };
                (existing.retain_count, existing_children, added_children)
            }
            None => (0, Vec::new(), children),
        };
        if retain_count > 0 {
            self.retain(added_children.iter());
        }
        self.cache.insert(
            key,
            DependencyCacheEntry {
                retain_count,
                value,
                children: {
                    let mut combined_children = existing_children;
                    combined_children.extend(added_children);
                    combined_children
                },
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
    pub fn gc(&mut self) {
        self.cache.retain(|_, value| value.retain_count > 0);
        self.cache.shrink_to_fit();
    }
}
struct DependencyCacheEntry<K, V> {
    value: V,
    retain_count: usize,
    children: Vec<K>,
}
