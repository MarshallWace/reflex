// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    iter::once,
};

pub use fnv::{FnvHashMap, FnvHashSet, FnvHasher};
pub use nohash_hasher::{IntMap, IntSet};

pub type HashId = u64;

pub fn hash_iter(values: impl IntoIterator<Item = impl Hash>) -> HashId {
    values
        .into_iter()
        .fold(DefaultHasher::new(), |mut hasher, value| {
            value.hash(&mut hasher);
            hasher
        })
        .finish()
}

pub fn hash_object(value: &impl Hash) -> HashId {
    hash_iter(once(value))
}

pub fn combine_hashes(value1: &impl Hash, value2: &impl Hash) -> HashId {
    let mut hasher = FnvHasher::default();
    value1.hash(&mut hasher);
    value2.hash(&mut hasher);
    hasher.finish()
}
