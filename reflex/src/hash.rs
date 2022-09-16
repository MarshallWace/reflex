// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::hash::{Hash, Hasher};

pub use fnv::{FnvHashMap, FnvHashSet, FnvHasher};
pub use nohash_hasher::{IntMap, IntSet};

pub type HashId = u64;

pub fn hash_object(value: &impl Hash) -> HashId {
    let mut hasher = FnvHasher::default();
    value.hash(&mut hasher);
    hasher.finish()
}

pub fn combine_hashes(value1: &impl Hash, value2: &impl Hash) -> HashId {
    let mut hasher = FnvHasher::default();
    value1.hash(&mut hasher);
    value2.hash(&mut hasher);
    hasher.finish()
}
