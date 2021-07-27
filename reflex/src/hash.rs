// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
};

pub type HashId = u64;

pub fn hash_object(value: &impl Hash) -> HashId {
    let mut hasher = DefaultHasher::new();
    value.hash(&mut hasher);
    hasher.finish()
}

pub fn combine_hashes(value1: &impl Hash, value2: &impl Hash) -> HashId {
    let mut hasher = DefaultHasher::new();
    value1.hash(&mut hasher);
    value2.hash(&mut hasher);
    hasher.finish()
}
