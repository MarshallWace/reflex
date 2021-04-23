// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
};

pub type HashId = u32;

pub trait Hashable {
    fn hash(&self) -> HashId;
}

pub fn prefix_hash(prefix: u8, hash: HashId) -> HashId {
    apply_hash_iteration(hash, prefix)
}

pub fn hash_u8(value: u8) -> HashId {
    apply_hash_iteration(hash_seed(), value)
}

pub fn hash_u32(value: u32) -> HashId {
    hash_byte_sequence(value.to_be_bytes().iter().copied())
}

pub fn hash_i32(value: i32) -> HashId {
    hash_byte_sequence(value.to_be_bytes().iter().copied())
}

pub fn hash_f64(value: f64) -> HashId {
    hash_byte_sequence(value.to_be_bytes().iter().copied())
}

pub fn hash_string(value: &str) -> HashId {
    hash_bytes(value.as_bytes())
}

pub fn hash_bool(value: bool) -> HashId {
    hash_u8(if value { 1 } else { 0 })
}

pub fn hash_option(value: Option<HashId>) -> HashId {
    match value {
        None => prefix_hash(0, hash_seed()),
        Some(hash) => prefix_hash(1, hash),
    }
}

pub fn combine_hashes(hash1: HashId, hash2: HashId) -> HashId {
    hash2
        .to_be_bytes()
        .iter()
        .copied()
        .fold(hash1, apply_hash_iteration)
}

pub fn hash_bytes(chars: &[u8]) -> HashId {
    return hash_byte_sequence(chars.iter().copied());
}

pub fn hash_byte_sequence(chars: impl IntoIterator<Item = u8>) -> HashId {
    chars.into_iter().fold(hash_seed(), apply_hash_iteration)
}

pub fn hash_sequence(hashes: impl IntoIterator<Item = HashId>) -> HashId {
    hashes.into_iter().fold(hash_seed(), |hash, item| {
        item.to_be_bytes()
            .iter()
            .copied()
            .fold(hash, apply_hash_iteration)
    })
}

pub fn hash_unordered_sequence(hashes: impl IntoIterator<Item = HashId>) -> HashId {
    let (hash1, hash2) = hashes
        .into_iter()
        .fold((hash_seed(), hash_seed()), |(hash1, hash2), item| {
            (hash1.wrapping_add(item), hash2.wrapping_mul(item))
        });
    combine_hashes(hash1, hash2)
}

pub fn hash_object<T: Hash>(value: T) -> HashId {
    let mut hasher = DefaultHasher::new();
    value.hash(&mut hasher);
    let value = hasher.finish();
    hash_bytes(&value.to_be_bytes())
}

// djb2 hash function: http://www.cse.yorku.ca/~oz/hash.html
pub fn hash_seed() -> HashId {
    5381
}
fn apply_hash_iteration(hash: HashId, byte: u8) -> HashId {
    hash.rotate_left(5)
        .wrapping_add(hash)
        .wrapping_add(byte as HashId)
}
