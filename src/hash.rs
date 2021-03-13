// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
pub fn prefix_hash(prefix: u8, hash: u32) -> u32 {
    apply_hash_iteration(hash, prefix)
}

pub fn hash_string(value: &str) -> u32 {
    hash_bytes(value.as_bytes())
}

pub fn combine_hashes(hash1: u32, hash2: u32) -> u32 {
    hash2
        .to_be_bytes()
        .iter()
        .copied()
        .fold(hash1, apply_hash_iteration)
}

pub fn hash_bytes(chars: &[u8]) -> u32 {
    return hash_byte_sequence(chars.iter().copied());
}

pub fn hash_byte_sequence(chars: impl IntoIterator<Item = u8>) -> u32 {
    chars.into_iter().fold(hash_seed(), apply_hash_iteration)
}

pub fn hash_sequence(hashes: impl IntoIterator<Item = u32>) -> u32 {
    hashes.into_iter().fold(hash_seed(), |hash, item| {
        item.to_be_bytes()
            .iter()
            .copied()
            .fold(hash, apply_hash_iteration)
    })
}

// djb2 hash function: http://www.cse.yorku.ca/~oz/hash.html
pub fn hash_seed() -> u32 {
    5381
}
fn apply_hash_iteration(hash: u32, byte: u8) -> u32 {
    hash.rotate_left(5)
        .wrapping_add(hash)
        .wrapping_add(byte as u32)
}
