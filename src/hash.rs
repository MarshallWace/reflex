// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
pub fn prefix_hash(prefix: u8, hash: u32) -> u32 {
    apply_hash_iteration(hash, prefix)
}

pub fn hash_string(value: &str) -> u32 {
    hash_bytes(value.as_bytes())
}

pub fn hash_bytes(chars: &[u8]) -> u32 {
    // djb2 hash function: http://www.cse.yorku.ca/~oz/hash.html
    return chars.into_iter().copied().fold(5381, apply_hash_iteration);
}

pub fn combine_hashes(hashes: &[u32]) -> u32 {
    hash_bytes(u32_slice_as_u8_slice(hashes))
}

fn u32_slice_as_u8_slice(values: &[u32]) -> &[u8] {
    unsafe {
        std::slice::from_raw_parts(
            values.as_ptr() as *const u8,
            values.len() * (std::mem::size_of::<u32>() / std::mem::size_of::<u8>()),
        )
    }
}

fn apply_hash_iteration(hash: u32, byte: u8) -> u32 {
    hash.rotate_left(5).wrapping_add(hash).wrapping_add(byte as u32)
}
