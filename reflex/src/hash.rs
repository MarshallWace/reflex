// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
};

use crate::core::{DynamicState, StateToken};

pub type HashId = u64;

pub fn hash_object<T: Hash>(value: &T) -> HashId {
    let mut hasher = DefaultHasher::new();
    value.hash(&mut hasher);
    hasher.finish()
}

pub fn hash_state_values(
    state: &DynamicState,
    state_tokens: impl IntoIterator<Item = StateToken>,
) -> HashId {
    let mut hasher = DefaultHasher::new();
    for state_token in state_tokens {
        match state.get(state_token) {
            None => hasher.write_u8(0),
            Some(value) => value.hash(&mut hasher),
        }
    }
    hasher.finish()
}
