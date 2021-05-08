// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::hash::{hash_seed, hash_string, prefix_hash, HashId, Hashable};

#[derive(PartialEq, Clone, Debug)]
pub enum SignalType {
    Error,
    Pending,
    Custom(String),
}
impl Hashable for SignalType {
    fn hash(&self) -> HashId {
        match self {
            SignalType::Error => prefix_hash(0, hash_seed()),
            SignalType::Pending => prefix_hash(1, hash_seed()),
            SignalType::Custom(signal) => prefix_hash(2, hash_string(signal)),
        }
    }
}
impl fmt::Display for SignalType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
