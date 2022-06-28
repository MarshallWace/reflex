// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::once;

pub fn partition_results<T1, T2, V, E>(results: impl IntoIterator<Item = Result<V, E>>) -> (T1, T2)
where
    T1: Default + Extend<V>,
    T2: Default + Extend<E>,
{
    let mut values = T1::default();
    let mut errors = T2::default();
    for result in results {
        match result {
            Ok(value) => values.extend(once(value)),
            Err(error) => errors.extend(once(error)),
        }
    }
    (values, errors)
}
