// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::Expression;

use crate::stdlib::globals::create_struct;

pub fn global_process(env_vars: impl IntoIterator<Item = (String, Expression)>) -> Expression {
    create_struct(vec![(String::from("env"), create_struct(env_vars))])
}
