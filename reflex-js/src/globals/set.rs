// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::stdlib::SetConstructor;
use reflex::core::{Expression, ExpressionFactory};

pub fn global_set<T: Expression>(factory: &impl ExpressionFactory<T>) -> T
where
    T::Builtin: From<SetConstructor>,
{
    factory.create_builtin_term(SetConstructor)
}
