// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::stdlib::Stdlib as JsStdlib;
use reflex::core::{Expression, ExpressionFactory};
use reflex_stdlib::Stdlib;

pub fn global_date<T: Expression>(factory: &impl ExpressionFactory<T>) -> T
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    factory.create_builtin_term(JsStdlib::DateConstructor)
}
