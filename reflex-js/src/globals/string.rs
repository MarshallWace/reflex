// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::stdlib::Stdlib as JsStdlib;
use reflex::core::{Expression, ExpressionFactory, HeapAllocator};

pub fn global_string<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    _allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<JsStdlib>,
{
    factory.create_builtin_term(JsStdlib::ToString)
}

pub fn global_encode_uri_component<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    _allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<JsStdlib>,
{
    factory.create_builtin_term(JsStdlib::EncodeUriComponent)
}
