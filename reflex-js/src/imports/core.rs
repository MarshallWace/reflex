// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{create_record, Expression, ExpressionFactory, HeapAllocator};
use reflex_stdlib::Stdlib;

pub fn import_core<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Stdlib>,
{
    create_record(
        [
            (
                factory.create_string_term(allocator.create_static_string("abs")),
                factory.create_builtin_term(Stdlib::Abs),
            ),
            (
                factory.create_string_term(allocator.create_static_string("add")),
                factory.create_builtin_term(Stdlib::Add),
            ),
            (
                factory.create_string_term(allocator.create_static_string("and")),
                factory.create_builtin_term(Stdlib::And),
            ),
            (
                factory.create_string_term(allocator.create_static_string("append")),
                factory.create_builtin_term(Stdlib::Append),
            ),
            (
                factory.create_string_term(allocator.create_static_string("apply")),
                factory.create_builtin_term(Stdlib::Apply),
            ),
            (
                factory.create_string_term(allocator.create_static_string("car")),
                factory.create_builtin_term(Stdlib::Car),
            ),
            (
                factory.create_string_term(allocator.create_static_string("cdr")),
                factory.create_builtin_term(Stdlib::Cdr),
            ),
            (
                factory.create_string_term(allocator.create_static_string("ceil")),
                factory.create_builtin_term(Stdlib::Ceil),
            ),
            (
                factory.create_string_term(allocator.create_static_string("collect")),
                factory.create_builtin_term(Stdlib::Collect),
            ),
            (
                factory.create_string_term(allocator.create_static_string("collectHashMap")),
                factory.create_builtin_term(Stdlib::CollectHashMap),
            ),
            (
                factory.create_string_term(allocator.create_static_string("collectHashSet")),
                factory.create_builtin_term(Stdlib::CollectHashSet),
            ),
            (
                factory.create_string_term(allocator.create_static_string("collectRecord")),
                factory.create_builtin_term(Stdlib::CollectRecord),
            ),
            (
                factory.create_string_term(allocator.create_static_string("collectList")),
                factory.create_builtin_term(Stdlib::CollectList),
            ),
            (
                factory.create_string_term(allocator.create_static_string("concat")),
                factory.create_builtin_term(Stdlib::Concat),
            ),
            (
                factory.create_string_term(allocator.create_static_string("cons")),
                factory.create_builtin_term(Stdlib::Cons),
            ),
            (
                factory.create_string_term(allocator.create_static_string("constructHashMap")),
                factory.create_builtin_term(Stdlib::ConstructHashMap),
            ),
            (
                factory.create_string_term(allocator.create_static_string("constructHashSet")),
                factory.create_builtin_term(Stdlib::ConstructHashSet),
            ),
            (
                factory.create_string_term(allocator.create_static_string("constructRecord")),
                factory.create_builtin_term(Stdlib::ConstructRecord),
            ),
            (
                factory.create_string_term(allocator.create_static_string("constructList")),
                factory.create_builtin_term(Stdlib::ConstructList),
            ),
            (
                factory.create_string_term(allocator.create_static_string("contains")),
                factory.create_builtin_term(Stdlib::Contains),
            ),
            (
                factory.create_string_term(allocator.create_static_string("divide")),
                factory.create_builtin_term(Stdlib::Divide),
            ),
            (
                factory.create_string_term(allocator.create_static_string("effect")),
                factory.create_builtin_term(Stdlib::Effect),
            ),
            (
                factory.create_string_term(allocator.create_static_string("endsWith")),
                factory.create_builtin_term(Stdlib::EndsWith),
            ),
            (
                factory.create_string_term(allocator.create_static_string("entries")),
                factory.create_builtin_term(Stdlib::Entries),
            ),
            (
                factory.create_string_term(allocator.create_static_string("eq")),
                factory.create_builtin_term(Stdlib::Eq),
            ),
            (
                factory.create_string_term(allocator.create_static_string("equal")),
                factory.create_builtin_term(Stdlib::Equal),
            ),
            (
                factory.create_string_term(allocator.create_static_string("filter")),
                factory.create_builtin_term(Stdlib::Filter),
            ),
            (
                factory.create_string_term(allocator.create_static_string("floor")),
                factory.create_builtin_term(Stdlib::Floor),
            ),
            (
                factory.create_string_term(allocator.create_static_string("get")),
                factory.create_builtin_term(Stdlib::Get),
            ),
            (
                factory.create_string_term(allocator.create_static_string("gt")),
                factory.create_builtin_term(Stdlib::Gt),
            ),
            (
                factory.create_string_term(allocator.create_static_string("gte")),
                factory.create_builtin_term(Stdlib::Gte),
            ),
            (
                factory.create_string_term(allocator.create_static_string("if")),
                factory.create_builtin_term(Stdlib::If),
            ),
            (
                factory.create_string_term(allocator.create_static_string("ifError")),
                factory.create_builtin_term(Stdlib::IfError),
            ),
            (
                factory.create_string_term(allocator.create_static_string("ifPending")),
                factory.create_builtin_term(Stdlib::IfPending),
            ),
            (
                factory.create_string_term(allocator.create_static_string("insert")),
                factory.create_builtin_term(Stdlib::Insert),
            ),
            (
                factory.create_string_term(allocator.create_static_string("keys")),
                factory.create_builtin_term(Stdlib::Keys),
            ),
            (
                factory.create_string_term(allocator.create_static_string("length")),
                factory.create_builtin_term(Stdlib::Length),
            ),
            (
                factory.create_string_term(allocator.create_static_string("lt")),
                factory.create_builtin_term(Stdlib::Lt),
            ),
            (
                factory.create_string_term(allocator.create_static_string("lte")),
                factory.create_builtin_term(Stdlib::Lte),
            ),
            (
                factory.create_string_term(allocator.create_static_string("map")),
                factory.create_builtin_term(Stdlib::Map),
            ),
            (
                factory.create_string_term(allocator.create_static_string("match")),
                factory.create_builtin_term(Stdlib::Match),
            ),
            (
                factory.create_string_term(allocator.create_static_string("max")),
                factory.create_builtin_term(Stdlib::Max),
            ),
            (
                factory.create_string_term(allocator.create_static_string("merge")),
                factory.create_builtin_term(Stdlib::Merge),
            ),
            (
                factory.create_string_term(allocator.create_static_string("min")),
                factory.create_builtin_term(Stdlib::Min),
            ),
            (
                factory.create_string_term(allocator.create_static_string("multiply")),
                factory.create_builtin_term(Stdlib::Multiply),
            ),
            (
                factory.create_string_term(allocator.create_static_string("not")),
                factory.create_builtin_term(Stdlib::Not),
            ),
            (
                factory.create_string_term(allocator.create_static_string("or")),
                factory.create_builtin_term(Stdlib::Or),
            ),
            (
                factory.create_string_term(allocator.create_static_string("pow")),
                factory.create_builtin_term(Stdlib::Pow),
            ),
            (
                factory.create_string_term(allocator.create_static_string("push")),
                factory.create_builtin_term(Stdlib::Push),
            ),
            (
                factory.create_string_term(allocator.create_static_string("pushFront")),
                factory.create_builtin_term(Stdlib::PushFront),
            ),
            (
                factory.create_string_term(allocator.create_static_string("reduce")),
                factory.create_builtin_term(Stdlib::Reduce),
            ),
            (
                factory.create_string_term(allocator.create_static_string("remainder")),
                factory.create_builtin_term(Stdlib::Remainder),
            ),
            (
                factory.create_string_term(allocator.create_static_string("replace")),
                factory.create_builtin_term(Stdlib::Replace),
            ),
            (
                factory.create_string_term(allocator.create_static_string("resolveArgs")),
                factory.create_builtin_term(Stdlib::ResolveArgs),
            ),
            (
                factory.create_string_term(allocator.create_static_string("resolveDeep")),
                factory.create_builtin_term(Stdlib::ResolveDeep),
            ),
            (
                factory.create_string_term(allocator.create_static_string("resolveHashMap")),
                factory.create_builtin_term(Stdlib::ResolveHashMap),
            ),
            (
                factory.create_string_term(allocator.create_static_string("resolveHashSet")),
                factory.create_builtin_term(Stdlib::ResolveHashSet),
            ),
            (
                factory.create_string_term(allocator.create_static_string("resolveShallow")),
                factory.create_builtin_term(Stdlib::ResolveShallow),
            ),
            (
                factory.create_string_term(allocator.create_static_string("resolveRecord")),
                factory.create_builtin_term(Stdlib::ResolveRecord),
            ),
            (
                factory.create_string_term(allocator.create_static_string("resolveList")),
                factory.create_builtin_term(Stdlib::ResolveList),
            ),
            (
                factory.create_string_term(allocator.create_static_string("round")),
                factory.create_builtin_term(Stdlib::Round),
            ),
            (
                factory.create_string_term(allocator.create_static_string("sequence")),
                factory.create_builtin_term(Stdlib::Sequence),
            ),
            (
                factory.create_string_term(allocator.create_static_string("slice")),
                factory.create_builtin_term(Stdlib::Slice),
            ),
            (
                factory.create_string_term(allocator.create_static_string("split")),
                factory.create_builtin_term(Stdlib::Split),
            ),
            (
                factory.create_string_term(allocator.create_static_string("startsWith")),
                factory.create_builtin_term(Stdlib::StartsWith),
            ),
            (
                factory.create_string_term(allocator.create_static_string("subtract")),
                factory.create_builtin_term(Stdlib::Subtract),
            ),
            (
                factory.create_string_term(allocator.create_static_string("values")),
                factory.create_builtin_term(Stdlib::Values),
            ),
        ],
        factory,
        allocator,
    )
}
