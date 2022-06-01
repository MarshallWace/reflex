// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator},
    lang::create_struct,
    stdlib::Stdlib,
};

pub fn import_core<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Stdlib>,
{
    create_struct(
        [
            (
                String::from("abs"),
                factory.create_builtin_term(Stdlib::Abs),
            ),
            (
                String::from("add"),
                factory.create_builtin_term(Stdlib::Add),
            ),
            (
                String::from("and"),
                factory.create_builtin_term(Stdlib::And),
            ),
            (
                String::from("append"),
                factory.create_builtin_term(Stdlib::Append),
            ),
            (
                String::from("apply"),
                factory.create_builtin_term(Stdlib::Apply),
            ),
            (
                String::from("car"),
                factory.create_builtin_term(Stdlib::Car),
            ),
            (
                String::from("cdr"),
                factory.create_builtin_term(Stdlib::Cdr),
            ),
            (
                String::from("ceil"),
                factory.create_builtin_term(Stdlib::Ceil),
            ),
            (
                String::from("collect"),
                factory.create_builtin_term(Stdlib::Collect),
            ),
            (
                String::from("collectHashMap"),
                factory.create_builtin_term(Stdlib::CollectHashMap),
            ),
            (
                String::from("collectHashSet"),
                factory.create_builtin_term(Stdlib::CollectHashSet),
            ),
            (
                String::from("collectStruct"),
                factory.create_builtin_term(Stdlib::CollectStruct),
            ),
            (
                String::from("collectTuple"),
                factory.create_builtin_term(Stdlib::CollectTuple),
            ),
            (
                String::from("collectVector"),
                factory.create_builtin_term(Stdlib::CollectVector),
            ),
            (
                String::from("concat"),
                factory.create_builtin_term(Stdlib::Concat),
            ),
            (
                String::from("cons"),
                factory.create_builtin_term(Stdlib::Cons),
            ),
            (
                String::from("constructHashMap"),
                factory.create_builtin_term(Stdlib::ConstructHashMap),
            ),
            (
                String::from("constructHashSet"),
                factory.create_builtin_term(Stdlib::ConstructHashSet),
            ),
            (
                String::from("constructStruct"),
                factory.create_builtin_term(Stdlib::ConstructStruct),
            ),
            (
                String::from("constructTuple"),
                factory.create_builtin_term(Stdlib::ConstructTuple),
            ),
            (
                String::from("constructVector"),
                factory.create_builtin_term(Stdlib::ConstructVector),
            ),
            (
                String::from("contains"),
                factory.create_builtin_term(Stdlib::Contains),
            ),
            (
                String::from("divide"),
                factory.create_builtin_term(Stdlib::Divide),
            ),
            (
                String::from("effect"),
                factory.create_builtin_term(Stdlib::Effect),
            ),
            (
                String::from("endsWith"),
                factory.create_builtin_term(Stdlib::EndsWith),
            ),
            (
                String::from("entries"),
                factory.create_builtin_term(Stdlib::Entries),
            ),
            (String::from("eq"), factory.create_builtin_term(Stdlib::Eq)),
            (
                String::from("equal"),
                factory.create_builtin_term(Stdlib::Equal),
            ),
            (
                String::from("filter"),
                factory.create_builtin_term(Stdlib::Filter),
            ),
            (
                String::from("floor"),
                factory.create_builtin_term(Stdlib::Floor),
            ),
            (
                String::from("get"),
                factory.create_builtin_term(Stdlib::Get),
            ),
            (String::from("gt"), factory.create_builtin_term(Stdlib::Gt)),
            (
                String::from("gte"),
                factory.create_builtin_term(Stdlib::Gte),
            ),
            (String::from("if"), factory.create_builtin_term(Stdlib::If)),
            (
                String::from("ifError"),
                factory.create_builtin_term(Stdlib::IfError),
            ),
            (
                String::from("ifPending"),
                factory.create_builtin_term(Stdlib::IfPending),
            ),
            (
                String::from("insert"),
                factory.create_builtin_term(Stdlib::Insert),
            ),
            (
                String::from("keys"),
                factory.create_builtin_term(Stdlib::Keys),
            ),
            (
                String::from("length"),
                factory.create_builtin_term(Stdlib::Length),
            ),
            (String::from("lt"), factory.create_builtin_term(Stdlib::Lt)),
            (
                String::from("lte"),
                factory.create_builtin_term(Stdlib::Lte),
            ),
            (
                String::from("map"),
                factory.create_builtin_term(Stdlib::Map),
            ),
            (
                String::from("match"),
                factory.create_builtin_term(Stdlib::Match),
            ),
            (
                String::from("max"),
                factory.create_builtin_term(Stdlib::Max),
            ),
            (
                String::from("merge"),
                factory.create_builtin_term(Stdlib::Merge),
            ),
            (
                String::from("min"),
                factory.create_builtin_term(Stdlib::Min),
            ),
            (
                String::from("multiply"),
                factory.create_builtin_term(Stdlib::Multiply),
            ),
            (
                String::from("not"),
                factory.create_builtin_term(Stdlib::Not),
            ),
            (String::from("or"), factory.create_builtin_term(Stdlib::Or)),
            (
                String::from("pow"),
                factory.create_builtin_term(Stdlib::Pow),
            ),
            (
                String::from("push"),
                factory.create_builtin_term(Stdlib::Push),
            ),
            (
                String::from("pushFront"),
                factory.create_builtin_term(Stdlib::PushFront),
            ),
            (
                String::from("reduce"),
                factory.create_builtin_term(Stdlib::Reduce),
            ),
            (
                String::from("remainder"),
                factory.create_builtin_term(Stdlib::Remainder),
            ),
            (
                String::from("replace"),
                factory.create_builtin_term(Stdlib::Replace),
            ),
            (
                String::from("resolveArgs"),
                factory.create_builtin_term(Stdlib::ResolveArgs),
            ),
            (
                String::from("resolveDeep"),
                factory.create_builtin_term(Stdlib::ResolveDeep),
            ),
            (
                String::from("resolveHashMap"),
                factory.create_builtin_term(Stdlib::ResolveHashMap),
            ),
            (
                String::from("resolveHashSet"),
                factory.create_builtin_term(Stdlib::ResolveHashSet),
            ),
            (
                String::from("resolveShallow"),
                factory.create_builtin_term(Stdlib::ResolveShallow),
            ),
            (
                String::from("resolveStruct"),
                factory.create_builtin_term(Stdlib::ResolveStruct),
            ),
            (
                String::from("resolveTuple"),
                factory.create_builtin_term(Stdlib::ResolveTuple),
            ),
            (
                String::from("resolveVector"),
                factory.create_builtin_term(Stdlib::ResolveVector),
            ),
            (
                String::from("round"),
                factory.create_builtin_term(Stdlib::Round),
            ),
            (
                String::from("sequence"),
                factory.create_builtin_term(Stdlib::Sequence),
            ),
            (
                String::from("slice"),
                factory.create_builtin_term(Stdlib::Slice),
            ),
            (
                String::from("split"),
                factory.create_builtin_term(Stdlib::Split),
            ),
            (
                String::from("startsWith"),
                factory.create_builtin_term(Stdlib::StartsWith),
            ),
            (
                String::from("subtract"),
                factory.create_builtin_term(Stdlib::Subtract),
            ),
            (
                String::from("values"),
                factory.create_builtin_term(Stdlib::Values),
            ),
        ],
        factory,
        allocator,
    )
}
