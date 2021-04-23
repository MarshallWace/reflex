// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
enum Type {
    Boolean,
    Int,
    Float,
    String,
    Enum(EnumType),
    Struct(StructType),
    Fn(FnType),
    List(Box<Type>),
    Maybe(Box<Type>),
}

struct EnumType {
    variants: Vec<(SymbolId, Vec<Type>)>,
}

struct StructType {
    fields: Vec<(SymbolId, Type)>,
}

struct FnType {
    parameters: Vec<Type>,
    return_type: Box<Type>
}

struct Class {
    shape: StructType,
    methods: Vec<(SymbolId, LambdaTerm)>,
}
