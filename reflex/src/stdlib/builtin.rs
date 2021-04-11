// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{Arity, Expression, NativeFunction},
    hash::{hash_u8, HashId, Hashable},
};

mod add;
use add::Add;
mod abs;
use abs::Abs;
mod array;
use array::Array;
mod car;
use car::Car;
mod cdr;
use cdr::Cdr;
mod collect;
use collect::Collect;
mod cons;
use cons::Cons;
mod equal;
use equal::Equal;
mod _if;
use _if::If;
mod get;
use get::Get;
mod map;
use map::Map;
mod _match;
use _match::Match;
mod multiply;
use multiply::Multiply;
pub mod query;
use query::Query;
mod subtract;
use subtract::Subtract;

#[derive(PartialEq, Clone, Debug)]
pub enum BuiltinTerm {
    Add,
    Abs,
    Array,
    Car,
    Cdr,
    Collect,
    Cons,
    Equal,
    Get,
    If,
    Map,
    Match,
    Multiply,
    Query,
    Subtract,
}
impl Hashable for BuiltinTerm {
    fn hash(&self) -> HashId {
        match self {
            Self::Abs => hash_u8(0),
            Self::Add => hash_u8(1),
            Self::Array => hash_u8(2),
            Self::Car => hash_u8(3),
            Self::Cdr => hash_u8(4),
            Self::Collect => hash_u8(5),
            Self::Cons => hash_u8(6),
            Self::Equal => hash_u8(7),
            Self::Get => hash_u8(8),
            Self::If => hash_u8(9),
            Self::Map => hash_u8(10),
            Self::Match => hash_u8(11),
            Self::Multiply => hash_u8(12),
            Self::Query => hash_u8(13),
            Self::Subtract => hash_u8(14),
        }
    }
}
impl BuiltinTerm {
    pub fn arity(&self) -> Arity {
        match self {
            Self::Abs => Abs::arity(),
            Self::Add => Add::arity(),
            Self::Array => Array::arity(),
            Self::Car => Car::arity(),
            Self::Cdr => Cdr::arity(),
            Self::Collect => Collect::arity(),
            Self::Cons => Cons::arity(),
            Self::Equal => Equal::arity(),
            Self::Get => Get::arity(),
            Self::If => If::arity(),
            Self::Map => Map::arity(),
            Self::Match => Match::arity(),
            Self::Multiply => Multiply::arity(),
            Self::Query => Query::arity(),
            Self::Subtract => Subtract::arity(),
        }
    }
    pub fn apply(
        &self,
        args: impl IntoIterator<Item = Expression> + ExactSizeIterator,
    ) -> Expression {
        match self {
            Self::Abs => Abs::apply(args),
            Self::Add => Add::apply(args),
            Self::Array => Array::apply(args),
            Self::Car => Car::apply(args),
            Self::Cdr => Cdr::apply(args),
            Self::Collect => Collect::apply(args),
            Self::Cons => Cons::apply(args),
            Self::Equal => Equal::apply(args),
            Self::Get => Get::apply(args),
            Self::If => If::apply(args),
            Self::Map => Map::apply(args),
            Self::Match => Match::apply(args),
            Self::Multiply => Multiply::apply(args),
            Self::Query => Query::apply(args),
            Self::Subtract => Subtract::apply(args),
        }
    }
}
impl std::fmt::Display for BuiltinTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<builtin:{:?}>", self)
    }
}
