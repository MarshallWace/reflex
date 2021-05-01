// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{Arity, Expression},
    hash::{hash_u8, HashId, Hashable},
};

mod add;
use add::Add;
mod abs;
use abs::Abs;
mod and;
use and::And;
mod append;
use append::Append;
mod car;
use car::Car;
mod cdr;
use cdr::Cdr;
mod ceil;
use ceil::Ceil;
mod collect;
use collect::{Collect, CollectArgs};
mod concat;
use concat::Concat;
mod cons;
use cons::Cons;
mod divide;
use divide::Divide;
mod entries;
use entries::Entries;
mod eq;
use eq::Eq;
mod equal;
use equal::Equal;
mod floor;
use floor::Floor;
mod get;
use get::Get;
mod gt;
use gt::Gt;
mod gte;
use gte::Gte;
mod _if;
use _if::If;
mod insert;
use insert::Insert;
mod keys;
use keys::Keys;
mod lt;
use lt::Lt;
mod lte;
use lte::Lte;
mod map;
use map::Map;
mod _match;
use _match::Match;
mod max;
use max::Max;
mod min;
use min::Min;
mod multiply;
use multiply::Multiply;
mod not;
use not::Not;
mod or;
use or::Or;
mod pow;
use pow::Pow;
mod remainder;
use remainder::Remainder;
mod round;
use round::Round;
mod subtract;
use subtract::Subtract;
mod tuple;
use tuple::Tuple;
mod values;
use values::Values;

pub trait BuiltinFunction {
    fn arity() -> Arity;
    fn apply(args: impl IntoIterator<Item = Expression> + ExactSizeIterator) -> Expression;
}

#[derive(PartialEq, Clone, Debug)]
pub enum BuiltinTerm {
    Add,
    Abs,
    And,
    Append,
    Car,
    Cdr,
    Ceil,
    Collect,
    CollectArgs,
    Concat,
    Cons,
    Divide,
    Entries,
    Eq,
    Equal,
    Floor,
    Get,
    Gt,
    Gte,
    If,
    Insert,
    Keys,
    Lt,
    Lte,
    Map,
    Match,
    Max,
    Min,
    Multiply,
    Not,
    Or,
    Pow,
    Remainder,
    Round,
    Subtract,
    Tuple,
    Values,
}
impl Hashable for BuiltinTerm {
    fn hash(&self) -> HashId {
        match self {
            Self::Abs => hash_u8(0),
            Self::Add => hash_u8(1),
            Self::And => hash_u8(2),
            Self::Append => hash_u8(3),
            Self::Car => hash_u8(4),
            Self::Cdr => hash_u8(5),
            Self::Ceil => hash_u8(6),
            Self::Collect => hash_u8(7),
            Self::CollectArgs => hash_u8(8),
            Self::Concat => hash_u8(9),
            Self::Cons => hash_u8(10),
            Self::Divide => hash_u8(11),
            Self::Entries => hash_u8(12),
            Self::Eq => hash_u8(13),
            Self::Equal => hash_u8(14),
            Self::Floor => hash_u8(15),
            Self::Get => hash_u8(16),
            Self::Gt => hash_u8(17),
            Self::Gte => hash_u8(18),
            Self::If => hash_u8(19),
            Self::Insert => hash_u8(20),
            Self::Keys => hash_u8(21),
            Self::Lt => hash_u8(22),
            Self::Lte => hash_u8(23),
            Self::Map => hash_u8(24),
            Self::Match => hash_u8(25),
            Self::Max => hash_u8(26),
            Self::Min => hash_u8(27),
            Self::Multiply => hash_u8(28),
            Self::Not => hash_u8(29),
            Self::Or => hash_u8(30),
            Self::Pow => hash_u8(31),
            Self::Remainder => hash_u8(32),
            Self::Round => hash_u8(33),
            Self::Subtract => hash_u8(34),
            Self::Tuple => hash_u8(35),
            Self::Values => hash_u8(36),
        }
    }
}
impl BuiltinTerm {
    pub fn arity(&self) -> Arity {
        match self {
            Self::Abs => Abs::arity(),
            Self::Add => Add::arity(),
            Self::And => And::arity(),
            Self::Append => Append::arity(),
            Self::Car => Car::arity(),
            Self::Cdr => Cdr::arity(),
            Self::Ceil => Ceil::arity(),
            Self::Collect => Collect::arity(),
            Self::CollectArgs => CollectArgs::arity(),
            Self::Concat => Concat::arity(),
            Self::Cons => Cons::arity(),
            Self::Divide => Divide::arity(),
            Self::Entries => Entries::arity(),
            Self::Eq => Eq::arity(),
            Self::Equal => Equal::arity(),
            Self::Floor => Floor::arity(),
            Self::Get => Get::arity(),
            Self::Gt => Gt::arity(),
            Self::Gte => Gte::arity(),
            Self::If => If::arity(),
            Self::Insert => Insert::arity(),
            Self::Keys => Keys::arity(),
            Self::Lt => Lt::arity(),
            Self::Lte => Lte::arity(),
            Self::Map => Map::arity(),
            Self::Match => Match::arity(),
            Self::Max => Max::arity(),
            Self::Min => Min::arity(),
            Self::Multiply => Multiply::arity(),
            Self::Not => Not::arity(),
            Self::Or => Or::arity(),
            Self::Pow => Pow::arity(),
            Self::Remainder => Remainder::arity(),
            Self::Round => Round::arity(),
            Self::Subtract => Subtract::arity(),
            Self::Tuple => Tuple::arity(),
            Self::Values => Values::arity(),
        }
    }
    pub fn apply(
        &self,
        args: impl IntoIterator<Item = Expression> + ExactSizeIterator,
    ) -> Expression {
        match self {
            Self::Abs => Abs::apply(args),
            Self::Add => Add::apply(args),
            Self::And => And::apply(args),
            Self::Append => Append::apply(args),
            Self::Car => Car::apply(args),
            Self::Cdr => Cdr::apply(args),
            Self::Ceil => Ceil::apply(args),
            Self::Collect => Collect::apply(args),
            Self::CollectArgs => CollectArgs::apply(args),
            Self::Concat => Concat::apply(args),
            Self::Cons => Cons::apply(args),
            Self::Divide => Divide::apply(args),
            Self::Entries => Entries::apply(args),
            Self::Eq => Eq::apply(args),
            Self::Equal => Equal::apply(args),
            Self::Floor => Floor::apply(args),
            Self::Get => Get::apply(args),
            Self::Gt => Gt::apply(args),
            Self::Gte => Gte::apply(args),
            Self::If => If::apply(args),
            Self::Insert => Insert::apply(args),
            Self::Keys => Keys::apply(args),
            Self::Lt => Lt::apply(args),
            Self::Lte => Lte::apply(args),
            Self::Map => Map::apply(args),
            Self::Match => Match::apply(args),
            Self::Max => Max::apply(args),
            Self::Min => Min::apply(args),
            Self::Multiply => Multiply::apply(args),
            Self::Not => Not::apply(args),
            Self::Or => Or::apply(args),
            Self::Pow => Pow::apply(args),
            Self::Remainder => Remainder::apply(args),
            Self::Round => Round::apply(args),
            Self::Subtract => Subtract::apply(args),
            Self::Tuple => Tuple::apply(args),
            Self::Values => Values::apply(args),
        }
    }
}
impl std::fmt::Display for BuiltinTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<builtin:{:?}>", self)
    }
}
