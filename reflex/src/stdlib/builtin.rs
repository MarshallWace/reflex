// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::core::{Arity, Expression};

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
use collect::{Collect, CollectHashMap, CollectHashSet, CollectTuple, CollectVector};
mod concat;
use concat::Concat;
mod cons;
use cons::Cons;
mod divide;
use divide::Divide;
mod effect;
use effect::Effect;
mod ends_with;
use ends_with::EndsWith;
mod entries;
use entries::Entries;
mod eq;
use eq::Eq;
mod equal;
use equal::Equal;
mod filter;
use filter::{CollectFilterResults, Filter};
mod floor;
use floor::Floor;
mod get;
use get::Get;
mod gt;
use gt::Gt;
mod gte;
use gte::Gte;
mod r#if;
use r#if::If;
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
mod r#match;
use r#match::Match;
mod max;
use max::Max;
mod merge;
use merge::Merge;
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
mod push;
use push::Push;
mod push_front;
use push_front::PushFront;
mod reduce;
use reduce::Reduce;
mod remainder;
use remainder::Remainder;
mod round;
use round::Round;
mod slice;
use slice::Slice;
mod starts_with;
use starts_with::StartsWith;
mod r#struct;
use r#struct::Struct;
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

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub enum BuiltinTerm {
    Add,
    Abs,
    And,
    Append,
    Car,
    Cdr,
    Ceil,
    Collect,
    CollectFilterResults,
    CollectHashMap,
    CollectHashSet,
    CollectTuple,
    CollectVector,
    Concat,
    Cons,
    Divide,
    Effect,
    EndsWith,
    Entries,
    Eq,
    Equal,
    Filter,
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
    Merge,
    Min,
    Multiply,
    Not,
    Or,
    Pow,
    Push,
    PushFront,
    Reduce,
    Remainder,
    Round,
    Slice,
    StartsWith,
    Struct,
    Subtract,
    Tuple,
    Values,
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
            Self::CollectFilterResults => CollectFilterResults::arity(),
            Self::CollectHashMap => CollectHashMap::arity(),
            Self::CollectHashSet => CollectHashSet::arity(),
            Self::CollectTuple => CollectTuple::arity(),
            Self::CollectVector => CollectVector::arity(),
            Self::Concat => Concat::arity(),
            Self::Cons => Cons::arity(),
            Self::Divide => Divide::arity(),
            Self::Effect => Effect::arity(),
            Self::EndsWith => EndsWith::arity(),
            Self::Entries => Entries::arity(),
            Self::Eq => Eq::arity(),
            Self::Equal => Equal::arity(),
            Self::Filter => Filter::arity(),
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
            Self::Merge => Merge::arity(),
            Self::Min => Min::arity(),
            Self::Multiply => Multiply::arity(),
            Self::Not => Not::arity(),
            Self::Or => Or::arity(),
            Self::Pow => Pow::arity(),
            Self::Push => Push::arity(),
            Self::PushFront => PushFront::arity(),
            Self::Reduce => Reduce::arity(),
            Self::Remainder => Remainder::arity(),
            Self::Round => Round::arity(),
            Self::Slice => Slice::arity(),
            Self::StartsWith => StartsWith::arity(),
            Self::Struct => Struct::arity(),
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
            Self::CollectFilterResults => CollectFilterResults::apply(args),
            Self::CollectHashMap => CollectHashMap::apply(args),
            Self::CollectHashSet => CollectHashSet::apply(args),
            Self::CollectTuple => CollectTuple::apply(args),
            Self::CollectVector => CollectVector::apply(args),
            Self::Concat => Concat::apply(args),
            Self::Cons => Cons::apply(args),
            Self::Divide => Divide::apply(args),
            Self::Effect => Effect::apply(args),
            Self::EndsWith => EndsWith::apply(args),
            Self::Entries => Entries::apply(args),
            Self::Eq => Eq::apply(args),
            Self::Equal => Equal::apply(args),
            Self::Filter => Filter::apply(args),
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
            Self::Merge => Merge::apply(args),
            Self::Min => Min::apply(args),
            Self::Multiply => Multiply::apply(args),
            Self::Not => Not::apply(args),
            Self::Or => Or::apply(args),
            Self::Pow => Pow::apply(args),
            Self::Push => Push::apply(args),
            Self::PushFront => PushFront::apply(args),
            Self::Reduce => Reduce::apply(args),
            Self::Slice => Slice::apply(args),
            Self::Remainder => Remainder::apply(args),
            Self::Round => Round::apply(args),
            Self::StartsWith => StartsWith::apply(args),
            Self::Struct => Struct::apply(args),
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
