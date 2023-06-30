// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use crate::stdlib::{
    Accessor, DateConstructor, EncodeUriComponent, FormatErrorMessage, IsFinite, ParseFloat,
    ParseInt, ToString,
};
use reflex::core::{Builtin, Expression, ExpressionFactory, HeapAllocator};
use reflex_json::stdlib::{JsonDeserialize, JsonSerialize};
use reflex_stdlib::{
    Abs, Apply, Ceil, CollectHashMap, CollectHashSet, ConstructRecord, Floor, Get, If, Keys, Map,
    Max, Min, ResolveDeep, ResolveHashMap, ResolveList, Round, Unzip, Values, Zip,
};

pub(crate) mod boolean;
pub(crate) mod date;
pub(crate) mod error;
pub(crate) mod json;
pub(crate) mod map;
pub(crate) mod math;
pub(crate) mod object;
pub(crate) mod process;
pub(crate) mod set;
pub(crate) mod string;

pub use self::boolean::global_boolean;
pub use self::date::global_date;
pub use self::error::{global_aggregate_error, global_error};
pub use self::json::global_json;
pub use self::map::global_map;
pub use self::math::global_math;
pub use self::object::global_object;
pub use self::process::global_process;
pub use self::set::global_set;
pub use self::string::global_string;

pub trait JsGlobalsBuiltin:
    Builtin
    + From<Abs>
    + From<Accessor>
    + From<Apply>
    + From<Ceil>
    + From<CollectHashMap>
    + From<CollectHashSet>
    + From<ConstructRecord>
    + From<DateConstructor>
    + From<EncodeUriComponent>
    + From<Floor>
    + From<FormatErrorMessage>
    + From<Get>
    + From<If>
    + From<IsFinite>
    + From<JsonDeserialize>
    + From<JsonSerialize>
    + From<Keys>
    + From<Map>
    + From<Max>
    + From<Min>
    + From<ParseFloat>
    + From<ParseInt>
    + From<ResolveDeep>
    + From<ResolveHashMap>
    + From<ResolveList>
    + From<Round>
    + From<ToString>
    + From<Unzip>
    + From<Values>
    + From<Zip>
{
}
impl<T> JsGlobalsBuiltin for T where
    T: Builtin
        + From<Abs>
        + From<Accessor>
        + From<Apply>
        + From<Ceil>
        + From<CollectHashMap>
        + From<CollectHashSet>
        + From<ConstructRecord>
        + From<DateConstructor>
        + From<EncodeUriComponent>
        + From<Floor>
        + From<FormatErrorMessage>
        + From<Get>
        + From<If>
        + From<IsFinite>
        + From<JsonDeserialize>
        + From<JsonSerialize>
        + From<Keys>
        + From<Map>
        + From<Max>
        + From<Min>
        + From<ParseFloat>
        + From<ParseInt>
        + From<ResolveDeep>
        + From<ResolveHashMap>
        + From<ResolveList>
        + From<Round>
        + From<ToString>
        + From<Unzip>
        + From<Values>
        + From<Zip>
{
}

pub fn builtin_globals<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Vec<(&'static str, T)>
where
    T::Builtin: JsGlobalsBuiltin,
{
    vec![
        ("Boolean", global_boolean(factory, allocator)),
        ("String", global_string(factory, allocator)),
        ("Object", global_object(factory, allocator)),
        ("Error", global_error(factory, allocator)),
        ("AggregateError", global_aggregate_error(factory, allocator)),
        ("Math", global_math(factory, allocator)),
        ("Map", global_map(factory, allocator)),
        ("Set", global_set(factory, allocator)),
        ("Date", global_date(factory, allocator)),
        ("JSON", global_json(factory, allocator)),
        ("isFinite", factory.create_builtin_term(IsFinite)),
        (
            "encodeURIComponent",
            factory.create_builtin_term(EncodeUriComponent),
        ),
        ("parseFloat", factory.create_builtin_term(ParseFloat)),
        ("parseInt", factory.create_builtin_term(ParseInt)),
        ("process", global_process(factory, allocator)),
    ]
}
