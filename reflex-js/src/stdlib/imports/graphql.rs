// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::{
    core::{Expression, StructPrototype, Term},
    stdlib::value::{StringValue, ValueTerm},
};

use crate::stdlib::imports::create_struct;

pub(crate) fn import_graphql() -> Expression {
    create_struct(vec![("Resolver", import_graphql_resolver())])
}

fn import_graphql_resolver() -> Expression {
    Expression::new(Term::StructConstructor(StructPrototype::new(vec![
        ValueTerm::String(StringValue::from("query")),
        ValueTerm::String(StringValue::from("mutation")),
        ValueTerm::String(StringValue::from("subscription")),
    ])))
}
