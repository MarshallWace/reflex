// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::borrow::Cow;

use crate::{
    core::{StructTerm, Term},
    stdlib::{
        collection::{vector::VectorTerm, CollectionTerm},
        value::ValueTerm,
    },
};

pub fn serialize(term: &Term) -> Result<Cow<ValueTerm>, &Term> {
    match term {
        Term::Value(value) => Ok(Cow::Borrowed(value)),
        Term::Struct(value) => serialize_struct_term(value),
        Term::Collection(value) => match value {
            CollectionTerm::Vector(value) => serialize_vector_term(value),
            _ => Err(term),
        },
        _ => Err(term),
    }
}

fn serialize_struct_term(value: &StructTerm) -> Result<Cow<ValueTerm>, &Term> {
    match value.prototype() {
        None => {
            let items = value
                .fields()
                .iter()
                .map(|value| {
                    let serialized_value = serialize(value.value())?;
                    Ok(match serialized_value {
                        Cow::Owned(value) => value,
                        Cow::Borrowed(value) => value.clone(),
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Cow::Owned(ValueTerm::Array(items)))
        }
        Some(prototype) => {
            let entries = prototype
                .keys()
                .iter()
                .zip(value.fields())
                .map(|(key, value)| {
                    let serialized_value = serialize(value.value())?;
                    Ok(ValueTerm::Array(vec![
                        key.clone(),
                        match serialized_value {
                            Cow::Owned(value) => value,
                            Cow::Borrowed(value) => value.clone(),
                        },
                    ]))
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Cow::Owned(ValueTerm::Array(entries)))
        }
    }
}

fn serialize_vector_term(value: &VectorTerm) -> Result<Cow<ValueTerm>, &Term> {
    let items = value
        .items()
        .iter()
        .map(|item| {
            let serialized_item = serialize(item.value())?;
            Ok(match serialized_item {
                Cow::Owned(item) => item,
                Cow::Borrowed(item) => item.clone(),
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(Cow::Owned(ValueTerm::Array(items)))
}
