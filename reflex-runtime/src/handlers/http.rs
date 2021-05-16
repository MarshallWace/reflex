// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::{
    core::{Expression, Signal, SignalTerm, Term},
    stdlib::{signal::SignalType, value::ValueTerm},
};

use crate::{utils::fetch, SignalResult};

pub fn handle_http_fetch(args: Option<&[ValueTerm]>) -> Result<SignalResult, String> {
    let args = args.ok_or_else(|| String::from("Invalid fetch signal: missing arguments"))?;
    if args.len() != 4 {
        return Err(format!(
            "Invalid fetch signal: Expected 4 arguments, received {}",
            args.len()
        ));
    }
    let mut args = args.into_iter();
    let url = parse_string_arg(args.next().unwrap());
    let method = parse_string_arg(args.next().unwrap());
    let headers = parse_key_values_arg(args.next().unwrap());
    let body = parse_optional_string_arg(args.next().unwrap());
    match (method, url, headers, body) {
        (Some(method), Some(url), Some(headers), Some(body)) => Ok((
            Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Pending,
                Vec::new(),
            )))),
            Some(Box::pin(async move {
                match fetch(method, url, headers, body).await {
                    Ok(data) => Expression::new(Term::Value(ValueTerm::String(data))),
                    Err(error) => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                        SignalType::Error,
                        vec![ValueTerm::String(error)],
                    )))),
                }
            })),
        )),
        _ => Err(String::from("Invalid fetch signal arguments")),
    }
}

fn parse_string_arg(value: &ValueTerm) -> Option<String> {
    match value {
        ValueTerm::String(value) => Some(String::from(value)),
        _ => None,
    }
}

fn parse_optional_string_arg(value: &ValueTerm) -> Option<Option<String>> {
    match value {
        ValueTerm::String(value) => Some(Some(String::from(value))),
        ValueTerm::Null => Some(None),
        _ => None,
    }
}

fn parse_key_values_arg(value: &ValueTerm) -> Option<Vec<(String, String)>> {
    match value {
        ValueTerm::Array(values) => values
            .into_iter()
            .map(|entry| match entry {
                ValueTerm::Array(entry) => {
                    let mut entry = entry.iter();
                    let key = entry.next();
                    let value = entry.next();
                    match (key, value) {
                        (Some(ValueTerm::String(key)), Some(ValueTerm::String(value))) => {
                            Some((String::from(key), String::from(value)))
                        }
                        _ => None,
                    }
                }
                _ => None,
            })
            .collect::<Option<Vec<_>>>(),
        _ => None,
    }
}
