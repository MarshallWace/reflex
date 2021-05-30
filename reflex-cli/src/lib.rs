// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{collections::HashMap, iter::FromIterator};

pub fn parse_cli_args(args: impl IntoIterator<Item = String>) -> Args {
    let (current, mut args) =
        args.into_iter()
            .fold((None, Vec::new()), |(current, mut args), arg| {
                match arg.strip_prefix("--") {
                    Some(name) => match current {
                        Some(previous_name) => {
                            args.push(ParsedArg::Named(previous_name, None));
                            (Some(arg), args)
                        }
                        None => (Some(String::from(name)), args),
                    },
                    None => match current {
                        Some(name) => {
                            args.push(ParsedArg::Named(name, Some(arg)));
                            (None, args)
                        }
                        None => {
                            args.push(ParsedArg::Positional(arg));
                            (None, args)
                        }
                    },
                }
            });
    let args = match current {
        Some(name) => {
            args.push(ParsedArg::Named(name, None));
            args
        }
        None => args,
    };
    Args::from_iter(args)
}

enum ParsedArg {
    Named(String, Option<String>),
    Positional(String),
}

pub struct Args {
    named: HashMap<String, Option<String>>,
    positional: Vec<String>,
}
impl Args {
    pub fn get<'a>(&'a self, name: &str) -> Option<Option<&'a str>> {
        self.named.get(name).map(|value| match value {
            Some(value) => Some(value.as_str()),
            None => None,
        })
    }
    pub fn len(&self) -> usize {
        self.positional.len()
    }
    pub fn iter(&self) -> impl Iterator<Item = &str> {
        self.positional.iter().map(|arg| arg.as_str())
    }
}
impl FromIterator<ParsedArg> for Args {
    fn from_iter<I: IntoIterator<Item = ParsedArg>>(iter: I) -> Self {
        let (named, positional) = iter.into_iter().fold(
            (HashMap::new(), Vec::new()),
            |(mut named, mut positional), arg| match arg {
                ParsedArg::Named(name, value) => {
                    named.insert(name, value);
                    (named, positional)
                }
                ParsedArg::Positional(value) => {
                    positional.push(value);
                    (named, positional)
                }
            },
        );
        Self { named, positional }
    }
}
impl IntoIterator for Args {
    type Item = String;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.positional.into_iter()
    }
}
