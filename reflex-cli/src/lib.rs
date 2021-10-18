// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use anyhow::anyhow;
use std::str::FromStr;

pub mod compiler;

#[derive(Eq, PartialEq, Debug)]
pub enum Syntax {
    JavaScript,
    ByteCode,
    Lisp,
}

impl FromStr for Syntax {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.to_lowercase() == "javascript" || s == "js" {
            Ok(Self::JavaScript)
        } else if s == "sexpr" || s == "lisp" {
            Ok(Self::Lisp)
        } else if s == "bytecode" {
            Ok(Self::ByteCode)
        } else {
            Err(anyhow!("Unknown syntax {}", s))
        }
    }
}
