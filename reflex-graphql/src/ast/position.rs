// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use serde::{Deserialize, Serialize};

#[derive(
    Debug, PartialOrd, Ord, PartialEq, Eq, Clone, Copy, Default, Hash, Serialize, Deserialize,
)]
pub struct Pos {
    pub line: usize,
    pub column: usize,
}
impl<'a> From<&'a graphql_parser::Pos> for Pos {
    fn from(value: &'a graphql_parser::Pos) -> Self {
        let graphql_parser::Pos { line, column } = value;
        Self {
            line: *line,
            column: *column,
        }
    }
}
impl From<Pos> for graphql_parser::Pos {
    fn from(value: Pos) -> Self {
        let Pos { line, column } = value;
        Self { line, column }
    }
}
