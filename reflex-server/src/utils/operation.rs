// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex_graphql::{
    ast::query::{Definition, OperationDefinition},
    GraphQlOperation, GraphQlQuery,
};

pub fn format_graphql_operation_label(operation: &GraphQlOperation) -> String {
    let operation_name = match operation.operation_name() {
        Some(operation_name) => operation_name,
        None => "<anonymous>",
    };
    format!(
        "{} {}",
        get_query_operation_type(operation.query()),
        operation_name,
    )
}

#[derive(Debug)]
enum GraphQlOperationType {
    Query,
    Mutation,
    Subscription,
}
impl std::fmt::Display for GraphQlOperationType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Query => write!(f, "query"),
            Self::Mutation => write!(f, "mutation"),
            Self::Subscription => write!(f, "subscription"),
        }
    }
}

fn get_query_operation_type(operation: &GraphQlQuery) -> GraphQlOperationType {
    operation
        .definitions
        .iter()
        .find_map(|definition| match definition {
            Definition::Operation(OperationDefinition::Query(_)) => {
                Some(GraphQlOperationType::Query)
            }
            Definition::Operation(OperationDefinition::Mutation(_)) => {
                Some(GraphQlOperationType::Mutation)
            }
            Definition::Operation(OperationDefinition::Subscription(_)) => {
                Some(GraphQlOperationType::Subscription)
            }
            Definition::Operation(OperationDefinition::SelectionSet(_)) => {
                Some(GraphQlOperationType::Query)
            }
            _ => None,
        })
        .unwrap_or(GraphQlOperationType::Query)
}
