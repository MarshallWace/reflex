// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use http::StatusCode;
use reflex_graphql::{GraphQlOperationPayload, GraphQlQuery, GraphQlQueryTransform};

pub fn apply_graphql_query_transform(
    operation: GraphQlOperationPayload,
    transform: &impl GraphQlQueryTransform,
) -> Result<GraphQlOperationPayload, (StatusCode, String)> {
    let (query, operation_name, variables, extensions) = operation.into_parts();
    let (query, extensions) = query
        .into_ast()
        .map_err(|err| format!("{}", err))
        .and_then(|query| transform.transform(query, extensions))
        .map_err(|err| {
            (
                StatusCode::BAD_REQUEST,
                format!("GraphQL query transformation failed: {}", err),
            )
        })?;
    Ok(GraphQlOperationPayload::new(
        GraphQlQuery::Ast(query),
        operation_name,
        variables,
        extensions,
    ))
}
