// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use http::StatusCode;
use reflex_graphql::{GraphQlOperation, GraphQlQueryTransform};

pub fn apply_graphql_query_transform(
    operation: GraphQlOperation,
    transform: &impl GraphQlQueryTransform,
) -> Result<GraphQlOperation, (StatusCode, String)> {
    let (query, operation_name, variables, extensions) = operation.into_parts();
    let (query, extensions) = transform.transform(query, extensions).map_err(|err| {
        (
            StatusCode::BAD_REQUEST,
            format!("GraphQL query transformation failed: {}", err),
        )
    })?;
    Ok(GraphQlOperation::new(
        query,
        operation_name,
        variables,
        extensions,
    ))
}
