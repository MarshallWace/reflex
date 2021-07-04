// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{collections::HashMap, iter::once};

use either::Either;
use graphql_parser::{parse_query, query::*};
use reflex::{
    core::{Expression, Signal, SignalTerm, StructPrototype, StructTerm, Term},
    serialize::{SerializedListTerm, SerializedObjectTerm, SerializedTerm},
    stdlib::{
        collection::{vector::VectorTerm, CollectionTerm},
        signal::SignalType,
        value::{StringValue, ValueTerm},
    },
};

mod loader;
pub use loader::graphql_loader;
mod operation;
pub use operation::{deserialize_graphql_operation, GraphQlOperationPayload};
mod query;
use query::{query, FieldSelector};
pub use query::{QueryShape, QueryTransform};

pub mod subscriptions;

type QueryVariables<'a> = HashMap<&'a str, Expression>;
type QueryFragments<'a> = HashMap<&'a str, &'a FragmentDefinition<'a, &'a str>>;

pub fn parse<'v>(
    source: &str,
    variables: impl IntoIterator<Item = (&'v str, Expression)>,
    root: &Expression,
) -> Result<(Expression, QueryTransform), String> {
    let variables = variables.into_iter().collect::<HashMap<_, _>>();
    match parse_graphql_query(source, &variables) {
        Ok((shape, transform)) => {
            let expression = query(Expression::clone(root), &shape);
            Ok((expression, transform))
        }
        Err(error) => Err(error),
    }
}

pub fn create_introspection_query_response() -> Expression {
    Expression::new(Term::Signal(SignalTerm::new(Signal::new(
        SignalType::Error,
        vec![Expression::new(Term::Value(ValueTerm::String(
            StringValue::from("Introspection query not yet implemented"),
        )))],
    ))))
}

const DEFAULT_OPERATION_TYPE: &str = "query";

fn parse_graphql_query(
    source: &str,
    variables: &QueryVariables,
) -> Result<(QueryShape, QueryTransform), String> {
    match parse_query::<&str>(source) {
        Ok(document) => {
            let fragments = document
                .definitions
                .iter()
                .filter_map(|definition| match definition {
                    Definition::Fragment(fragment) => Some((fragment.name, fragment)),
                    _ => None,
                })
                .collect::<QueryFragments>();
            match get_root_operation(&document) {
                Err(error) => Err(error),
                Ok(root) => match root {
                    OperationDefinition::SelectionSet(selection_set) => parse_root_operation(
                        DEFAULT_OPERATION_TYPE,
                        selection_set,
                        variables,
                        &fragments,
                    ),
                    _ => parse_operation(root, variables, &fragments),
                },
            }
        }
        Err(error) => Err(format!("{}", error)),
    }
}

pub fn wrap_graphql_success_response(value: SerializedTerm) -> SerializedTerm {
    SerializedTerm::Object(SerializedObjectTerm::new(once((
        String::from("data"),
        value,
    ))))
}

pub fn wrap_graphql_error_response(errors: Vec<String>) -> SerializedTerm {
    SerializedTerm::Object(SerializedObjectTerm::new(once((
        String::from("errors"),
        SerializedTerm::List(SerializedListTerm::new(
            errors
                .into_iter()
                .map(|error| SerializedTerm::Value(ValueTerm::String(error))),
        )),
    ))))
}

fn get_root_operation<'src, 'a>(
    document: &'a Document<'src, &'src str>,
) -> Result<&'a OperationDefinition<'src, &'src str>, String> {
    let operations = document
        .definitions
        .iter()
        .filter_map(|definition| match definition {
            Definition::Fragment(_) => None,
            Definition::Operation(definition) => Some(definition),
        })
        .collect::<Vec<_>>();
    match operations.len() {
        0 => Err(String::from("Missing root operation")),
        1 => Ok(operations.into_iter().next().unwrap()),
        _ => Err(String::from("Multiple root operations")),
    }
}

fn parse_operation<'src>(
    operation: &OperationDefinition<'src, &'src str>,
    variables: &QueryVariables,
    fragments: &QueryFragments,
) -> Result<(QueryShape, QueryTransform), String> {
    match operation {
        OperationDefinition::Query(operation) => {
            parse_query_operation(operation, variables, fragments)
        }
        OperationDefinition::Mutation(operation) => {
            parse_mutation_operation(operation, variables, fragments)
        }
        OperationDefinition::Subscription(operation) => {
            parse_subscription_operation(operation, variables, fragments)
        }
        OperationDefinition::SelectionSet(selection) => {
            parse_selection_set(selection, variables, fragments)
        }
    }
}

fn parse_query_operation<'src>(
    operation: &Query<'src, &'src str>,
    variables: &QueryVariables,
    fragments: &QueryFragments,
) -> Result<(QueryShape, QueryTransform), String> {
    parse_root_operation("query", &operation.selection_set, variables, fragments)
}

fn parse_mutation_operation<'src>(
    operation: &Mutation<'src, &'src str>,
    variables: &QueryVariables,
    fragments: &QueryFragments,
) -> Result<(QueryShape, QueryTransform), String> {
    parse_root_operation("mutation", &operation.selection_set, variables, fragments)
}

fn parse_subscription_operation<'src>(
    operation: &Subscription<'src, &'src str>,
    variables: &QueryVariables,
    fragments: &QueryFragments,
) -> Result<(QueryShape, QueryTransform), String> {
    parse_root_operation(
        "subscription",
        &operation.selection_set,
        variables,
        fragments,
    )
}

fn parse_root_operation<'src>(
    operation_type: &str,
    selection_set: &SelectionSet<'src, &'src str>,
    variables: &QueryVariables,
    fragments: &QueryFragments,
) -> Result<(QueryShape, QueryTransform), String> {
    let (inner_query, inner_transform) = parse_selection_set(selection_set, variables, fragments)?;
    let query = QueryShape::branch(vec![FieldSelector::NamedField(
        ValueTerm::String(StringValue::from(operation_type)),
        inner_query,
    )]);
    let transform = Box::new(move |result: &SerializedTerm| match result {
        SerializedTerm::List(value) if value.items().len() == 1 => {
            inner_transform(value.items().iter().next().unwrap())
        }
        _ => Err(format!("Invalid root operation result: {}", result)),
    });
    Ok((query, transform))
}

fn parse_selection_set<'src>(
    selection_set: &SelectionSet<'src, &'src str>,
    variables: &QueryVariables,
    fragments: &QueryFragments,
) -> Result<(QueryShape, QueryTransform), String> {
    let fields = parse_selection_set_fields(selection_set, variables, fragments)?;
    let (field_queries, field_transforms): (Vec<_>, Vec<_>) = fields.into_iter().unzip();
    let query = QueryShape::branch(field_queries);
    let transform = Box::new(move |result: &SerializedTerm| match result {
        SerializedTerm::List(value) => {
            let transformed_fields = value
                .items()
                .iter()
                .zip(field_transforms.iter())
                .map(|(value, (key, transform))| match transform(value) {
                    Err(error) => Err(error),
                    Ok(value) => Ok((String::from(key), value)),
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(create_struct(transformed_fields))
        }
        _ => Err(format!("Invalid branch result: {}", result)),
    });
    Ok((query, transform))
}

fn parse_selection_set_fields<'src>(
    selection_set: &SelectionSet<'src, &'src str>,
    variables: &QueryVariables,
    fragments: &QueryFragments,
) -> Result<Vec<(FieldSelector, (String, QueryTransform))>, String> {
    selection_set
        .items
        .iter()
        .flat_map(|field| match field {
            Selection::Field(field) => match parse_field(field, variables, fragments) {
                Ok((query, transform)) => {
                    let key = field.alias.unwrap_or(field.name);
                    Either::Left(once(Ok((query, (String::from(key), transform)))))
                }
                Err(error) => Either::Left(once(Err(error))),
            },
            Selection::FragmentSpread(fragment) => match fragments.get(fragment.fragment_name) {
                Some(fragment) => {
                    Either::Right(parse_fragment_fields(fragment, variables, fragments).into_iter())
                }
                None => Either::Left(once(Err(format!(
                    "Invalid fragment name: {}",
                    fragment.fragment_name
                )))),
            },
            Selection::InlineFragment(_) => Either::Left(once(Err(String::from(
                "Inline fragments not yet implemented",
            )))),
        })
        .collect::<Result<Vec<_>, _>>()
}

fn parse_fragment_fields<'src>(
    fragment: &FragmentDefinition<'src, &'src str>,
    variables: &QueryVariables,
    fragments: &QueryFragments,
) -> impl IntoIterator<Item = Result<(FieldSelector, (String, QueryTransform)), String>> {
    match parse_selection_set_fields(&fragment.selection_set, variables, fragments) {
        Err(error) => Either::Left(once(Err(error))),
        Ok(fields) => Either::Right(fields.into_iter().map(Ok)),
    }
}

fn parse_field<'src>(
    field: &Field<'src, &'src str>,
    variables: &QueryVariables,
    fragments: &QueryFragments,
) -> Result<(FieldSelector, QueryTransform), String> {
    let (query, transform): (QueryShape, QueryTransform) = if field.selection_set.items.is_empty() {
        (
            QueryShape::leaf(),
            Box::new(|result| Ok(result.deserialize())),
        )
    } else {
        parse_selection_set(&field.selection_set, variables, fragments)?
    };
    let list_directives = field
        .directives
        .iter()
        .filter(|directive| directive.name == "list");
    let (query, transform) = list_directives.fold((query, transform), |(query, transform), _| {
        let query = QueryShape::list(query);
        let transform = Box::new(move |result: &SerializedTerm| match result {
            SerializedTerm::List(value) => {
                let transformed_items = value
                    .items()
                    .iter()
                    .map(|value| transform(value))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Expression::new(Term::Collection(CollectionTerm::Vector(
                    VectorTerm::new(transformed_items),
                ))))
            }
            _ => Err(format!("Invalid list result: {}", result)),
        });
        (query, transform)
    });
    let field_name = ValueTerm::String(StringValue::from(field.name));
    let (query, transform): (QueryShape, QueryTransform) = if field.arguments.is_empty() {
        (query, transform)
    } else {
        let args = parse_field_arguments(&field.arguments, variables, fragments)?;
        let query = QueryShape::branch(vec![FieldSelector::FunctionField(args, query)]);
        let transform = Box::new(move |result: &SerializedTerm| match result {
            SerializedTerm::List(value) if value.items().len() == 1 => {
                transform(value.items().iter().next().unwrap())
            }
            _ => Err(format!("Invalid function result: {}", result)),
        });
        (query, transform)
    };
    Ok((FieldSelector::NamedField(field_name, query), transform))
}

fn parse_field_arguments<'src>(
    args: &Vec<(&'src str, Value<'src, &'src str>)>,
    variables: &QueryVariables,
    fragments: &QueryFragments,
) -> Result<Vec<Expression>, String> {
    let arg_fields = args
        .iter()
        .map(
            |(key, value)| match parse_value(value, variables, fragments) {
                Ok(value) => Ok((String::from(*key), value)),
                Err(error) => Err(error),
            },
        )
        .collect::<Result<Vec<_>, _>>()?;
    let arg = create_struct(arg_fields);
    Ok(vec![arg])
}

fn parse_value<'src>(
    value: &Value<'src, &'src str>,
    variables: &QueryVariables,
    fragments: &QueryFragments,
) -> Result<Expression, String> {
    match value {
        Value::Variable(name) => match variables.get(*name) {
            Some(value) => Ok(Expression::clone(value)),
            None => Err(format!("Missing query variable: {}", name)),
        },
        Value::Int(value) => Ok(Expression::new(Term::Value(ValueTerm::Float(
            value
                .as_i64()
                .ok_or_else(|| format!("Invalid integer argument: {:?}", value))?
                as f64,
        )))),
        Value::Float(value) => Ok(Expression::new(Term::Value(ValueTerm::Float(*value)))),
        Value::String(value) => Ok(Expression::new(Term::Value(ValueTerm::String(
            StringValue::from(value),
        )))),
        Value::Boolean(value) => Ok(Expression::new(Term::Value(ValueTerm::Boolean(*value)))),
        Value::Null => Ok(Expression::new(Term::Value(ValueTerm::Null))),
        Value::Enum(value) => Ok(Expression::new(Term::Value(ValueTerm::String(
            StringValue::from(*value),
        )))),
        Value::List(value) => {
            let values = value
                .iter()
                .map(|item| parse_value(item, variables, fragments))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Expression::new(Term::Collection(CollectionTerm::Vector(
                VectorTerm::new(values),
            ))))
        }
        Value::Object(value) => {
            let fields = value
                .iter()
                .map(
                    |(key, value)| match parse_value(value, variables, fragments) {
                        Ok(value) => Ok((StringValue::from(*key), value)),
                        Err(error) => Err(error),
                    },
                )
                .collect::<Result<Vec<_>, _>>()?;
            Ok(create_struct(fields))
        }
    }
}

fn create_struct(fields: impl IntoIterator<Item = (String, Expression)>) -> Expression {
    let (keys, values) = fields
        .into_iter()
        .map(|(key, value)| (ValueTerm::String(key), value))
        .unzip();
    Expression::new(Term::Struct(StructTerm::new(
        Some(StructPrototype::new(keys)),
        values,
    )))
}
