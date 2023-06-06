// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{borrow::Cow, collections::HashMap, iter::once};

use reflex::core::{
    create_record, Builtin, ConditionListType, ConditionType, Expression, ExpressionFactory,
    HeapAllocator, RefType, SignalTermType, SignalType,
};
use reflex_json::{sanitize, JsonMap, JsonValue};
use reflex_stdlib::{CollectRecord, Get};
use reflex_utils::json::json_object;
use serde::{Deserialize, Serialize};
use stdlib::{DynamicQueryBranch, FlattenDeep};

use crate::ast::{
    common::{Type, Value},
    query::*,
};

pub use graphql_parser;

pub mod ast;
mod operation;
pub use operation::{graphql_variables_are_equal, GraphQlOperation};

pub mod stdlib;
pub mod subscriptions;
pub mod transform;
pub mod validate;

pub trait GraphQlParserBuiltin:
    Builtin + From<CollectRecord> + From<DynamicQueryBranch> + From<FlattenDeep> + From<Get>
{
}
impl<T> GraphQlParserBuiltin for T where
    T: Builtin + From<CollectRecord> + From<DynamicQueryBranch> + From<FlattenDeep> + From<Get>
{
}

#[allow(type_alias_bounds)]
type QueryFragments<'a> = HashMap<String, &'a FragmentDefinition>;

pub trait GraphQlQueryTransform {
    fn transform(
        &self,
        query: GraphQlQuery,
        variables: GraphQlVariables,
        extensions: GraphQlExtensions,
    ) -> Result<(GraphQlQuery, GraphQlVariables, GraphQlExtensions), String>;
}

#[derive(Clone, Copy)]
pub struct NoopGraphQlQueryTransform;
impl GraphQlQueryTransform for NoopGraphQlQueryTransform {
    fn transform(
        &self,
        query: GraphQlQuery,
        variables: GraphQlVariables,
        extensions: GraphQlExtensions,
    ) -> Result<(GraphQlQuery, GraphQlVariables, GraphQlExtensions), String> {
        Ok((query, variables, extensions))
    }
}

impl<TInner: GraphQlQueryTransform> GraphQlQueryTransform for Option<TInner> {
    fn transform(
        &self,
        query: GraphQlQuery,
        variables: GraphQlVariables,
        extensions: GraphQlExtensions,
    ) -> Result<(GraphQlQuery, GraphQlVariables, GraphQlExtensions), String> {
        match self {
            Some(inner) => inner.transform(query, variables, extensions),
            None => Ok((query, variables, extensions)),
        }
    }
}

pub enum EitherGraphQlQueryTransform<T1: GraphQlQueryTransform, T2: GraphQlQueryTransform> {
    Left(T1),
    Right(T2),
}
impl<T1, T2> Clone for EitherGraphQlQueryTransform<T1, T2>
where
    T1: GraphQlQueryTransform + Clone,
    T2: GraphQlQueryTransform + Clone,
{
    fn clone(&self) -> Self {
        match self {
            Self::Left(inner) => Self::Left(inner.clone()),
            Self::Right(inner) => Self::Right(inner.clone()),
        }
    }
}
impl<T1: GraphQlQueryTransform, T2: GraphQlQueryTransform> GraphQlQueryTransform
    for EitherGraphQlQueryTransform<T1, T2>
{
    fn transform(
        &self,
        query: GraphQlQuery,
        variables: GraphQlVariables,
        extensions: GraphQlExtensions,
    ) -> Result<(GraphQlQuery, GraphQlVariables, GraphQlExtensions), String> {
        match self {
            Self::Left(inner) => inner.transform(query, variables, extensions),
            Self::Right(inner) => inner.transform(query, variables, extensions),
        }
    }
}

pub struct ChainedGraphQlQueryTransform<T1: GraphQlQueryTransform, T2: GraphQlQueryTransform> {
    left: T1,
    right: T2,
}
impl<T1, T2> Clone for ChainedGraphQlQueryTransform<T1, T2>
where
    T1: GraphQlQueryTransform + Clone,
    T2: GraphQlQueryTransform + Clone,
{
    fn clone(&self) -> Self {
        Self {
            left: self.left.clone(),
            right: self.right.clone(),
        }
    }
}
impl<T1: GraphQlQueryTransform, T2: GraphQlQueryTransform> GraphQlQueryTransform
    for ChainedGraphQlQueryTransform<T1, T2>
{
    fn transform(
        &self,
        query: GraphQlQuery,
        variables: GraphQlVariables,
        extensions: GraphQlExtensions,
    ) -> Result<(GraphQlQuery, GraphQlVariables, GraphQlExtensions), String> {
        let (query, variables, extensions) = self.left.transform(query, variables, extensions)?;
        self.right.transform(query, variables, extensions)
    }
}

pub fn compose_graphql_transforms<T1: GraphQlQueryTransform, T2: GraphQlQueryTransform>(
    left: T1,
    right: T2,
) -> ChainedGraphQlQueryTransform<T1, T2> {
    ChainedGraphQlQueryTransform { left, right }
}

pub trait GraphQlText<'src>:
    graphql_parser::query::Text<'src, Value = Self>
    + 'src
    + From<&'src str>
    + AsRef<str>
    + PartialEq
    + Eq
    + PartialOrd
    + Ord
    + Clone
    + ToString
    + From<String>
    + std::borrow::Borrow<str>
    + std::hash::Hash
    + std::fmt::Debug
{
}
impl GraphQlText<'static> for String {}
impl<'a> GraphQlText<'a> for Cow<'a, str> {}

pub type GraphQlSchema = graphql_parser::schema::Document<'static, String>;

#[derive(Clone, Debug)]
pub struct GraphQlSchemaTypes<'a, T: GraphQlText<'a>> {
    types: HashMap<String, graphql_parser::schema::TypeDefinition<'a, T>>,
    query: Option<String>,
    mutation: Option<String>,
    subscription: Option<String>,
}
impl<'a, T: GraphQlText<'a>> GraphQlSchemaTypes<'a, T> {
    fn get_type(&self, name: &str) -> Option<&graphql_parser::schema::TypeDefinition<'a, T>> {
        self.types.get(name)
    }
}

pub type GraphQlQuery = crate::ast::query::Document;
pub type GraphQlVariables = JsonMap<String, JsonValue>;
pub type GraphQlExtensions = JsonMap<String, JsonValue>;

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub enum GraphQlOperationType {
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

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct GraphQlOperationPayload {
    pub query: String,
    pub operation_name: Option<String>,
    pub variables: GraphQlVariables,
    pub extensions: GraphQlExtensions,
}
impl GraphQlOperationPayload {
    pub fn into_json(self) -> JsonValue {
        JsonValue::Object(JsonMap::from_iter([
            (String::from("query"), JsonValue::String(self.query)),
            (
                String::from("operationName"),
                match self.operation_name {
                    Some(operation_name) => JsonValue::String(operation_name),
                    None => JsonValue::Null,
                },
            ),
            (String::from("variables"), JsonValue::Object(self.variables)),
            (
                String::from("extensions"),
                JsonValue::Object(self.extensions),
            ),
        ]))
    }
}

pub fn deserialize_graphql_operation(data: &str) -> Result<GraphQlOperationPayload, String> {
    match reflex_json::deserialize(data) {
        Err(error) => Err(error),
        Ok(value) => match value {
            JsonValue::Object(payload) => {
                parse_graphql_operation_payload(&payload).map_err(|err| format!("{}", err))
            }
            _ => Err(String::from("Invalid request payload")),
        },
    }
}

pub(crate) fn parse_graphql_operation_payload(
    payload: &JsonMap<String, JsonValue>,
) -> Result<GraphQlOperationPayload, ParseOperationPayloadError> {
    let query = match payload.get("query") {
        None => Err(ParseOperationPayloadError::MissingQuery),
        Some(value) => match value {
            JsonValue::String(value) => Ok(String::from(value)),
            _ => Err(ParseOperationPayloadError::InvalidQuery(value)),
        },
    }?;
    let operation_name = match payload.get("operationName") {
        None => Ok(None),
        Some(value) => match value {
            JsonValue::String(value) => Ok(Some(String::from(value))),
            JsonValue::Null => Ok(None),
            _ => Err(ParseOperationPayloadError::InvalidOperationName(value)),
        },
    }?;
    let variables = match payload.get("variables") {
        None => Ok(Default::default()),
        Some(JsonValue::Object(variables)) => Ok(variables.clone()),
        Some(value) => Err(ParseOperationPayloadError::InvalidVariables(value)),
    }?;
    let extensions = match payload.get("extensions") {
        None => Ok(Default::default()),
        Some(JsonValue::Object(extensions)) => Ok(extensions.clone()),
        Some(value) => Err(ParseOperationPayloadError::InvalidExtensions(value)),
    }?;
    Ok(GraphQlOperationPayload {
        query,
        operation_name,
        variables,
        extensions,
    })
}

pub(crate) enum ParseOperationPayloadError<'a> {
    MissingQuery,
    InvalidQuery(&'a JsonValue),
    InvalidOperationName(&'a JsonValue),
    InvalidVariables(&'a JsonValue),
    InvalidExtensions(&'a JsonValue),
}
impl<'a> std::error::Error for ParseOperationPayloadError<'a> {}
impl<'a> std::fmt::Debug for ParseOperationPayloadError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MissingQuery => write!(f, "MissingQuery"),
            Self::InvalidQuery(_) => f.debug_tuple("InvalidQuery").finish(),
            Self::InvalidOperationName(_) => f.debug_tuple("InvalidOperationName").finish(),
            Self::InvalidVariables(_) => f.debug_tuple("InvalidVariables").finish(),
            Self::InvalidExtensions(_) => f.debug_tuple("InvalidExtensions").finish(),
        }
    }
}
impl<'a> std::fmt::Display for ParseOperationPayloadError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseOperationPayloadError::MissingQuery => write!(f, "Missing query"),
            ParseOperationPayloadError::InvalidQuery(_) => write!(f, "Invalid query"),
            ParseOperationPayloadError::InvalidOperationName(_) => {
                write!(f, "Invalid operation name")
            }
            ParseOperationPayloadError::InvalidVariables(_) => {
                write!(f, "Invalid variables")
            }
            ParseOperationPayloadError::InvalidExtensions(_) => {
                write!(f, "Invalid extensions")
            }
        }
    }
}

pub fn parse_graphql_query(query: &str) -> Result<GraphQlQuery, graphql_parser::query::ParseError> {
    graphql_parser::parse_query::<String>(query).map(|document| (&document.into_static()).into())
}

pub fn parse_graphql_schema(
    schema: &str,
) -> Result<GraphQlSchema, graphql_parser::schema::ParseError> {
    graphql_parser::parse_schema::<String>(schema).map(|document| document.into_static())
}

#[derive(Debug)]
pub enum GraphQlOperationTypeError {
    MissingRootOperation,
    MultipleRootOperations,
    InvalidOperationName(String),
    MultipleNamedOperations(String),
}
impl std::error::Error for GraphQlOperationTypeError {}
impl std::fmt::Display for GraphQlOperationTypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MissingRootOperation => write!(f, "Missing root operation"),
            Self::MultipleRootOperations => write!(f, "Multiple root operations"),
            Self::InvalidOperationName(name) => write!(f, "Invalid root operation name: {}", name),
            Self::MultipleNamedOperations(name) => {
                write!(f, "Ambiguous root operation name: {}", name)
            }
        }
    }
}

pub fn parse_graphql_root_operation<'a>(
    ast: &'a GraphQlQuery,
    operation_name: Option<&str>,
) -> Result<&'a OperationDefinition, GraphQlOperationTypeError> {
    let root_operations = ast
        .definitions
        .iter()
        .filter_map(|definition| match definition {
            Definition::Operation(operation) => Some(operation),
            _ => None,
        });
    let named_operations = root_operations.map(|operation| match operation {
        OperationDefinition::Query(query) => (operation, query.name.as_ref()),
        OperationDefinition::Mutation(mutation) => (operation, mutation.name.as_ref()),
        OperationDefinition::Subscription(subscription) => (operation, subscription.name.as_ref()),
        OperationDefinition::SelectionSet(_) => (operation, None),
    });
    let mut matching_operations = named_operations
        .filter(|(_, name)| match (operation_name, name.as_ref()) {
            (None, None) => true,
            (Some(operation_name), Some(name)) => name.as_str() == operation_name,
            _ => false,
        })
        .map(|(operation, _)| operation);
    match matching_operations.next() {
        None => Err(match operation_name {
            None => GraphQlOperationTypeError::MissingRootOperation,
            Some(operation_name) => {
                GraphQlOperationTypeError::InvalidOperationName(String::from(operation_name))
            }
        }),
        Some(root_operation) => match matching_operations.next() {
            Some(_) => Err(match operation_name {
                None => GraphQlOperationTypeError::MultipleRootOperations,
                Some(operation_name) => {
                    GraphQlOperationTypeError::MultipleNamedOperations(String::from(operation_name))
                }
            }),
            None => Ok(root_operation),
        },
    }
}

pub fn parse_graphql_operation_type(
    ast: &GraphQlQuery,
    operation_name: Option<&str>,
) -> Result<GraphQlOperationType, GraphQlOperationTypeError> {
    parse_graphql_root_operation(ast, operation_name).map(|operation| match operation {
        OperationDefinition::Query(_) => GraphQlOperationType::Query,
        OperationDefinition::Mutation(_) => GraphQlOperationType::Mutation,
        OperationDefinition::Subscription(_) => GraphQlOperationType::Subscription,
        OperationDefinition::SelectionSet(_) => GraphQlOperationType::Query,
    })
}

pub fn serialize_graphql_result_payload<T: Expression>(
    result: &T,
    factory: &impl ExpressionFactory<T>,
) -> Result<JsonValue, Vec<JsonValue>> {
    match factory.match_signal_term(result) {
        Some(result) => {
            let errors = serialize_json_signal_errors(result);
            Err(if errors.is_empty() {
                vec![create_json_error_object(
                    String::from("Unknown error"),
                    None,
                )]
            } else {
                errors
            })
        }
        None => match reflex_json::sanitize(result) {
            Ok(result) => Ok(result),
            Err(message) => Err(vec![create_json_error_object(message, None)]),
        },
    }
}

pub fn create_json_error_object(
    message: impl Into<String>,
    metadata: impl IntoIterator<Item = (String, JsonValue)>,
) -> JsonValue {
    json_object(once((String::from("message"), JsonValue::String(message.into()))).chain(metadata))
}

pub fn create_graphql_success_response(result: JsonValue) -> JsonValue {
    json_object(once((String::from("data"), result)))
}

pub fn create_graphql_error_response(errors: impl IntoIterator<Item = JsonValue>) -> JsonValue {
    json_object(once((
        String::from("errors"),
        JsonValue::Array(
            errors
                .into_iter()
                .flat_map(normalize_graphql_error_payload)
                .collect(),
        ),
    )))
}

fn normalize_graphql_error_payload(payload: JsonValue) -> Vec<JsonValue> {
    match payload {
        JsonValue::Array(errors) => errors
            .into_iter()
            .flat_map(normalize_graphql_error_payload)
            .collect(),
        JsonValue::Object(_) => vec![payload],
        JsonValue::String(message) => vec![create_json_error_object(message, None)],
        payload => vec![json_object(once((String::from("message"), payload)))],
    }
}

pub fn serialize_json_signal_errors<TTerm: SignalTermType<T>, T: Expression>(
    signal: &TTerm,
) -> Vec<JsonValue> {
    signal
        .signals()
        .as_deref()
        .iter()
        .map(|item| item.as_deref())
        .filter(|signal| match signal.signal_type() {
            SignalType::Error => true,
            _ => false,
        })
        .map(|signal| {
            sanitize(signal.payload().as_deref())
                .map(|value| match value {
                    JsonValue::String(message) => create_json_error_object(message, None),
                    _ => value,
                })
                .unwrap_or_else(|_| JsonValue::Null)
        })
        .collect()
}

pub fn parse_graphql_operation<T: Expression>(
    operation: &GraphQlOperation,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, String>
where
    T::Builtin: GraphQlParserBuiltin,
{
    parse_ast_query(operation.query(), operation.variables(), factory, allocator)
}

pub fn parse<'a, T: Expression>(
    query: &'a str,
    variables: &GraphQlVariables,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, String>
where
    T::Builtin: GraphQlParserBuiltin,
{
    match graphql_parser::parse_query::<String>(query) {
        Ok(query) => parse_ast_query(
            &GraphQlQuery::from(&query.into_static()),
            variables,
            factory,
            allocator,
        ),
        Err(error) => Err(format!("{}", error)),
    }
}

fn parse_ast_query<'vars, T: Expression>(
    query: &Document,
    variables: &GraphQlVariables,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, String>
where
    T::Builtin: GraphQlParserBuiltin,
{
    let fragments = query
        .definitions
        .iter()
        .filter_map(|definition| match definition {
            Definition::Fragment(fragment) => Some((fragment.name.clone(), fragment)),
            _ => None,
        })
        .collect::<QueryFragments>();
    match get_query_root_operation(&query) {
        Err(error) => Err(error),
        Ok(operation_root) => {
            parse_operation(operation_root, &variables, &fragments, factory, allocator)
        }
    }
}

fn get_query_root_operation<'a>(document: &'a Document) -> Result<&'a OperationDefinition, String> {
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

fn parse_operation<T: Expression>(
    operation_root: &OperationDefinition,
    variables: &GraphQlVariables,
    fragments: &QueryFragments<'_>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, String>
where
    T::Builtin: GraphQlParserBuiltin,
{
    let (selection_set, variable_definitions) = match operation_root {
        OperationDefinition::Query(operation) => (
            &operation.selection_set,
            Some(&operation.variable_definitions),
        ),
        OperationDefinition::Mutation(operation) => (
            &operation.selection_set,
            Some(&operation.variable_definitions),
        ),
        OperationDefinition::Subscription(operation) => (
            &operation.selection_set,
            Some(&operation.variable_definitions),
        ),
        OperationDefinition::SelectionSet(selection) => (selection, None),
    };
    let variables =
        parse_operation_variables(variable_definitions.unwrap_or(&Vec::new()), variables)?;
    let shape = parse_selection_set(selection_set, &variables, fragments, factory, allocator)?;
    let operation_type = match operation_root {
        OperationDefinition::Query(_) | OperationDefinition::SelectionSet(_) => "query",
        OperationDefinition::Mutation(_) => "mutation",
        OperationDefinition::Subscription(_) => "subscription",
    };
    Ok(create_query_root(shape, operation_type, factory, allocator))
}

fn create_query_root<T: Expression>(
    shape: T,
    operation_type: &'static str,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: GraphQlParserBuiltin,
{
    factory.create_lambda_term(
        1,
        factory.create_application_term(
            shape,
            allocator.create_unit_list(factory.create_application_term(
                factory.create_builtin_term(Get),
                allocator.create_pair(
                    factory.create_variable_term(0),
                    factory.create_string_term(allocator.create_static_string(operation_type)),
                ),
            )),
        ),
    )
}

fn parse_operation_variables(
    variable_definitions: &[VariableDefinition],
    variables: &GraphQlVariables,
) -> Result<GraphQlVariables, String> {
    variable_definitions
        .iter()
        .map(|definition| {
            let value = variables.get(&definition.name).cloned();
            Ok((
                definition.name.clone(),
                validate_variable(value, &definition.name, &definition.var_type)?,
            ))
        })
        .collect::<Result<GraphQlVariables, _>>()
}

fn validate_variable(
    value: Option<JsonValue>,
    name: &str,
    var_type: &Type,
) -> Result<JsonValue, String> {
    match var_type {
        Type::NonNullType(_) => {
            value.ok_or_else(|| format!("Missing required query variable: {}", name.to_string()))
        }
        // TODO: Validate query variable types
        // TODO: Differentiate between missing optional variables and null values
        _ => Ok(value.unwrap_or_else(|| JsonValue::Null)),
    }
}

fn parse_selection_set<T: Expression>(
    selection_set: &SelectionSet,
    variables: &GraphQlVariables,
    fragments: &QueryFragments<'_>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, String>
where
    T::Builtin: GraphQlParserBuiltin,
{
    if selection_set.items.is_empty() {
        parse_leaf(factory)
    } else {
        let fields =
            parse_selection_set_fields(selection_set, variables, fragments, factory, allocator)?;
        let (keys, values): (Vec<_>, Vec<_>) = fields.into_iter().unzip();
        Ok(create_query_branch(
            factory.create_lambda_term(
                1,
                factory.create_application_term(
                    factory.create_builtin_term(CollectRecord),
                    allocator.create_sized_list(
                        values.len() + 1,
                        once(factory.create_constructor_term(
                            allocator.create_struct_prototype(allocator.create_list(keys)),
                        ))
                        .chain(values.into_iter().map(|query| {
                            factory.create_application_term(
                                query,
                                allocator.create_unit_list(factory.create_variable_term(0)),
                            )
                        })),
                    ),
                ),
            ),
            factory,
            allocator,
        ))
    }
}

fn parse_leaf<T: Expression>(factory: &impl ExpressionFactory<T>) -> Result<T, String>
where
    T::Builtin: GraphQlParserBuiltin,
{
    Ok(factory.create_builtin_term(FlattenDeep))
}

fn parse_selection_set_fields<T: Expression>(
    selection_set: &SelectionSet,
    variables: &GraphQlVariables,
    fragments: &QueryFragments<'_>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<Vec<(T, T)>, String>
where
    T::Builtin: GraphQlParserBuiltin,
{
    selection_set
        .items
        .iter()
        .flat_map(|field| match field {
            Selection::Field(field) => match get_field_is_ignored(&field, variables) {
                Err(err) => Countable::Single(Err(err)),
                Ok(is_ignored_field) => {
                    if is_ignored_field {
                        Countable::Empty
                    } else {
                        match parse_field(&field, variables, fragments, factory, allocator) {
                            Ok(result) => {
                                let key = field.alias.as_ref().unwrap_or(&field.name).to_string();
                                Countable::Single(Ok((
                                    factory.create_string_term(allocator.create_string(key)),
                                    result,
                                )))
                            }
                            Err(error) => Countable::Single(Err(error)),
                        }
                    }
                }
            },
            Selection::FragmentSpread(fragment) => match fragments.get(&fragment.fragment_name) {
                Some(fragment) => {
                    let fields =
                        parse_fragment_fields(fragment, variables, fragments, factory, allocator);
                    Countable::Multiple(fields)
                }
                None => Countable::Single(Err(format!(
                    "Invalid fragment name: {}",
                    fragment.fragment_name.to_string()
                ))),
            },
            Selection::InlineFragment(_) => {
                Countable::Single(Err(String::from("Inline fragments not yet implemented")))
            }
        })
        .collect::<Result<Vec<_>, _>>()
}

fn get_field_is_ignored(field: &Field, variables: &GraphQlVariables) -> Result<bool, String> {
    // https://spec.graphql.org/June2018/#sec--skip
    // https://spec.graphql.org/June2018/#sec--include
    let skip_value = parse_field_skip_directive(field, variables)?;
    let include_value = parse_field_include_directive(field, variables)?;
    Ok(match (skip_value, include_value) {
        (None, None) => false,
        (Some(skip), None) => skip,
        (None, Some(include)) => !include,
        (Some(skip), Some(include)) => skip || !include,
    })
}

fn parse_field_skip_directive(
    field: &Field,
    variables: &GraphQlVariables,
) -> Result<Option<bool>, String> {
    parse_boolean_field_directive("skip", field, variables)
}

fn parse_field_include_directive(
    field: &Field,
    variables: &GraphQlVariables,
) -> Result<Option<bool>, String> {
    parse_boolean_field_directive("include", field, variables)
}

fn parse_boolean_field_directive(
    directive_name: &'static str,
    field: &Field,
    variables: &GraphQlVariables,
) -> Result<Option<bool>, String> {
    let directive = field
        .directives
        .iter()
        .find(|directive| directive.name.as_str() == directive_name);
    match directive {
        None => Ok(None),
        Some(directive) => {
            match directive
                .arguments
                .iter()
                .find(|(key, _value)| key.as_str() == "if")
            {
                None => Err(format!(
                    "Missing \"if\" argument for @{} directive",
                    directive_name
                )),
                Some((_key, value)) => match value {
                    Value::Boolean(value) => Ok(Some(*value)),
                    Value::Variable(variable_name) => match variables.get(variable_name) {
                        None => Err(format!(
                            "Undefined variable for \"if\" argument of @{} directive: \"{}\"",
                            directive_name, variable_name
                        )),
                        Some(value) => match value {
                            JsonValue::Bool(value) => Ok(Some(*value)),
                            _ => Err(format!(
                                "Invalid value for \"if\" argument of @{} directive: \"{}\"",
                                directive_name, value
                            )),
                        },
                    },
                    _ => Err(format!(
                        "Invalid value for \"if\" argument of @{} directive: \"{}\"",
                        directive_name,
                        format_graphql_value(value)
                    )),
                },
            }
        }
    }
}

fn format_graphql_value(value: &Value) -> String {
    match value {
        Value::Variable(variable_name) => format!("${}", variable_name),
        Value::Int(value) => match value.as_i64() {
            Some(value) => format!("{}", value),
            None => format!("{:?}", value),
        },
        Value::Float(value) => format!("{}", value),
        Value::String(value) => format!("{}", value),
        Value::Boolean(value) => format!("{}", value),
        Value::Null => format!("null"),
        Value::Enum(value) => format!("{}", value),
        Value::List(value) => format!(
            "[{}]",
            value
                .iter()
                .map(format_graphql_value)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        Value::Object(value) => {
            if value.is_empty() {
                String::from("")
            } else {
                format!(
                    "{{ {} }}",
                    value
                        .iter()
                        .map(|(key, value)| format!("{}: {}", key, format_graphql_value(value)))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }
}

fn parse_fragment_fields<T: Expression>(
    fragment: &FragmentDefinition,
    variables: &GraphQlVariables,
    fragments: &QueryFragments<'_>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> impl IntoIterator<Item = Result<(T, T), String>>
where
    T::Builtin: GraphQlParserBuiltin,
{
    match parse_selection_set_fields(
        &fragment.selection_set,
        variables,
        fragments,
        factory,
        allocator,
    ) {
        Ok(fields) => Countable::Multiple(fields.into_iter().map(Ok)),
        Err(error) => Countable::Single(Err(error)),
    }
}

fn parse_field<T: Expression>(
    field: &Field,
    variables: &GraphQlVariables,
    fragments: &QueryFragments<'_>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, String>
where
    T::Builtin: GraphQlParserBuiltin,
{
    let field_args = if field.arguments.is_empty() {
        None
    } else {
        Some(parse_field_arguments(
            &field.arguments,
            variables,
            fragments,
            factory,
            allocator,
        )?)
    };
    let body = parse_selection_set(
        &field.selection_set,
        variables,
        fragments,
        factory,
        allocator,
    )?;
    Ok(factory.create_lambda_term(
        1,
        factory.create_application_term(
            body,
            allocator.create_unit_list({
                let field = factory.create_application_term(
                    factory.create_builtin_term(Get),
                    allocator.create_pair(
                        factory.create_variable_term(0),
                        factory.create_string_term(allocator.create_string(field.name.to_string())),
                    ),
                );
                match field_args {
                    None => field,
                    Some(field_args) => factory.create_application_term(field, field_args),
                }
            }),
        ),
    ))
}

fn parse_field_arguments<T: Expression>(
    args: &Vec<(String, Value)>,
    variables: &GraphQlVariables,
    fragments: &QueryFragments<'_>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T::ExpressionList, String> {
    let arg_fields = args
        .iter()
        .map(
            |(key, value)| match parse_value(value, variables, fragments, factory, allocator) {
                Ok(value) => Ok((
                    factory.create_string_term(allocator.create_string(key.to_string())),
                    value,
                )),
                Err(error) => Err(error),
            },
        )
        .collect::<Result<Vec<_>, _>>()?;
    let arg = create_record(arg_fields, factory, allocator);
    Ok(allocator.create_unit_list(arg))
}

fn parse_value<T: Expression>(
    value: &Value,
    variables: &GraphQlVariables,
    fragments: &QueryFragments<'_>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, String> {
    match value {
        Value::Variable(name) => match variables.get(name) {
            Some(value) => reflex_json::hydrate(value.clone(), factory, allocator),
            None => Err(format!("Undeclared query variable: {}", name.to_string())),
        },
        Value::Int(value) => Ok(factory.create_int_term(
            value
                .as_i64()
                .ok_or_else(|| format!("Invalid integer argument: {:?}", value))?
                as i32,
        )),
        Value::Float(value) => Ok(factory.create_float_term(*value)),
        Value::String(value) => {
            Ok(factory.create_string_term(allocator.create_string(value.as_str())))
        }
        Value::Boolean(value) => Ok(factory.create_boolean_term(*value)),
        Value::Null => Ok(factory.create_nil_term()),
        Value::Enum(value) => {
            Ok(factory.create_string_term(allocator.create_string(value.to_string())))
        }
        Value::List(value) => {
            let values = value
                .iter()
                .map(|item| parse_value(item, variables, fragments, factory, allocator))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(factory.create_list_term(allocator.create_list(values)))
        }
        Value::Object(value) => {
            let entries = value
                .iter()
                .map(|(key, value)| {
                    match parse_value(value, variables, fragments, factory, allocator) {
                        Ok(value) => Ok((
                            factory.create_string_term(allocator.create_string(key.to_string())),
                            value,
                        )),
                        Err(error) => Err(error),
                    }
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(create_record(entries, factory, allocator))
        }
    }
}

fn create_query_branch<T: Expression>(
    shape: T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: GraphQlParserBuiltin,
{
    factory.create_lambda_term(
        1,
        factory.create_application_term(
            factory.create_builtin_term(DynamicQueryBranch),
            allocator.create_pair(factory.create_variable_term(0), shape),
        ),
    )
}

enum Countable<T, I: IntoIterator<Item = T>> {
    Empty,
    Single(T),
    Multiple(I),
}
impl<T, I: IntoIterator<Item = T>> IntoIterator for Countable<T, I> {
    type Item = T;
    type IntoIter =
        std::iter::Chain<std::option::IntoIter<T>, std::iter::Flatten<std::option::IntoIter<I>>>;
    fn into_iter(self) -> Self::IntoIter {
        let (left, right) = match self {
            Self::Empty => (None, None),
            Self::Single(value) => (Some(value), None),
            Self::Multiple(value) => (None, Some(value)),
        };
        left.into_iter().chain(right.into_iter().flatten())
    }
}

#[cfg(test)]
mod tests {
    use reflex::core::Uuid;
    use reflex::{
        cache::SubstitutionCache,
        core::{
            create_record, evaluate, Applicable, Arity, Builtin, DependencyList, Evaluate,
            EvaluationCache, EvaluationResult, Expression, ExpressionFactory, HeapAllocator,
            Reducible, Rewritable, StateCache, Uid,
        },
    };
    use reflex_lang::{allocator::DefaultAllocator, SharedTermFactory};
    use reflex_stdlib::{Add, Stdlib};
    use std::convert::{TryFrom, TryInto};

    use crate as reflex_graphql;

    use super::{parse, stdlib::Stdlib as GraphQlStdlib};

    #[derive(Clone, Copy, Eq, PartialEq, Hash, Debug)]
    enum GraphQlTestBuiltins {
        Stdlib(Stdlib),
        GraphQl(GraphQlStdlib),
    }
    impl From<Stdlib> for GraphQlTestBuiltins {
        fn from(target: Stdlib) -> Self {
            GraphQlTestBuiltins::Stdlib(target)
        }
    }
    impl From<GraphQlStdlib> for GraphQlTestBuiltins {
        fn from(target: GraphQlStdlib) -> Self {
            GraphQlTestBuiltins::GraphQl(target)
        }
    }
    impl Uid for GraphQlTestBuiltins {
        fn uid(&self) -> reflex::core::Uuid {
            match self {
                GraphQlTestBuiltins::Stdlib(term) => term.uid(),
                GraphQlTestBuiltins::GraphQl(term) => term.uid(),
            }
        }
    }
    impl TryFrom<Uuid> for GraphQlTestBuiltins {
        type Error = ();
        fn try_from(value: Uuid) -> Result<Self, Self::Error> {
            TryInto::<Stdlib>::try_into(value)
                .map(Self::Stdlib)
                .or_else(|_| TryInto::<GraphQlStdlib>::try_into(value).map(Self::GraphQl))
        }
    }
    impl Builtin for GraphQlTestBuiltins {
        fn arity(&self) -> Arity {
            match self {
                GraphQlTestBuiltins::Stdlib(term) => term.arity(),
                GraphQlTestBuiltins::GraphQl(term) => term.arity(),
            }
        }
        fn should_parallelize<T: Expression<Builtin = Self> + Applicable<T>>(
            &self,
            args: &[T],
        ) -> bool {
            match self {
                GraphQlTestBuiltins::Stdlib(term) => term.should_parallelize(args),
                GraphQlTestBuiltins::GraphQl(term) => term.should_parallelize(args),
            }
        }
        fn apply<T: Expression<Builtin = Self> + Applicable<T>>(
            &self,
            args: impl ExactSizeIterator<Item = T>,
            factory: &impl ExpressionFactory<T>,
            allocator: &impl HeapAllocator<T>,
            cache: &mut impl EvaluationCache<T>,
        ) -> Result<T, String> {
            match self {
                GraphQlTestBuiltins::Stdlib(term) => term.apply(args, factory, allocator, cache),
                GraphQlTestBuiltins::GraphQl(term) => term.apply(args, factory, allocator, cache),
            }
        }
    }
    impl std::fmt::Display for GraphQlTestBuiltins {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Stdlib(target) => std::fmt::Display::fmt(target, f),
                Self::GraphQl(target) => std::fmt::Display::fmt(target, f),
            }
        }
    }

    impl From<reflex_stdlib::stdlib::Abs> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Abs) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Add> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Add) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::And> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::And) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Append> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Append) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Apply> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Apply) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Car> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Car) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Cdr> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Cdr) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Ceil> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Ceil) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Collect> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Collect) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::CollectFilterResults> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::CollectFilterResults) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::CollectHashMap> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::CollectHashMap) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::CollectHashSet> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::CollectHashSet) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::CollectRecord> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::CollectRecord) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::CollectList> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::CollectList) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Concat> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Concat) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Cons> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Cons) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::ConstructHashMap> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::ConstructHashMap) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::ConstructHashSet> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::ConstructHashSet) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::ConstructRecord> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::ConstructRecord) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::ConstructList> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::ConstructList) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Contains> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Contains) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Divide> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Divide) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Effect> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Effect) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::EndsWith> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::EndsWith) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Entries> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Entries) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Eq> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Eq) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Equal> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Equal) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Filter> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Filter) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Flatten> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Flatten) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Floor> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Floor) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Get> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Get) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Gt> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Gt) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Gte> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Gte) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::If> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::If) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::IfError> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::IfError) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::IfPending> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::IfPending) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Insert> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Insert) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Keys> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Keys) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Length> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Length) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Lt> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Lt) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Lte> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Lte) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Map> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Map) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Match> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Match) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Max> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Max) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Merge> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Merge) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Min> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Min) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Multiply> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Multiply) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Not> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Not) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Or> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Or) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Pow> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Pow) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Push> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Push) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::PushFront> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::PushFront) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Reduce> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Reduce) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Remainder> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Remainder) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Replace> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Replace) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::ResolveArgs> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::ResolveArgs) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::ResolveDeep> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::ResolveDeep) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::ResolveHashMap> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::ResolveHashMap) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::ResolveHashSet> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::ResolveHashSet) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::ResolveShallow> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::ResolveShallow) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::ResolveRecord> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::ResolveRecord) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::ResolveList> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::ResolveList) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Round> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Round) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Sequence> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Sequence) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Slice> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Slice) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Split> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Split) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::StartsWith> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::StartsWith) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Subtract> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Subtract) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_stdlib::stdlib::Values> for GraphQlTestBuiltins {
        fn from(value: reflex_stdlib::stdlib::Values) -> Self {
            Self::from(reflex_stdlib::stdlib::Stdlib::from(value))
        }
    }

    impl From<reflex_graphql::stdlib::CollectQueryListItems> for GraphQlTestBuiltins {
        fn from(value: reflex_graphql::stdlib::CollectQueryListItems) -> Self {
            Self::from(reflex_graphql::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_graphql::stdlib::DynamicQueryBranch> for GraphQlTestBuiltins {
        fn from(value: reflex_graphql::stdlib::DynamicQueryBranch) -> Self {
            Self::from(reflex_graphql::stdlib::Stdlib::from(value))
        }
    }
    impl From<reflex_graphql::stdlib::FlattenDeep> for GraphQlTestBuiltins {
        fn from(value: reflex_graphql::stdlib::FlattenDeep) -> Self {
            Self::from(reflex_graphql::stdlib::Stdlib::from(value))
        }
    }

    #[test]
    fn leaf_queries() {
        let factory = SharedTermFactory::<GraphQlTestBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let root = create_record(
            [
                (
                    factory.create_string_term(allocator.create_static_string("query")),
                    create_record(
                        [
                            (
                                factory.create_string_term(allocator.create_static_string("first")),
                                factory.create_int_term(3),
                            ),
                            (
                                factory
                                    .create_string_term(allocator.create_static_string("second")),
                                factory.create_int_term(4),
                            ),
                            (
                                factory.create_string_term(allocator.create_static_string("third")),
                                factory.create_int_term(5),
                            ),
                        ],
                        &factory,
                        &allocator,
                    ),
                ),
                (
                    factory.create_string_term(allocator.create_static_string("mutation")),
                    factory.create_nil_term(),
                ),
                (
                    factory.create_string_term(allocator.create_static_string("subscription")),
                    factory.create_nil_term(),
                ),
            ],
            &factory,
            &allocator,
        );
        let variables = Default::default();
        let query = parse(
            "
                query {
                    second
                    third
                }
            ",
            &variables,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = apply_query(query, root, &factory, &allocator);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_record(
                    [
                        (
                            factory.create_string_term(allocator.create_static_string("second")),
                            factory.create_int_term(4)
                        ),
                        (
                            factory.create_string_term(allocator.create_static_string("third")),
                            factory.create_int_term(5)
                        ),
                    ],
                    &factory,
                    &allocator
                ),
                DependencyList::empty()
            )
        );
    }

    #[test]
    fn computed_leaf_queries() {
        let factory = SharedTermFactory::<GraphQlTestBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let root = create_record(
            [
                (
                    factory.create_string_term(allocator.create_static_string("query")),
                    factory.create_application_term(
                        factory.create_lambda_term(
                            1,
                            create_record(
                                [
                                    (
                                        factory.create_string_term(
                                            allocator.create_static_string("first"),
                                        ),
                                        factory.create_application_term(
                                            factory.create_builtin_term(Add),
                                            allocator.create_pair(
                                                factory.create_int_term(3),
                                                factory.create_variable_term(0),
                                            ),
                                        ),
                                    ),
                                    (
                                        factory.create_string_term(
                                            allocator.create_static_string("second"),
                                        ),
                                        factory.create_application_term(
                                            factory.create_builtin_term(Add),
                                            allocator.create_pair(
                                                factory.create_int_term(4),
                                                factory.create_variable_term(0),
                                            ),
                                        ),
                                    ),
                                    (
                                        factory.create_string_term(
                                            allocator.create_static_string("third"),
                                        ),
                                        factory.create_application_term(
                                            factory.create_builtin_term(Add),
                                            allocator.create_pair(
                                                factory.create_int_term(5),
                                                factory.create_variable_term(0),
                                            ),
                                        ),
                                    ),
                                ],
                                &factory,
                                &allocator,
                            ),
                        ),
                        allocator.create_unit_list(factory.create_int_term(10)),
                    ),
                ),
                (
                    factory.create_string_term(allocator.create_static_string("mutation")),
                    factory.create_nil_term(),
                ),
                (
                    factory.create_string_term(allocator.create_static_string("subscription")),
                    factory.create_nil_term(),
                ),
            ],
            &factory,
            &allocator,
        );
        let variables = Default::default();
        let query = parse(
            "
                query {
                    second
                    third
                }
            ",
            &variables,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = apply_query(query, root, &factory, &allocator);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_record(
                    [
                        (
                            factory.create_string_term(allocator.create_static_string("second")),
                            factory.create_int_term(4 + 10)
                        ),
                        (
                            factory.create_string_term(allocator.create_static_string("third")),
                            factory.create_int_term(5 + 10)
                        ),
                    ],
                    &factory,
                    &allocator
                ),
                DependencyList::empty()
            )
        );
    }

    #[test]
    fn list_leaf_queries() {
        let factory = SharedTermFactory::<GraphQlTestBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let root = create_record(
            [
                (
                    factory.create_string_term(allocator.create_static_string("query")),
                    create_record(
                        [
                            (
                                factory.create_string_term(allocator.create_static_string("foo")),
                                factory.create_nil_term(),
                            ),
                            (
                                factory.create_string_term(allocator.create_static_string("items")),
                                factory.create_list_term(allocator.create_list([
                                    factory.create_int_term(3),
                                    factory.create_int_term(4),
                                    factory.create_int_term(5),
                                ])),
                            ),
                            (
                                factory.create_string_term(allocator.create_static_string("bar")),
                                factory.create_nil_term(),
                            ),
                        ],
                        &factory,
                        &allocator,
                    ),
                ),
                (
                    factory.create_string_term(allocator.create_static_string("mutation")),
                    factory.create_nil_term(),
                ),
                (
                    factory.create_string_term(allocator.create_static_string("subscription")),
                    factory.create_nil_term(),
                ),
            ],
            &factory,
            &allocator,
        );
        let variables = Default::default();
        let query = parse(
            "
                query {
                    items
                }
            ",
            &variables,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = apply_query(query, root, &factory, &allocator);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_record(
                    [(
                        factory.create_string_term(allocator.create_static_string("items")),
                        factory.create_list_term(allocator.create_list([
                            factory.create_int_term(3),
                            factory.create_int_term(4),
                            factory.create_int_term(5)
                        ]))
                    ),],
                    &factory,
                    &allocator,
                ),
                DependencyList::empty()
            ),
        );
    }

    #[test]
    fn deeply_nested_list_leaf_queries() {
        let factory = SharedTermFactory::<GraphQlTestBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let root = create_record(
            [
                (
                    factory.create_string_term(allocator.create_static_string("query")),
                    create_record(
                        [
                            (
                                factory.create_string_term(allocator.create_static_string("foo")),
                                factory.create_nil_term(),
                            ),
                            (
                                factory.create_string_term(allocator.create_static_string("items")),
                                factory.create_list_term(allocator.create_list([
                                    factory.create_list_term(allocator.create_list([
                                        factory.create_list_term(allocator.create_list([
                                            factory.create_float_term(1.1),
                                            factory.create_float_term(1.2),
                                            factory.create_float_term(1.3),
                                        ])),
                                        factory.create_list_term(allocator.create_list([
                                            factory.create_float_term(1.4),
                                            factory.create_float_term(1.5),
                                            factory.create_float_term(1.6),
                                        ])),
                                        factory.create_list_term(allocator.create_list([
                                            factory.create_float_term(1.7),
                                            factory.create_float_term(1.8),
                                            factory.create_float_term(1.9),
                                        ])),
                                    ])),
                                    factory.create_list_term(allocator.create_list([
                                        factory.create_list_term(allocator.create_list([
                                            factory.create_float_term(2.1),
                                            factory.create_float_term(2.2),
                                            factory.create_float_term(2.3),
                                        ])),
                                        factory.create_list_term(allocator.create_list([
                                            factory.create_float_term(2.4),
                                            factory.create_float_term(2.5),
                                            factory.create_float_term(2.6),
                                        ])),
                                        factory.create_list_term(allocator.create_list([
                                            factory.create_float_term(2.7),
                                            factory.create_float_term(2.8),
                                            factory.create_float_term(2.9),
                                        ])),
                                    ])),
                                    factory.create_list_term(allocator.create_list([
                                        factory.create_list_term(allocator.create_list([
                                            factory.create_float_term(3.1),
                                            factory.create_float_term(3.2),
                                            factory.create_float_term(3.3),
                                        ])),
                                        factory.create_list_term(allocator.create_list([
                                            factory.create_float_term(3.4),
                                            factory.create_float_term(3.5),
                                            factory.create_float_term(3.6),
                                        ])),
                                        factory.create_list_term(allocator.create_list([
                                            factory.create_float_term(3.7),
                                            factory.create_float_term(3.8),
                                            factory.create_float_term(3.9),
                                        ])),
                                    ])),
                                ])),
                            ),
                            (
                                factory.create_string_term(allocator.create_static_string("bar")),
                                factory.create_nil_term(),
                            ),
                        ],
                        &factory,
                        &allocator,
                    ),
                ),
                (
                    factory.create_string_term(allocator.create_static_string("mutation")),
                    factory.create_nil_term(),
                ),
                (
                    factory.create_string_term(allocator.create_static_string("subscription")),
                    factory.create_nil_term(),
                ),
            ],
            &factory,
            &allocator,
        );
        let variables = Default::default();
        let query = parse(
            "
                query {
                    items
                }
            ",
            &variables,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = apply_query(query, root, &factory, &allocator);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_record(
                    [(
                        factory.create_string_term(allocator.create_static_string("items")),
                        factory.create_list_term(allocator.create_list([
                            factory.create_list_term(allocator.create_list([
                                factory.create_list_term(allocator.create_list([
                                    factory.create_float_term(1.1),
                                    factory.create_float_term(1.2),
                                    factory.create_float_term(1.3),
                                ])),
                                factory.create_list_term(allocator.create_list([
                                    factory.create_float_term(1.4),
                                    factory.create_float_term(1.5),
                                    factory.create_float_term(1.6),
                                ])),
                                factory.create_list_term(allocator.create_list([
                                    factory.create_float_term(1.7),
                                    factory.create_float_term(1.8),
                                    factory.create_float_term(1.9),
                                ])),
                            ])),
                            factory.create_list_term(allocator.create_list([
                                factory.create_list_term(allocator.create_list([
                                    factory.create_float_term(2.1),
                                    factory.create_float_term(2.2),
                                    factory.create_float_term(2.3),
                                ])),
                                factory.create_list_term(allocator.create_list([
                                    factory.create_float_term(2.4),
                                    factory.create_float_term(2.5),
                                    factory.create_float_term(2.6),
                                ])),
                                factory.create_list_term(allocator.create_list([
                                    factory.create_float_term(2.7),
                                    factory.create_float_term(2.8),
                                    factory.create_float_term(2.9),
                                ])),
                            ])),
                            factory.create_list_term(allocator.create_list([
                                factory.create_list_term(allocator.create_list([
                                    factory.create_float_term(3.1),
                                    factory.create_float_term(3.2),
                                    factory.create_float_term(3.3),
                                ])),
                                factory.create_list_term(allocator.create_list([
                                    factory.create_float_term(3.4),
                                    factory.create_float_term(3.5),
                                    factory.create_float_term(3.6),
                                ])),
                                factory.create_list_term(allocator.create_list([
                                    factory.create_float_term(3.7),
                                    factory.create_float_term(3.8),
                                    factory.create_float_term(3.9),
                                ])),
                            ])),
                        ])),
                    )],
                    &factory,
                    &allocator,
                ),
                DependencyList::empty()
            )
        );
    }

    fn apply_query<T: Expression + Rewritable<T> + Reducible<T> + Evaluate<T>>(
        query: T,
        root: T,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> EvaluationResult<T> {
        let expression = factory.create_application_term(query, allocator.create_unit_list(root));
        evaluate(
            &expression,
            &StateCache::default(),
            factory,
            allocator,
            &mut SubstitutionCache::new(),
        )
    }
}
