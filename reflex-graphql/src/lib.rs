// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{borrow::Cow, collections::HashMap, iter::once};

use either::Either;
use reflex::{
    core::{Expression, ExpressionFactory, ExpressionList, HeapAllocator, SignalType},
    lang::{create_record, term::SignalTerm},
    stdlib::Stdlib,
};
use reflex_json::{json_object, sanitize, JsonMap, JsonValue};
use serde::{Deserialize, Serialize};

use crate::ast::{
    common::{Type, Value},
    query::*,
};

pub use graphql_parser;

pub mod ast;
mod operation;
pub use operation::{graphql_variables_are_equal, GraphQlOperation};
pub mod stdlib;
pub mod transform;
pub mod validate;
use stdlib::Stdlib as GraphQlStdlib;
pub mod subscriptions;

#[allow(type_alias_bounds)]
type QueryVariables<T: Expression> = HashMap<String, T>;
#[allow(type_alias_bounds)]
type QueryFragments<'a> = HashMap<String, &'a FragmentDefinition>;

pub trait GraphQlQueryTransform {
    fn transform(
        &self,
        document: GraphQlQuery,
        extensions: GraphQlExtensions,
    ) -> Result<(GraphQlQuery, GraphQlExtensions), String>;
}

#[derive(Clone, Copy)]
pub struct NoopGraphQlQueryTransform;
impl GraphQlQueryTransform for NoopGraphQlQueryTransform {
    fn transform(
        &self,
        document: GraphQlQuery,
        extensions: GraphQlExtensions,
    ) -> Result<(GraphQlQuery, GraphQlExtensions), String> {
        Ok((document, extensions))
    }
}

impl<TInner: GraphQlQueryTransform> GraphQlQueryTransform for Option<TInner> {
    fn transform(
        &self,
        document: GraphQlQuery,
        extensions: GraphQlExtensions,
    ) -> Result<(GraphQlQuery, GraphQlExtensions), String> {
        match self {
            Some(inner) => inner.transform(document, extensions),
            None => Ok((document, extensions)),
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
        document: GraphQlQuery,
        extensions: GraphQlExtensions,
    ) -> Result<(GraphQlQuery, GraphQlExtensions), String> {
        match self {
            Self::Left(inner) => inner.transform(document, extensions),
            Self::Right(inner) => inner.transform(document, extensions),
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
        document: GraphQlQuery,
        extensions: GraphQlExtensions,
    ) -> Result<(GraphQlQuery, GraphQlExtensions), String> {
        let (document, extensions) = self.left.transform(document, extensions)?;
        self.right.transform(document, extensions)
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

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct GraphQlOperationPayload {
    pub query: String,
    pub operation_name: Option<String>,
    pub variables: GraphQlVariables,
    pub extensions: GraphQlExtensions,
}
impl GraphQlOperationPayload {
    pub fn into_json(self) -> JsonValue {
        JsonValue::Object(JsonMap::from_iter(vec![
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

pub fn serialize_json_signal_errors<T: Expression>(signal: &SignalTerm<T>) -> Vec<JsonValue> {
    signal
        .signals()
        .iter()
        .filter(|signal| match signal.signal_type() {
            SignalType::Error => true,
            _ => false,
        })
        .map(|signal| {
            signal
                .args()
                .iter()
                .next()
                .and_then(|arg| sanitize(arg).ok())
                .map(|value| match value {
                    JsonValue::String(message) => create_json_error_object(message, None),
                    _ => value,
                })
                .unwrap_or_else(|| JsonValue::Null)
        })
        .collect()
}

pub fn parse_graphql_operation<T: Expression>(
    operation: &GraphQlOperation,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, String>
where
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
{
    let variables = operation
        .variables()
        .map(|(key, value)| {
            reflex_json::hydrate(value.clone(), factory, allocator)
                .map(|value| (String::from(key), value))
        })
        .collect::<Result<Vec<_>, _>>()?;
    parse_ast_query(operation.query(), variables, factory, allocator)
}

pub fn parse<'vars, 'a, T: Expression>(
    query: &'a str,
    variables: impl IntoIterator<Item = (&'vars str, T)>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, String>
where
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
{
    match graphql_parser::parse_query::<String>(query) {
        Ok(query) => parse_ast_query(
            &GraphQlQuery::from(&query.into_static()),
            variables
                .into_iter()
                .map(|(key, value)| (String::from(key), value)),
            factory,
            allocator,
        ),
        Err(error) => Err(format!("{}", error)),
    }
}

fn parse_ast_query<'vars, T: Expression>(
    query: &Document,
    variables: impl IntoIterator<Item = (String, T)> + 'vars,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, String>
where
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
{
    let variables = variables.into_iter().collect::<QueryVariables<T>>();
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
    variables: &QueryVariables<T>,
    fragments: &QueryFragments<'_>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, String>
where
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
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
    let variables = parse_operation_variables(
        variable_definitions.unwrap_or(&Vec::new()),
        variables,
        factory,
        allocator,
    )?;
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
    T::Builtin: From<Stdlib>,
{
    factory.create_lambda_term(
        1,
        factory.create_application_term(
            shape,
            allocator.create_unit_list(factory.create_application_term(
                factory.create_builtin_term(Stdlib::Get),
                allocator.create_pair(
                    factory.create_variable_term(0),
                    factory.create_string_term(allocator.create_static_string(operation_type)),
                ),
            )),
        ),
    )
}

fn parse_operation_variables<T: Expression>(
    variable_definitions: &[VariableDefinition],
    variables: &QueryVariables<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<QueryVariables<T>, String> {
    variable_definitions
        .iter()
        .map(|definition| {
            let value = variables.get(&definition.name).cloned();
            let value = match value {
                Some(value) => Some(value),
                None => match &definition.default_value {
                    Some(value) => Some(parse_value(
                        value,
                        &QueryVariables::<T>::new(),
                        &QueryFragments::new(),
                        factory,
                        allocator,
                    )?),
                    None => None,
                },
            };
            Ok((
                definition.name.clone(),
                validate_variable(value, &definition.name, &definition.var_type, factory)?,
            ))
        })
        .collect::<Result<QueryVariables<T>, _>>()
}

fn validate_variable<T: Expression>(
    value: Option<T>,
    name: &str,
    var_type: &Type,
    factory: &impl ExpressionFactory<T>,
) -> Result<T, String> {
    match var_type {
        Type::NonNullType(_) => {
            value.ok_or_else(|| format!("Missing required query variable: {}", name.to_string()))
        }
        // TODO: Validate query variable types
        // TODO: Differentiate between missing optional variables and null values
        _ => Ok(value.unwrap_or_else(|| factory.create_nil_term())),
    }
}

fn parse_selection_set<T: Expression>(
    selection_set: &SelectionSet,
    variables: &QueryVariables<T>,
    fragments: &QueryFragments<'_>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, String>
where
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
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
                    factory.create_builtin_term(Stdlib::CollectRecord),
                    allocator.create_sized_list(
                        values.len() + 1,
                        once(
                            factory
                                .create_constructor_term(allocator.create_struct_prototype(keys)),
                        )
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
    T::Builtin: From<GraphQlStdlib>,
{
    Ok(factory.create_builtin_term(GraphQlStdlib::FlattenDeep))
}

fn parse_selection_set_fields<T: Expression>(
    selection_set: &SelectionSet,
    variables: &QueryVariables<T>,
    fragments: &QueryFragments<'_>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<Vec<(String, T)>, String>
where
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
{
    selection_set
        .items
        .iter()
        .flat_map(|field| match field {
            // TODO: Support GraphQL @skip and @include directives
            Selection::Field(field) => {
                match parse_field(field, variables, fragments, factory, allocator) {
                    Ok(result) => {
                        let key = field.alias.as_ref().unwrap_or(&field.name).to_string();
                        Either::Left(once(Ok((key, result))))
                    }
                    Err(error) => Either::Left(once(Err(error))),
                }
            }
            Selection::FragmentSpread(fragment) => match fragments.get(&fragment.fragment_name) {
                Some(fragment) => {
                    let fields =
                        parse_fragment_fields(fragment, variables, fragments, factory, allocator);
                    Either::Right(fields.into_iter())
                }
                None => Either::Left(once(Err(format!(
                    "Invalid fragment name: {}",
                    fragment.fragment_name.to_string()
                )))),
            },
            Selection::InlineFragment(_) => Either::Left(once(Err(String::from(
                "Inline fragments not yet implemented",
            )))),
        })
        .collect::<Result<Vec<_>, _>>()
}

fn parse_fragment_fields<T: Expression>(
    fragment: &FragmentDefinition,
    variables: &QueryVariables<T>,
    fragments: &QueryFragments<'_>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> impl IntoIterator<Item = Result<(String, T), String>>
where
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
{
    match parse_selection_set_fields(
        &fragment.selection_set,
        variables,
        fragments,
        factory,
        allocator,
    ) {
        Err(error) => Either::Left(once(Err(error))),
        Ok(fields) => Either::Right(fields.into_iter().map(Ok)),
    }
}

fn parse_field<T: Expression>(
    field: &Field,
    variables: &QueryVariables<T>,
    fragments: &QueryFragments<'_>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, String>
where
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
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
                    factory.create_builtin_term(Stdlib::Get),
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
    variables: &QueryVariables<T>,
    fragments: &QueryFragments<'_>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<ExpressionList<T>, String> {
    let arg_fields = args
        .iter()
        .map(
            |(key, value)| match parse_value(value, variables, fragments, factory, allocator) {
                Ok(value) => Ok((key.to_string(), value)),
                Err(error) => Err(error),
            },
        )
        .collect::<Result<Vec<_>, _>>()?;
    let arg = create_record(arg_fields, factory, allocator);
    Ok(allocator.create_unit_list(arg))
}

fn parse_value<T: Expression>(
    value: &Value,
    variables: &QueryVariables<T>,
    fragments: &QueryFragments<'_>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, String> {
    match value {
        Value::Variable(name) => match variables.get(name) {
            Some(value) => Ok(value.clone()),
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
                        Ok(value) => Ok((key.to_string(), value)),
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
    T::Builtin: From<GraphQlStdlib>,
{
    factory.create_lambda_term(
        1,
        factory.create_application_term(
            factory.create_builtin_term(GraphQlStdlib::DynamicQueryBranch),
            allocator.create_pair(factory.create_variable_term(0), shape),
        ),
    )
}

#[cfg(test)]
mod tests {
    use reflex::core::Uuid;
    use reflex::{
        allocator::DefaultAllocator,
        cache::SubstitutionCache,
        core::{
            evaluate, Applicable, Arity, Builtin, DependencyList, Evaluate, EvaluationCache,
            EvaluationResult, Expression, ExpressionFactory, HeapAllocator, Reducible, Rewritable,
            StateCache, Uid,
        },
        lang::{create_record, SharedTermFactory},
        stdlib::Stdlib,
    };
    use std::convert::{TryFrom, TryInto};

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
        fn arity<T: Expression<Builtin = Self>>(&self) -> Arity {
            match self {
                GraphQlTestBuiltins::Stdlib(term) => term.arity::<T>(),
                GraphQlTestBuiltins::GraphQl(term) => term.arity::<T>(),
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

    #[test]
    fn leaf_queries() {
        let factory = SharedTermFactory::<GraphQlTestBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let root = create_record(
            vec![
                (
                    String::from("query"),
                    create_record(
                        vec![
                            (String::from("first"), factory.create_int_term(3)),
                            (String::from("second"), factory.create_int_term(4)),
                            (String::from("third"), factory.create_int_term(5)),
                        ],
                        &factory,
                        &allocator,
                    ),
                ),
                (String::from("mutation"), factory.create_nil_term()),
                (String::from("subscription"), factory.create_nil_term()),
            ],
            &factory,
            &allocator,
        );
        let variables = Vec::new();
        let query = parse(
            "
                query {
                    second
                    third
                }
            ",
            variables,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = apply_query(query, root, &factory, &allocator);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_record(
                    vec![
                        (String::from("second"), factory.create_int_term(4)),
                        (String::from("third"), factory.create_int_term(5)),
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
            vec![
                (
                    String::from("query"),
                    factory.create_application_term(
                        factory.create_lambda_term(
                            1,
                            create_record(
                                vec![
                                    (
                                        String::from("first"),
                                        factory.create_application_term(
                                            factory.create_builtin_term(Stdlib::Add),
                                            allocator.create_pair(
                                                factory.create_int_term(3),
                                                factory.create_variable_term(0),
                                            ),
                                        ),
                                    ),
                                    (
                                        String::from("second"),
                                        factory.create_application_term(
                                            factory.create_builtin_term(Stdlib::Add),
                                            allocator.create_pair(
                                                factory.create_int_term(4),
                                                factory.create_variable_term(0),
                                            ),
                                        ),
                                    ),
                                    (
                                        String::from("third"),
                                        factory.create_application_term(
                                            factory.create_builtin_term(Stdlib::Add),
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
                (String::from("mutation"), factory.create_nil_term()),
                (String::from("subscription"), factory.create_nil_term()),
            ],
            &factory,
            &allocator,
        );
        let variables = Vec::new();
        let query = parse(
            "
                query {
                    second
                    third
                }
            ",
            variables,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = apply_query(query, root, &factory, &allocator);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_record(
                    vec![
                        (String::from("second"), factory.create_int_term(4 + 10)),
                        (String::from("third"), factory.create_int_term(5 + 10)),
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
            vec![
                (
                    String::from("query"),
                    create_record(
                        vec![
                            (String::from("foo"), factory.create_nil_term()),
                            (
                                String::from("items"),
                                factory.create_list_term(allocator.create_list(vec![
                                    factory.create_int_term(3),
                                    factory.create_int_term(4),
                                    factory.create_int_term(5),
                                ])),
                            ),
                            (String::from("bar"), factory.create_nil_term()),
                        ],
                        &factory,
                        &allocator,
                    ),
                ),
                (String::from("mutation"), factory.create_nil_term()),
                (String::from("subscription"), factory.create_nil_term()),
            ],
            &factory,
            &allocator,
        );
        let variables = Vec::new();
        let query = parse(
            "
                query {
                    items
                }
            ",
            variables,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = apply_query(query, root, &factory, &allocator);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_record(
                    vec![(
                        String::from("items"),
                        factory.create_list_term(allocator.create_list(vec![
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
            vec![
                (
                    String::from("query"),
                    create_record(
                        vec![
                            (String::from("foo"), factory.create_nil_term()),
                            (
                                String::from("items"),
                                factory.create_list_term(allocator.create_list(vec![
                                    factory.create_list_term(allocator.create_list(vec![
                                        factory.create_list_term(allocator.create_list(vec![
                                            factory.create_float_term(1.1),
                                            factory.create_float_term(1.2),
                                            factory.create_float_term(1.3),
                                        ])),
                                        factory.create_list_term(allocator.create_list(vec![
                                            factory.create_float_term(1.4),
                                            factory.create_float_term(1.5),
                                            factory.create_float_term(1.6),
                                        ])),
                                        factory.create_list_term(allocator.create_list(vec![
                                            factory.create_float_term(1.7),
                                            factory.create_float_term(1.8),
                                            factory.create_float_term(1.9),
                                        ])),
                                    ])),
                                    factory.create_list_term(allocator.create_list(vec![
                                        factory.create_list_term(allocator.create_list(vec![
                                            factory.create_float_term(2.1),
                                            factory.create_float_term(2.2),
                                            factory.create_float_term(2.3),
                                        ])),
                                        factory.create_list_term(allocator.create_list(vec![
                                            factory.create_float_term(2.4),
                                            factory.create_float_term(2.5),
                                            factory.create_float_term(2.6),
                                        ])),
                                        factory.create_list_term(allocator.create_list(vec![
                                            factory.create_float_term(2.7),
                                            factory.create_float_term(2.8),
                                            factory.create_float_term(2.9),
                                        ])),
                                    ])),
                                    factory.create_list_term(allocator.create_list(vec![
                                        factory.create_list_term(allocator.create_list(vec![
                                            factory.create_float_term(3.1),
                                            factory.create_float_term(3.2),
                                            factory.create_float_term(3.3),
                                        ])),
                                        factory.create_list_term(allocator.create_list(vec![
                                            factory.create_float_term(3.4),
                                            factory.create_float_term(3.5),
                                            factory.create_float_term(3.6),
                                        ])),
                                        factory.create_list_term(allocator.create_list(vec![
                                            factory.create_float_term(3.7),
                                            factory.create_float_term(3.8),
                                            factory.create_float_term(3.9),
                                        ])),
                                    ])),
                                ])),
                            ),
                            (String::from("bar"), factory.create_nil_term()),
                        ],
                        &factory,
                        &allocator,
                    ),
                ),
                (String::from("mutation"), factory.create_nil_term()),
                (String::from("subscription"), factory.create_nil_term()),
            ],
            &factory,
            &allocator,
        );
        let variables = Vec::new();
        let query = parse(
            "
                query {
                    items
                }
            ",
            variables,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = apply_query(query, root, &factory, &allocator);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_record(
                    vec![(
                        String::from("items"),
                        factory.create_list_term(allocator.create_list(vec![
                            factory.create_list_term(allocator.create_list(vec![
                                factory.create_list_term(allocator.create_list(vec![
                                    factory.create_float_term(1.1),
                                    factory.create_float_term(1.2),
                                    factory.create_float_term(1.3),
                                ])),
                                factory.create_list_term(allocator.create_list(vec![
                                    factory.create_float_term(1.4),
                                    factory.create_float_term(1.5),
                                    factory.create_float_term(1.6),
                                ])),
                                factory.create_list_term(allocator.create_list(vec![
                                    factory.create_float_term(1.7),
                                    factory.create_float_term(1.8),
                                    factory.create_float_term(1.9),
                                ])),
                            ])),
                            factory.create_list_term(allocator.create_list(vec![
                                factory.create_list_term(allocator.create_list(vec![
                                    factory.create_float_term(2.1),
                                    factory.create_float_term(2.2),
                                    factory.create_float_term(2.3),
                                ])),
                                factory.create_list_term(allocator.create_list(vec![
                                    factory.create_float_term(2.4),
                                    factory.create_float_term(2.5),
                                    factory.create_float_term(2.6),
                                ])),
                                factory.create_list_term(allocator.create_list(vec![
                                    factory.create_float_term(2.7),
                                    factory.create_float_term(2.8),
                                    factory.create_float_term(2.9),
                                ])),
                            ])),
                            factory.create_list_term(allocator.create_list(vec![
                                factory.create_list_term(allocator.create_list(vec![
                                    factory.create_float_term(3.1),
                                    factory.create_float_term(3.2),
                                    factory.create_float_term(3.3),
                                ])),
                                factory.create_list_term(allocator.create_list(vec![
                                    factory.create_float_term(3.4),
                                    factory.create_float_term(3.5),
                                    factory.create_float_term(3.6),
                                ])),
                                factory.create_list_term(allocator.create_list(vec![
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
