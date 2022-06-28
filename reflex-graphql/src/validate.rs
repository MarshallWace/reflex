// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{hash_map::Entry, HashMap},
    iter::{empty, once, Rev},
    vec::IntoIter,
};

use graphql_parser::schema;
use reflex_json::{JsonMap, JsonValue};

use crate::ast::{common::Value, query};

use crate::{
    create_json_error_object, get_query_root_operation, GraphQlExtensions, GraphQlQuery,
    GraphQlQueryTransform, GraphQlSchemaTypes, GraphQlText,
};

type GraphQlQueryFragments<'a> = HashMap<&'a String, &'a query::FragmentDefinition>;

/// GraphQL transform that validates the incoming GraphQL query against the provided GraphQL schema.
///
/// Any optional schema field arguments not supplied by the query will be substituted with null values.
///
/// # Examples
///
/// ```
/// use reflex_graphql::validate::{parse_graphql_schema_types, ValidateQueryGraphQlTransform};
///
/// let schema = graphql_parser::parse_schema::<'static, String>("
///     type Query {
///       greet(user: String): String!
///     }
/// ").unwrap();
///
/// let schema_types = parse_graphql_schema_types(schema).unwrap();
/// let transform = ValidateQueryGraphQlTransform::new(schema_types);
/// ```
#[derive(Clone)]
pub struct ValidateQueryGraphQlTransform<'schema, TSchema: GraphQlText<'schema>> {
    schema_types: GraphQlSchemaTypes<'schema, TSchema>,
}
impl<'schema, TSchema: GraphQlText<'schema>> ValidateQueryGraphQlTransform<'schema, TSchema> {
    pub fn new(schema_types: GraphQlSchemaTypes<'schema, TSchema>) -> Self {
        Self { schema_types }
    }
}
impl<'schema, TSchema: GraphQlText<'schema>> GraphQlQueryTransform
    for ValidateQueryGraphQlTransform<'schema, TSchema>
{
    fn transform(
        &self,
        document: GraphQlQuery,
        extensions: GraphQlExtensions,
    ) -> Result<(GraphQlQuery, GraphQlExtensions), String> {
        validate_graphql_query(&document, &self.schema_types)
            .map(|transformed| match transformed {
                Some(document) => document,
                None => document,
            })
            .map(|document| (document, extensions))
    }
}

fn get_builtin_scalars<'a, T: GraphQlText<'a>>(
) -> impl IntoIterator<Item = schema::ScalarType<'a, T>> {
    ["Int", "Float", "String", "Boolean", "Id"]
        .into_iter()
        .map(|name| schema::ScalarType {
            position: Default::default(),
            description: None,
            name: String::from(name).into(),
            directives: Default::default(),
        })
}

pub fn parse_graphql_schema_types<'schema, TSchema: GraphQlText<'schema>>(
    schema: schema::Document<'schema, TSchema>,
) -> Result<GraphQlSchemaTypes<'schema, TSchema>, String> {
    let builtin_scalar_definitions: Vec<schema::Definition<'schema, TSchema>> =
        get_builtin_scalars()
            .into_iter()
            .map(|scalar_type| {
                schema::Definition::TypeDefinition(schema::TypeDefinition::Scalar(scalar_type))
            })
            .collect();
    let schema_definitions = {
        // Ensure all type extensions come after their corresponding definitions
        let (extensions, mut definitions): (Vec<_>, Vec<_>) = schema
            .definitions
            .into_iter()
            .partition(|definition| matches!(definition, schema::Definition::TypeExtension(_)));
        definitions.extend(extensions);
        definitions
    };
    let (schema_definition, schema_types) = builtin_scalar_definitions
        .into_iter()
        .chain(schema_definitions)
        .fold(Ok((None, HashMap::new())), |results, definition| {
            let (schema_definition, mut schema_types) = results?;
            match definition {
                schema::Definition::DirectiveDefinition(_) => {
                    // TODO: support schema directive validation
                    Ok((schema_definition, schema_types))
                }
                schema::Definition::SchemaDefinition(definition) => {
                    if let Some(_existing) = schema_definition {
                        Err(String::from(
                            "Multiple schema definitions not currently supported",
                        ))
                    } else {
                        Ok((Some(definition), schema_types))
                    }
                }
                schema::Definition::TypeDefinition(definition) => {
                    let type_name = get_schema_type_name(&definition);
                    match schema_types.entry(type_name.to_string()) {
                        Entry::Occupied(_) => Err(format!(
                            "Unable to redeclare existing schema type {}",
                            type_name.as_ref(),
                        )),
                        Entry::Vacant(entry) => {
                            entry.insert(definition);
                            Ok((schema_definition, schema_types))
                        }
                    }
                }
                schema::Definition::TypeExtension(definition) => {
                    let type_name = get_schema_type_extension_name(&definition);
                    match schema_types.entry(type_name.to_string()) {
                        Entry::Vacant(_) => Err(format!(
                            "Unable to extend undeclared schema type {}",
                            type_name.as_ref(),
                        )),
                        Entry::Occupied(mut entry) => {
                            extend_schema_type(entry.get_mut(), definition)?;
                            Ok((schema_definition, schema_types))
                        }
                    }
                }
            }
        })?;
    let query_root = schema_definition
        .as_ref()
        .and_then(|definition| {
            definition.query.as_ref().map(|type_name| {
                if schema_types.contains_key(type_name.as_ref()) {
                    Ok(type_name.to_string())
                } else {
                    Err(format!(
                        "Undeclared query operation type: {}",
                        type_name.as_ref()
                    ))
                }
            })
        })
        .or_else(|| {
            if schema_types.contains_key("Query") {
                Some(Ok(String::from("Query")))
            } else {
                None
            }
        })
        .transpose()?;
    let mutation_root = schema_definition
        .as_ref()
        .and_then(|definition| {
            definition.mutation.as_ref().map(|type_name| {
                if schema_types.contains_key(type_name.as_ref()) {
                    Ok(type_name.to_string())
                } else {
                    Err(format!(
                        "Undeclared mutation operation type: {}",
                        type_name.as_ref()
                    ))
                }
            })
        })
        .or_else(|| {
            if schema_types.contains_key("Mutation") {
                Some(Ok(String::from("Mutation")))
            } else {
                None
            }
        })
        .transpose()?;
    let subscription_root = schema_definition
        .as_ref()
        .and_then(|definition| {
            definition.subscription.as_ref().map(|type_name| {
                if schema_types.contains_key(type_name.as_ref()) {
                    Ok(type_name.to_string())
                } else {
                    Err(format!(
                        "Undeclared subscription operation type: {}",
                        type_name.as_ref()
                    ))
                }
            })
        })
        .or_else(|| {
            if schema_types.contains_key("Subscription") {
                Some(Ok(String::from("Subscription")))
            } else {
                None
            }
        })
        .transpose()?;
    Ok(GraphQlSchemaTypes {
        types: schema_types,
        query: query_root,
        mutation: mutation_root,
        subscription: subscription_root,
    })
}

fn extend_schema_type<'schema, TSchema: GraphQlText<'schema>>(
    schema_type: &mut schema::TypeDefinition<'schema, TSchema>,
    extension: schema::TypeExtension<'schema, TSchema>,
) -> Result<(), String> {
    match (schema_type, extension) {
        (schema::TypeDefinition::Scalar(schema_type), schema::TypeExtension::Scalar(extension)) => {
            schema_type.directives.extend(extension.directives);
            Ok(())
        }
        (schema::TypeDefinition::Object(schema_type), schema::TypeExtension::Object(extension)) => {
            schema_type
                .implements_interfaces
                .extend(extension.implements_interfaces);
            schema_type.directives.extend(extension.directives);
            schema_type.fields.extend(extension.fields);
            Ok(())
        }
        (
            schema::TypeDefinition::Interface(schema_type),
            schema::TypeExtension::Interface(extension),
        ) => {
            schema_type
                .implements_interfaces
                .extend(extension.implements_interfaces);
            schema_type.directives.extend(extension.directives);
            schema_type.fields.extend(extension.fields);
            Ok(())
        }
        (schema::TypeDefinition::Union(schema_type), schema::TypeExtension::Union(extension)) => {
            schema_type.directives.extend(extension.directives);
            schema_type.types.extend(extension.types);
            Ok(())
        }
        (schema::TypeDefinition::Enum(schema_type), schema::TypeExtension::Enum(extension)) => {
            schema_type.directives.extend(extension.directives);
            schema_type.values.extend(extension.values);
            Ok(())
        }
        (
            schema::TypeDefinition::InputObject(schema_type),
            schema::TypeExtension::InputObject(extension),
        ) => {
            schema_type.directives.extend(extension.directives);
            schema_type.fields.extend(extension.fields);
            Ok(())
        }
        (schema_type, _) => Err(format!(
            "Invalid extension for schema type {}",
            get_schema_type_name(schema_type).as_ref()
        )),
    }
}

pub fn validate_graphql_query<'schema, TSchema: GraphQlText<'schema>>(
    document: &query::Document,
    schema_types: &GraphQlSchemaTypes<'schema, TSchema>,
) -> Result<Option<query::Document>, String> {
    let fragments = parse_query_fragments(document);
    get_query_root_operation(document).and_then(|operation| {
        validate_operation(operation, schema_types, &fragments).map(|result| {
            result.and_then(|operation| {
                let updated_root_operation = query::Definition::Operation(operation);
                let updated_document = find_replace_list_item(
                    &document.definitions,
                    |definition| matches!(definition, query::Definition::Operation(_)),
                    updated_root_operation,
                )
                .map(|definitions| query::Document { definitions });
                updated_document
            })
        })
    })
}

fn parse_query_fragments<'a>(document: &'a query::Document) -> GraphQlQueryFragments<'a> {
    document
        .definitions
        .iter()
        .filter_map(|definition| match definition {
            query::Definition::Fragment(fragment) => Some((&fragment.name, fragment)),
            _ => None,
        })
        .collect::<GraphQlQueryFragments<'_>>()
}

struct GraphQlResultValidationError {
    message: String,
    path: GraphQlQueryPath,
}
impl GraphQlResultValidationError {
    fn new(message: String) -> Self {
        Self {
            message,
            path: Default::default(),
        }
    }
    fn with_path_prefix(self, segment: GraphQlQueryPathSegment) -> Self {
        let Self { message, path } = self;
        Self {
            message,
            path: path.prepend(segment),
        }
    }
    fn path_len(&self) -> usize {
        self.path.len()
    }
    fn into_json(self) -> JsonValue {
        create_json_error_object(
            format!("GraphQL validation error: {}", self.message),
            once((
                String::from("path"),
                JsonValue::Array(
                    self.path
                        .into_iter()
                        .map(|segment| segment.into_json())
                        .collect(),
                ),
            )),
        )
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
struct GraphQlQueryPath {
    segments: Vec<GraphQlQueryPathSegment>,
}
impl Default for GraphQlQueryPath {
    fn default() -> Self {
        Self {
            segments: Default::default(),
        }
    }
}
impl GraphQlQueryPath {
    fn prepend(self, segment: GraphQlQueryPathSegment) -> Self {
        let Self { mut segments } = self;
        segments.push(segment);
        Self { segments }
    }
    fn len(&self) -> usize {
        self.segments.len()
    }
}
impl IntoIterator for GraphQlQueryPath {
    type Item = GraphQlQueryPathSegment;
    type IntoIter = Rev<IntoIter<Self::Item>>;
    fn into_iter(self) -> Self::IntoIter {
        let Self { segments } = self;
        segments.into_iter().rev()
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
enum GraphQlQueryPathSegment {
    Field(String),
    Index(usize),
}
impl GraphQlQueryPathSegment {
    fn into_json(self) -> JsonValue {
        match self {
            Self::Field(key) => JsonValue::String(key),
            Self::Index(index) => JsonValue::Number(index.into()),
        }
    }
}

pub fn validate_graphql_result<'schema, TSchema: GraphQlText<'schema>>(
    payload: &JsonValue,
    operation: &query::Document,
    schema_types: &GraphQlSchemaTypes<'schema, TSchema>,
) -> Result<(), Vec<JsonValue>> {
    let fragments = parse_query_fragments(operation);
    validate_graphql_result_root(payload, operation, schema_types, &fragments)
        .map_err(|errors| errors.into_iter().map(|err| err.into_json()).collect())
}

fn validate_graphql_result_root<'schema, TSchema: GraphQlText<'schema>>(
    payload: &JsonValue,
    operation: &query::Document,
    schema_types: &GraphQlSchemaTypes<'schema, TSchema>,
    fragments: &GraphQlQueryFragments<'_>,
) -> Result<(), Vec<GraphQlResultValidationError>> {
    let operation_root =
        get_query_root_operation(operation).map_err(|err| vec![format_validation_error(err)])?;
    let operation_root_type = get_operation_root_type(operation_root, schema_types)
        .map_err(|err| vec![format_validation_error(err)])?;
    let selection_set = match operation_root {
        query::OperationDefinition::Query(operation) => &operation.selection_set,
        query::OperationDefinition::Mutation(operation) => &operation.selection_set,
        query::OperationDefinition::Subscription(operation) => &operation.selection_set,
        query::OperationDefinition::SelectionSet(selection_set) => selection_set,
    };
    let errors = validate_result_selection_set(
        payload,
        selection_set,
        operation_root_type,
        schema_types,
        fragments,
    );
    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

fn validate_result_selection_set<'schema, TSchema: GraphQlText<'schema>>(
    payload: &JsonValue,
    selection_set: &query::SelectionSet,
    schema_type: &schema::TypeDefinition<'schema, TSchema>,
    schema_types: &GraphQlSchemaTypes<'schema, TSchema>,
    fragments: &GraphQlQueryFragments<'_>,
) -> Vec<GraphQlResultValidationError> {
    match schema_type {
        schema::TypeDefinition::Scalar(schema_type) => validate_result_scalar_selection_set(
            payload,
            selection_set,
            schema_type,
            schema_types,
            fragments,
        ),
        schema::TypeDefinition::Enum(schema_type) => validate_result_enum_selection_set(
            payload,
            selection_set,
            schema_type,
            schema_types,
            fragments,
        ),
        schema::TypeDefinition::Object(schema_type) => validate_result_object_selection_set(
            payload,
            selection_set,
            schema_type,
            schema_types,
            fragments,
        ),
        schema::TypeDefinition::Interface(_) => once(format_validation_error(
            "Interface field types not currently supported",
        ))
        .collect(),
        schema::TypeDefinition::Union(_) => once(format_validation_error(
            "Union field types not currently supported",
        ))
        .collect(),
        schema::TypeDefinition::InputObject(schema_type) => once(format_validation_error(format!(
            "Invalid field type: {}",
            schema_type.name.as_ref()
        )))
        .collect(),
    }
}

fn validate_result_scalar_selection_set<'schema, TSchema: GraphQlText<'schema>>(
    payload: &JsonValue,
    _selection_set: &query::SelectionSet,
    schema_type: &schema::ScalarType<'schema, TSchema>,
    _schema_types: &GraphQlSchemaTypes<'schema, TSchema>,
    _fragments: &GraphQlQueryFragments<'_>,
) -> Vec<GraphQlResultValidationError> {
    match schema_type.name.as_ref() {
        "Int" => validate_result_scalar_int_selection_set(payload, schema_type),
        "Float" => validate_result_scalar_float_selection_set(payload, schema_type),
        "String" => validate_result_scalar_string_selection_set(payload, schema_type),
        "Boolean" => validate_result_scalar_boolean_selection_set(payload, schema_type),
        "Id" => validate_result_scalar_id_selection_set(payload, schema_type),
        _ => empty().collect(),
    }
}

fn validate_result_scalar_int_selection_set<'schema, TSchema: GraphQlText<'schema>>(
    payload: &JsonValue,
    schema_type: &schema::ScalarType<'schema, TSchema>,
) -> Vec<GraphQlResultValidationError> {
    match parse_json_integer_value(payload) {
        Some(_) => empty().collect(),
        None => once(format_type_validation_error(
            Some(schema_type.name.as_ref()),
            Some(payload),
        ))
        .collect(),
    }
}

fn parse_json_integer_value(payload: &JsonValue) -> Option<i64> {
    match payload {
        JsonValue::Number(value) => value
            .as_i64()
            .or_else(|| value.as_f64().and_then(|value| as_i64(value))),
        _ => None,
    }
}

fn as_i64(value: f64) -> Option<i64> {
    let int_value = value as i64;
    if value == int_value as f64 {
        Some(int_value)
    } else {
        None
    }
}

fn validate_result_scalar_float_selection_set<'schema, TSchema: GraphQlText<'schema>>(
    payload: &JsonValue,
    schema_type: &schema::ScalarType<'schema, TSchema>,
) -> Vec<GraphQlResultValidationError> {
    match payload.as_f64() {
        Some(_) => empty().collect(),
        None => once(format_type_validation_error(
            Some(schema_type.name.as_ref()),
            Some(payload),
        ))
        .collect(),
    }
}

fn validate_result_scalar_string_selection_set<'schema, TSchema: GraphQlText<'schema>>(
    payload: &JsonValue,
    schema_type: &schema::ScalarType<'schema, TSchema>,
) -> Vec<GraphQlResultValidationError> {
    match payload.as_str() {
        Some(_) => empty().collect(),
        None => once(format_type_validation_error(
            Some(schema_type.name.as_ref()),
            Some(payload),
        ))
        .collect(),
    }
}

fn validate_result_scalar_boolean_selection_set<'schema, TSchema: GraphQlText<'schema>>(
    payload: &JsonValue,
    schema_type: &schema::ScalarType<'schema, TSchema>,
) -> Vec<GraphQlResultValidationError> {
    match payload.as_bool() {
        Some(_) => empty().collect(),
        None => once(format_type_validation_error(
            Some(schema_type.name.as_ref()),
            Some(payload),
        ))
        .collect(),
    }
}

fn validate_result_scalar_id_selection_set<'schema, TSchema: GraphQlText<'schema>>(
    payload: &JsonValue,
    schema_type: &schema::ScalarType<'schema, TSchema>,
) -> Vec<GraphQlResultValidationError> {
    match payload.as_str() {
        Some(_) => empty().collect(),
        None => once(format_type_validation_error(
            Some(schema_type.name.as_ref()),
            Some(payload),
        ))
        .collect(),
    }
}

fn validate_result_enum_selection_set<'schema, TSchema: GraphQlText<'schema>>(
    payload: &JsonValue,
    _selection_set: &query::SelectionSet,
    schema_type: &schema::EnumType<'schema, TSchema>,
    _schema_types: &GraphQlSchemaTypes<'schema, TSchema>,
    _fragments: &GraphQlQueryFragments<'_>,
) -> Vec<GraphQlResultValidationError> {
    let parsed_value = payload
        .as_str()
        .and_then(|value| match_enum_value(value, schema_type));
    match parsed_value {
        Some(_) => empty().collect(),
        None => once(format_type_validation_error(
            Some(schema_type.name.as_ref()),
            Some(payload),
        ))
        .collect(),
    }
}

fn match_enum_value<'a, 'schema, TSchema: GraphQlText<'schema>>(
    value: &str,
    enum_type: &'a schema::EnumType<'schema, TSchema>,
) -> Option<&'a TSchema> {
    enum_type
        .values
        .iter()
        .map(|variant| &variant.name)
        .find(|&enum_value| enum_value.as_ref() == value)
}

fn validate_result_object_selection_set<'schema, TSchema: GraphQlText<'schema>>(
    payload: &JsonValue,
    selection_set: &query::SelectionSet,
    schema_type: &schema::ObjectType<'schema, TSchema>,
    schema_types: &GraphQlSchemaTypes<'schema, TSchema>,
    fragments: &GraphQlQueryFragments<'_>,
) -> Vec<GraphQlResultValidationError> {
    match payload.as_object() {
        None => once(format_type_validation_error(
            Some(schema_type.name.as_ref()),
            Some(payload),
        ))
        .collect(),
        Some(payload_fields) => {
            // FIXME: Return GraphQL validation errors for result fields not specified in query
            let unexpected_field_errors = empty();
            validate_result_object_field_selection_set(
                payload_fields,
                selection_set,
                schema_type,
                schema_types,
                fragments,
            )
            .into_iter()
            .chain(unexpected_field_errors)
            .collect()
        }
    }
}

fn validate_result_object_field_selection_set<'schema, TSchema: GraphQlText<'schema>>(
    payload_fields: &JsonMap<String, JsonValue>,
    selection_set: &query::SelectionSet,
    schema_type: &schema::ObjectType<'schema, TSchema>,
    schema_types: &GraphQlSchemaTypes<'schema, TSchema>,
    fragments: &GraphQlQueryFragments<'_>,
) -> Vec<GraphQlResultValidationError> {
    selection_set
        .items
        .iter()
        .flat_map(|selection| -> Vec<_> {
            match selection {
                query::Selection::Field(query_field) => {
                    let schema_field_name = &query_field.name;
                    let query_field_name = get_aliased_field_name(query_field);
                    let expected_type = get_object_field_type(schema_type, schema_field_name);
                    match (expected_type, payload_fields.get(query_field_name.as_str())) {
                        (None, received) => {
                            once(format_type_validation_error(None, received)).collect()
                        }
                        (Some(expected), None) => {
                            once(format_field_type_validation_error(expected, None)).collect()
                        }
                        (Some(expected), Some(received)) => validate_result_field(
                            received,
                            &query_field.selection_set,
                            expected,
                            schema_types,
                            fragments,
                        ),
                    }
                    .into_iter()
                    .map(|err| {
                        err.with_path_prefix(GraphQlQueryPathSegment::Field(String::from(
                            query_field
                                .alias
                                .as_ref()
                                .unwrap_or(&query_field.name)
                                .as_str(),
                        )))
                    })
                    .collect()
                }
                query::Selection::FragmentSpread(fragment) => {
                    match get_named_fragment(&fragment.fragment_name, fragments) {
                        Err(err) => once(format_validation_error(err)).collect(),
                        Ok(fragment) => validate_result_object_field_selection_set(
                            payload_fields,
                            &fragment.selection_set,
                            schema_type,
                            schema_types,
                            fragments,
                        ),
                    }
                }
                query::Selection::InlineFragment(_) => once(format_validation_error(
                    "Inline fragments not currently supported",
                ))
                .collect(),
            }
        })
        .collect()
}

fn validate_result_field<'schema, TSchema: GraphQlText<'schema>>(
    payload: &JsonValue,
    selection_set: &query::SelectionSet,
    schema_type: &schema::Type<'schema, TSchema>,
    schema_types: &GraphQlSchemaTypes<'schema, TSchema>,
    fragments: &GraphQlQueryFragments<'_>,
) -> Vec<GraphQlResultValidationError> {
    match schema_type {
        schema::Type::NamedType(type_name) => {
            if payload.is_null() {
                empty().collect()
            } else {
                match schema_types.get_type(type_name.as_ref()) {
                    None => once(format_field_type_validation_error(
                        schema_type,
                        Some(payload),
                    ))
                    .collect(),
                    Some(inner_type) => validate_result_selection_set(
                        payload,
                        selection_set,
                        inner_type,
                        schema_types,
                        fragments,
                    ),
                }
            }
        }
        schema::Type::NonNullType(inner_type) => {
            if payload.is_null() {
                once(format_field_type_validation_error(
                    schema_type,
                    Some(payload),
                ))
                .collect()
            } else {
                validate_result_field(payload, selection_set, inner_type, schema_types, fragments)
                    .into_iter()
                    .map(|err| {
                        if err.path_len() == 0 {
                            format_field_type_validation_error(schema_type, Some(payload))
                        } else {
                            err
                        }
                    })
                    .collect()
            }
        }
        schema::Type::ListType(inner_type) => {
            if payload.is_null() {
                empty().collect()
            } else {
                match payload.as_array() {
                    None => once(format_field_type_validation_error(
                        schema_type,
                        Some(payload),
                    ))
                    .collect(),
                    Some(items) => items
                        .iter()
                        .enumerate()
                        .flat_map(|(index, item)| {
                            validate_result_field(
                                item,
                                selection_set,
                                inner_type,
                                schema_types,
                                fragments,
                            )
                            .into_iter()
                            .map(move |err| {
                                err.with_path_prefix(GraphQlQueryPathSegment::Index(index))
                            })
                        })
                        .collect(),
                }
            }
        }
    }
}

fn get_object_field_type<'a, 'schema, TSchema: GraphQlText<'schema>>(
    schema_type: &'a schema::ObjectType<'schema, TSchema>,
    field_name: &String,
) -> Option<&'a schema::Type<'schema, TSchema>> {
    let field_name = field_name.as_str();
    schema_type
        .fields
        .iter()
        .find(|&field| field.name.as_ref() == field_name)
        .map(|field| &field.field_type)
}

fn get_aliased_field_name<'a>(field: &'a query::Field) -> &'a String {
    field.alias.as_ref().unwrap_or(&field.name)
}

fn format_field_type_validation_error<'schema, TSchema: GraphQlText<'schema>>(
    expected: &schema::Type<'schema, TSchema>,
    received: Option<&JsonValue>,
) -> GraphQlResultValidationError {
    format_type_validation_error(Some(&format_field_type_name(expected)), received)
}

fn format_field_type_name<'a, T: GraphQlText<'a>>(field_type: &schema::Type<'a, T>) -> String {
    match field_type {
        schema::Type::NamedType(inner) => format!("{}", inner.as_ref()),
        schema::Type::ListType(inner) => format!("[{}]", format_field_type_name(inner)),
        schema::Type::NonNullType(inner) => format!("{}!", format_field_type_name(inner)),
    }
}

fn format_type_validation_error(
    expected: Option<&str>,
    received: Option<&JsonValue>,
) -> GraphQlResultValidationError {
    format_validation_error(match (expected, received) {
        (Some(expected), Some(received)) => format!(
            "Expected {}, received {}",
            expected,
            format_json_value(received)
        ),
        (Some(expected), None) => format!("Missing field, expected {}", expected,),
        (None, Some(received)) => {
            format!("Unexpected field, received {}", format_json_value(received))
        }
        _ => String::from("Invalid field"),
    })
}

fn format_json_value(value: &JsonValue) -> String {
    match value {
        JsonValue::Null | JsonValue::Bool(_) | JsonValue::Number(_) | JsonValue::String(_) => {
            value.to_string()
        }
        JsonValue::Array(items) => format_json_array_value(items),
        JsonValue::Object(fields) => format_json_object_value(fields),
    }
}

fn format_json_array_value(items: &[JsonValue]) -> String {
    let mut items = items.iter();
    match items.next() {
        None => String::from("[]"),
        Some(first_item) => match items.next() {
            None => format!("[{}]", format_json_value(first_item)),
            Some(_second_item) => format!("[{}, ...]", format_json_value(first_item)),
        },
    }
}

fn format_json_object_value(fields: &reflex_json::JsonMap<String, JsonValue>) -> String {
    if fields.is_empty() {
        String::from("")
    } else {
        format!(
            "{{ {} }}",
            fields
                .iter()
                .map(|(key, _)| key.as_str())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

fn format_validation_error(message: impl Into<String>) -> GraphQlResultValidationError {
    GraphQlResultValidationError::new(message.into())
}

fn get_operation_root_type<'a, 'schema, TSchema: GraphQlText<'schema>>(
    operation: &query::OperationDefinition,
    schema_types: &'a GraphQlSchemaTypes<'schema, TSchema>,
) -> Result<&'a schema::TypeDefinition<'schema, TSchema>, String> {
    match operation {
        query::OperationDefinition::Query(_) | query::OperationDefinition::SelectionSet(_) => {
            schema_types.query.as_ref()
        }
        query::OperationDefinition::Mutation(_) => schema_types.mutation.as_ref(),
        query::OperationDefinition::Subscription(_) => schema_types.subscription.as_ref(),
    }
    .and_then(|root_type_name| schema_types.get_type(root_type_name))
    .ok_or_else(|| {
        format!(
            "Unsupported root operation type: {}",
            format_operation_root_type(operation)
        )
    })
}

fn format_operation_root_type(operation: &query::OperationDefinition) -> &'static str {
    match operation {
        query::OperationDefinition::Query(_) | query::OperationDefinition::SelectionSet(_) => {
            "query"
        }
        query::OperationDefinition::Mutation(_) => "mutation",
        query::OperationDefinition::Subscription(_) => "subscription",
    }
}

fn get_schema_type_name<'a, 'schema, TSchema: GraphQlText<'schema>>(
    schema_type: &'a schema::TypeDefinition<'schema, TSchema>,
) -> &'a TSchema {
    match schema_type {
        schema::TypeDefinition::Scalar(schema_type) => &schema_type.name,
        schema::TypeDefinition::Object(schema_type) => &schema_type.name,
        schema::TypeDefinition::Interface(schema_type) => &schema_type.name,
        schema::TypeDefinition::Union(schema_type) => &schema_type.name,
        schema::TypeDefinition::Enum(schema_type) => &schema_type.name,
        schema::TypeDefinition::InputObject(schema_type) => &schema_type.name,
    }
}

fn get_schema_type_extension_name<'a, 'schema, TSchema: GraphQlText<'schema>>(
    schema_type: &'a schema::TypeExtension<'schema, TSchema>,
) -> &'a TSchema {
    match schema_type {
        schema::TypeExtension::Scalar(schema_type) => &schema_type.name,
        schema::TypeExtension::Object(schema_type) => &schema_type.name,
        schema::TypeExtension::Interface(schema_type) => &schema_type.name,
        schema::TypeExtension::Union(schema_type) => &schema_type.name,
        schema::TypeExtension::Enum(schema_type) => &schema_type.name,
        schema::TypeExtension::InputObject(schema_type) => &schema_type.name,
    }
}

fn validate_operation<'schema, TSchema: GraphQlText<'schema>>(
    operation: &query::OperationDefinition,
    schema_types: &GraphQlSchemaTypes<'schema, TSchema>,
    fragments: &GraphQlQueryFragments<'_>,
) -> Result<Option<query::OperationDefinition>, String> {
    let operation_root_type = get_operation_root_type(operation, schema_types)?;
    match operation {
        query::OperationDefinition::Query(operation) => validate_selection_set(
            &operation.selection_set,
            operation_root_type,
            schema_types,
            fragments,
        )
        .map(|result| {
            result.map(|selection_set| {
                query::OperationDefinition::Query(query::Query {
                    selection_set,
                    ..operation.clone()
                })
            })
        }),
        query::OperationDefinition::Mutation(operation) => validate_selection_set(
            &operation.selection_set,
            operation_root_type,
            schema_types,
            fragments,
        )
        .map(|result| {
            result.map(|selection_set| {
                query::OperationDefinition::Mutation(query::Mutation {
                    selection_set,
                    ..operation.clone()
                })
            })
        }),
        query::OperationDefinition::Subscription(operation) => validate_selection_set(
            &operation.selection_set,
            operation_root_type,
            schema_types,
            fragments,
        )
        .map(|result| {
            result.map(|selection_set| {
                query::OperationDefinition::Subscription(query::Subscription {
                    selection_set,
                    ..operation.clone()
                })
            })
        }),
        query::OperationDefinition::SelectionSet(root) => {
            validate_selection_set(root, operation_root_type, schema_types, fragments)
                .map(|result| result.map(query::OperationDefinition::SelectionSet))
        }
    }
}

fn validate_selection_set<'schema, TSchema: GraphQlText<'schema>>(
    selection_set: &query::SelectionSet,
    schema_type: &schema::TypeDefinition<'schema, TSchema>,
    schema_types: &GraphQlSchemaTypes<'schema, TSchema>,
    fragments: &GraphQlQueryFragments<'_>,
) -> Result<Option<query::SelectionSet>, String> {
    match schema_type {
        schema::TypeDefinition::Scalar(schema_type) => {
            validate_scalar_selection_set(selection_set, schema_type)
        }
        schema::TypeDefinition::Object(schema_type) => {
            validate_object_selection_set(selection_set, schema_type, schema_types, fragments)
        }
        schema::TypeDefinition::Interface(_) => Err(String::from(
            "Interface field types not currently supported",
        )),
        schema::TypeDefinition::Union(_) => {
            Err(String::from("Union field types not currently supported"))
        }
        schema::TypeDefinition::Enum(schema_type) => {
            validate_enum_selection_set(selection_set, schema_type)
        }
        schema::TypeDefinition::InputObject(schema_type) => {
            Err(format!("Invalid field type: {}", schema_type.name.as_ref()))
        }
    }
}

fn validate_scalar_selection_set<'schema, TSchema: GraphQlText<'schema>>(
    selection_set: &query::SelectionSet,
    schema_type: &schema::ScalarType<'schema, TSchema>,
) -> Result<Option<query::SelectionSet>, String> {
    if selection_set.items.is_empty() {
        Ok(None)
    } else {
        Err(format!(
            "Scalar type {} does not support child selections",
            schema_type.name.as_ref()
        ))
    }
}

fn validate_enum_selection_set<'schema, TSchema: GraphQlText<'schema>>(
    selection_set: &query::SelectionSet,
    schema_type: &schema::EnumType<'schema, TSchema>,
) -> Result<Option<query::SelectionSet>, String> {
    if selection_set.items.is_empty() {
        Ok(None)
    } else {
        Err(format!(
            "Enum type {} does not support child selections",
            schema_type.name.as_ref()
        ))
    }
}

fn validate_object_selection_set<'schema, TSchema: GraphQlText<'schema>>(
    selection_set: &query::SelectionSet,
    schema_type: &schema::ObjectType<'schema, TSchema>,
    schema_types: &GraphQlSchemaTypes<'schema, TSchema>,
    fragments: &GraphQlQueryFragments<'_>,
) -> Result<Option<query::SelectionSet>, String> {
    if selection_set.items.is_empty() {
        Err(String::from("No fields selected"))
    } else {
        collect_optionally_transformed_items(
            &selection_set.items,
            |item| validate_object_selection(item, schema_type, schema_types, fragments),
            |item| vec![item.clone()],
        )
        .map(|result| {
            result.map(|items| query::SelectionSet {
                items: items.into_iter().flatten().collect::<Vec<_>>(),
                ..selection_set.clone()
            })
        })
    }
}

fn validate_object_selection<'schema, TSchema: GraphQlText<'schema>>(
    selection: &query::Selection,
    schema_type: &schema::ObjectType<'schema, TSchema>,
    schema_types: &GraphQlSchemaTypes<'schema, TSchema>,
    fragments: &GraphQlQueryFragments<'_>,
) -> Result<Option<Vec<query::Selection>>, String> {
    match selection {
        query::Selection::Field(field) => {
            validate_object_field_selection(field, schema_type, schema_types, fragments)
                .map(|result| result.map(|field| vec![query::Selection::Field(field)]))
        }
        query::Selection::FragmentSpread(fragment) => validate_object_fragment_spread_selection(
            fragment,
            schema_type,
            schema_types,
            fragments,
        ),
        query::Selection::InlineFragment(_) => {
            Err(String::from("Inline fragments not currently supported"))
        }
    }
}

fn validate_object_fragment_spread_selection<'schema, TSchema: GraphQlText<'schema>>(
    fragment: &query::FragmentSpread,
    schema_type: &schema::ObjectType<'schema, TSchema>,
    schema_types: &GraphQlSchemaTypes<'schema, TSchema>,
    fragments: &GraphQlQueryFragments<'_>,
) -> Result<Option<Vec<query::Selection>>, String> {
    get_named_fragment(&fragment.fragment_name, fragments).and_then(|fragment| {
        validate_object_selection_set(
            &fragment.selection_set,
            schema_type,
            schema_types,
            fragments,
        )
        .map(|result| result.map(|selection_set| selection_set.items))
    })
}

fn get_named_fragment<'a, 'fragments>(
    fragment_name: &String,
    fragments: &'a GraphQlQueryFragments<'fragments>,
) -> Result<&'a &'fragments query::FragmentDefinition, String> {
    fragments
        .get(fragment_name)
        .ok_or_else(|| format!("Undefined fragment: {}", fragment_name.as_str()))
}

fn validate_object_field_selection<'schema, TSchema: GraphQlText<'schema>>(
    query_field: &query::Field,
    schema_type: &schema::ObjectType<'schema, TSchema>,
    schema_types: &GraphQlSchemaTypes<'schema, TSchema>,
    fragments: &GraphQlQueryFragments<'_>,
) -> Result<Option<query::Field>, String> {
    let schema_field = schema_type
        .fields
        .iter()
        .find(|schema_field| schema_field.name.as_ref() == query_field.name.as_str())
        .ok_or_else(|| {
            format!(
                "Field \"{}\" not found on type {}",
                query_field.name.as_str(),
                schema_type.name.as_ref()
            )
        })?;
    let field_type_name = get_type_identifier_name(&schema_field.field_type);
    let field_type = schema_types
        .get_type(field_type_name.as_ref())
        .ok_or_else(|| format!("Undefined field type: {}", field_type_name.as_ref()))?;
    let arguments = validate_object_field_arguments(query_field, schema_field).map_err(|err| {
        format!(
            "Invalid arguments for field \"{}\" on type {}: {}",
            query_field.name.as_str(),
            schema_type.name.as_ref(),
            err
        )
    })?;
    let selection_set = validate_selection_set(
        &query_field.selection_set,
        field_type,
        schema_types,
        fragments,
    )
    .map_err(|err| {
        format!(
            "Invalid selection set for field \"{}\" on type {}: {}",
            query_field.name.as_str(),
            schema_type.name.as_ref(),
            err
        )
    })?;
    Ok(match (arguments, selection_set) {
        (None, None) => None,
        (arguments, selection_set) => Some(query::Field {
            arguments: arguments.unwrap_or_else(|| query_field.arguments.clone()),
            selection_set: selection_set.unwrap_or_else(|| query_field.selection_set.clone()),
            ..query_field.clone()
        }),
    })
}

fn validate_object_field_arguments<'schema, TSchema: GraphQlText<'schema>>(
    query_field: &query::Field,
    schema_field: &schema::Field<'schema, TSchema>,
) -> Result<Option<Vec<(String, Value)>>, String> {
    let (missing_required_arguments, missing_optional_arguments): (Vec<_>, Vec<_>) = schema_field
        .arguments
        .iter()
        .filter(|schema_argument| {
            query_field
                .arguments
                .iter()
                .find(|(key, _)| key.as_str() == schema_argument.name.as_ref())
                .is_none()
        })
        .partition(|schema_argument| match &schema_argument.value_type {
            schema::Type::NonNullType(_) if schema_argument.default_value.is_none() => true,
            _ => false,
        });
    if let Some(schema_argument) = missing_required_arguments.first() {
        return Err(format!(
            "Missing argument \"{}\"",
            schema_argument.name.as_ref(),
        ));
    }
    let mut undeclared_arguments = query_field.arguments.iter().filter(|(key, _value)| {
        schema_field
            .arguments
            .iter()
            .find(|schema_argument| schema_argument.name.as_ref() == key.as_str())
            .is_none()
    });
    if let Some((key, _value)) = undeclared_arguments.next() {
        return Err(format!("Unexpected argument \"{}\"", key.as_str(),));
    }
    // TODO: Validate query field argument types against schema types
    Ok(if missing_optional_arguments.is_empty() {
        None
    } else {
        Some(
            query_field
                .arguments
                .iter()
                .cloned()
                .chain(
                    missing_optional_arguments
                        .into_iter()
                        .map(|schema_argument| {
                            (
                                String::from(schema_argument.name.as_ref()).into(),
                                get_default_argument_value(schema_argument),
                            )
                        }),
                )
                .collect(),
        )
    })
}

fn get_default_argument_value<'schema, TSchema: GraphQlText<'schema>>(
    schema_argument: &schema::InputValue<'schema, TSchema>,
) -> Value {
    match &schema_argument.default_value {
        None => Value::Null,
        Some(value) => instantiate_schema_value(value),
    }
}

fn get_type_identifier_name<'a, 'schema, TSchema: GraphQlText<'schema>>(
    field_type: &'a schema::Type<'schema, TSchema>,
) -> &'a TSchema {
    match field_type {
        schema::Type::NamedType(type_name) => type_name,
        schema::Type::ListType(field_type) => get_type_identifier_name(field_type.as_ref()),
        schema::Type::NonNullType(field_type) => get_type_identifier_name(field_type.as_ref()),
    }
}

/// Converts a GraphQL schema value into a GraphQL query value
fn instantiate_schema_value<'schema, TSchema: GraphQlText<'schema>>(
    value: &schema::Value<'schema, TSchema>,
) -> Value {
    match value {
        schema::Value::Variable(name) => Value::Variable(String::from(name.as_ref()).into()),
        schema::Value::Int(value) => Value::Int(value.into()),
        schema::Value::Float(value) => Value::Float(*value),
        schema::Value::String(value) => Value::String(value.clone()),
        schema::Value::Boolean(value) => Value::Boolean(*value),
        schema::Value::Null => Value::Null,
        schema::Value::Enum(variant) => Value::Enum(String::from(variant.as_ref()).into()),
        schema::Value::List(items) => {
            Value::List(items.iter().map(instantiate_schema_value).collect())
        }
        schema::Value::Object(fields) => Value::Object(
            fields
                .iter()
                .map(|(key, value)| {
                    (
                        String::from(key.as_ref()).into(),
                        instantiate_schema_value(value),
                    )
                })
                .collect(),
        ),
    }
}

/// Applies the given `transform` to each of the `items`, returning an ordered list of results if any of the transformations return a value.
/// If no items return a transformed value, `None` is returned.
///
/// For any items that do not return a transformed value (but other items in the list do return a transformed value), the `fallback` will be invoked to determine that item's output value.
fn collect_optionally_transformed_items<T, V>(
    items: &[T],
    transform: impl Fn(&T) -> Result<Option<V>, String>,
    fallback: impl Fn(&T) -> V,
) -> Result<Option<Vec<V>>, String> {
    // We want to avoid allocating a vector if none of the items need transforming - so we iterate
    // until we find the first item that needs to be transformed (if any), at which point we allocate
    // a vector with the correct capacity and back-fill any preceding entries with clones of the
    // original items. From that point forwards, each subsequent result will be pushed to the vector
    // (whether a transformed item or a clone of the original).
    items
        .iter()
        .enumerate()
        .fold(Ok(None), |results, (index, item)| {
            let results = results?;
            let transformed_items = transform(item)?;
            Ok(match transformed_items {
                None => results.map(|mut results| {
                    results.push(fallback(item));
                    results
                }),
                Some(transformed_item) => {
                    let mut results = results.unwrap_or_else(|| {
                        let mut results = Vec::with_capacity(items.len());
                        results.extend((0..index).map(|index| fallback(&items[index])));
                        results
                    });
                    results.push(transformed_item);
                    Some(results)
                }
            })
        })
}

/// Replaces the first list item matching a given `predicate` with the given `replacement`.
///
/// If no items match the predicate, `None` is returned.
fn find_replace_list_item<T: Clone>(
    items: &[T],
    predicate: impl Fn(&T) -> bool,
    replacement: T,
) -> Option<Vec<T>> {
    let insertion_index = items
        .iter()
        .enumerate()
        .find(|(_, item)| predicate(*item))
        .map(|(index, _)| index)?;
    Some(
        (items[0..insertion_index])
            .iter()
            .cloned()
            .chain(once(replacement))
            .chain((items[insertion_index + 1..]).iter().cloned())
            .collect(),
    )
}

#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    use graphql_parser::{parse_query, parse_schema};
    use reflex_json::json;

    use crate::{GraphQlQuery, GraphQlQueryTransform};

    use super::{
        parse_graphql_schema_types, validate_graphql_result, ValidateQueryGraphQlTransform,
    };

    fn assert_graphql_transform(
        transform: &impl GraphQlQueryTransform,
        input: &str,
        expected: &str,
    ) {
        let input = parse_query::<String>(input).unwrap().into_static();
        let expected = parse_query::<String>(expected).unwrap().into_static();
        let result = transform.transform(GraphQlQuery::from(&input), Default::default());
        assert_eq!(
            result.map(|(result, _)| format!("{}", result)),
            Ok(format!("{}", expected))
        );
    }

    #[test]
    fn validate_query_optional_args() {
        let schema = "
        type Query {
            standalone: Int!
            args(foo: Int!, bar: Int!): Int!
            optionalArgs(foo: Int, bar: Int): Int!
            defaultArgs(foo: Int = 3, bar: Int = 4): Int!
            nested(foo: Int): Entity!
            deeply(foo: Int): DeeplyNested!
        }

        type DeeplyNested {
            nested(bar: Int): Entity!
        }

        type Entity {
            value(required: Int!, optional: Int, default: Int = 3): Int!
        }
        ";
        let schema = parse_schema::<Cow<str>>(schema).unwrap();
        let schema_types = parse_graphql_schema_types(schema).unwrap();
        let transform = ValidateQueryGraphQlTransform::new(schema_types);

        let input = "query {
            standalone
        }";
        let expected = "query {
            standalone
        }";
        assert_graphql_transform(&transform, input, expected);

        let input = "query {
            args(foo: 3, bar: 4)
        }";
        let expected = "query {
            args(foo: 3, bar: 4)
        }";
        assert_graphql_transform(&transform, input, expected);

        let input = "query {
            unspecified: optionalArgs
            partial: optionalArgs(foo: 3)
            full: optionalArgs(foo: 3, bar: 4)
        }";
        let expected = "query {
            unspecified: optionalArgs(foo: null, bar: null)
            partial: optionalArgs(foo: 3, bar: null)
            full: optionalArgs(foo: 3, bar: 4)
        }";
        assert_graphql_transform(&transform, input, expected);

        let input = "query {
            unspecified: defaultArgs
            partial: defaultArgs(foo: 5)
            full: defaultArgs(foo: 5, bar: 6)
        }";
        let expected = "query {
            unspecified: defaultArgs(foo: 3, bar: 4)
            partial: defaultArgs(foo: 5, bar: 4)
            full: defaultArgs(foo: 5, bar: 6)
        }";
        assert_graphql_transform(&transform, input, expected);

        let input = "query {
            nested {
                value(required: 5)
            }
        }";
        let expected = "query {
            nested(foo: null) {
                value(required: 5, optional: null, default: 3)
            }
        }";
        assert_graphql_transform(&transform, input, expected);

        let input = "query {
            deeply {
                nested {
                    value(required: 5)
                }
            }
        }";
        let expected = "query {
            deeply(foo: null) {
                nested(bar: null) {
                    value(required: 5, optional: null, default: 3)
                }
            }
        }";
        assert_graphql_transform(&transform, input, expected);
    }

    #[test]
    fn validate_query_custom_resolver_roots() {
        let schema = "
        schema {
            query: Foo
            subscription: Bar
        }

        type Foo {
            foo(value: Int): Int!
        }

        type Bar {
            bar(value: Int): Int!
        }

        type Mutation {
            baz(value: Int): Int!
        }
        ";
        let schema = parse_schema::<Cow<str>>(schema).unwrap();
        let schema_types = parse_graphql_schema_types(schema).unwrap();
        let transform = ValidateQueryGraphQlTransform::new(schema_types);

        let input = "query {
            foo
        }";
        let expected = "query {
            foo(value: null)
        }";
        assert_graphql_transform(&transform, input, expected);

        let input = "subscription {
            bar
        }";
        let expected = "subscription {
            bar(value: null)
        }";
        assert_graphql_transform(&transform, input, expected);

        let input = "mutation {
            baz
        }";
        let expected = "mutation {
            baz(value: null)
        }";
        assert_graphql_transform(&transform, input, expected);
    }

    #[test]
    fn validate_result() {
        let schema = "
        type Query {
            boolean: Boolean!
            int: Int!
            string: String!
            enum: Foo!
            object: Query!
            list_of_ints: [Int!]!
            list_of_strings: [String!]!
            list_of_objects: [Query!]!
        }

        enum Foo {
            FOO
            BAR
            BAZ
        }
        ";
        let schema = parse_schema::<Cow<str>>(schema).unwrap();
        let schema_types = parse_graphql_schema_types(schema).unwrap();

        let query = GraphQlQuery::from(
            &parse_query(
                "query {
                boolean
                int
                string
                enum
                object {
                    int
                }
                list_of_ints
                list_of_strings
                list_of_objects {
                    int
                }
            }",
            )
            .unwrap()
            .into_static(),
        );
        let payload = json!({
            "int": null,
            "string": 3,
            "enum": "QUX",
            "object": {
                "int": "foo",
            },
            "list_of_ints": ["foo", 3, "bar"],
            "list_of_strings": "foo",
            "list_of_objects": [
                { "int": "foo" },
                { "int": 3 },
                { "int": "bar" },
            ],
        });
        let errors = validate_graphql_result(&payload, &query, &schema_types);
        assert_eq!(
            errors,
            Err(vec![
                json!({ "path": ["boolean"], "message": "GraphQL validation error: Missing field, expected Boolean!" }),
                json!({ "path": ["int"], "message": "GraphQL validation error: Expected Int!, received null" }),
                json!({ "path": ["string"], "message": "GraphQL validation error: Expected String!, received 3" }),
                json!({ "path": ["enum"], "message": "GraphQL validation error: Expected Foo!, received \"QUX\"" }),
                json!({ "path": ["object", "int"], "message": "GraphQL validation error: Expected Int!, received \"foo\"" }),
                json!({ "path": ["list_of_ints", 0], "message": "GraphQL validation error: Expected Int!, received \"foo\"" }),
                json!({ "path": ["list_of_ints", 2], "message": "GraphQL validation error: Expected Int!, received \"bar\"" }),
                json!({ "path": ["list_of_strings"], "message": "GraphQL validation error: Expected [String!]!, received \"foo\"" }),
                json!({ "path": ["list_of_objects", 0, "int"], "message": "GraphQL validation error: Expected Int!, received \"foo\"" }),
                json!({ "path": ["list_of_objects", 2, "int"], "message": "GraphQL validation error: Expected Int!, received \"bar\"" }),
            ])
        );

        let query = GraphQlQuery::from(
            &parse_query(
                "query {
                boolean
                int
                string
                enum
                object {
                    boolean
                    int
                    string
                    object {
                        boolean
                        int
                        string
                    }
                }
                list_of_ints
                list_of_strings
                list_of_objects {
                    boolean
                    int
                    string
                    object {
                        boolean
                        int
                        string
                    }
                }
            }",
            )
            .unwrap()
            .into_static(),
        );
        let payload = json!({
            "boolean": true,
            "int": 3,
            "string": "foo",
            "enum": "FOO",
            "object": {
                "boolean": true,
                "int": 3,
                "string": "foo",
                "object": {
                    "boolean": true,
                    "int": 3,
                    "string": "foo",
                }
            },
            "list_of_ints": [3, 4, 5],
            "list_of_strings": ["foo", "bar", "baz"],
            "list_of_objects": [
                {
                    "boolean": true,
                    "int": 3,
                    "string": "foo",
                    "object": {
                        "boolean": true,
                        "int": 3,
                        "string": "foo",
                    }
                },
                {
                    "boolean": false,
                    "int": 4,
                    "string": "bar",
                    "object": {
                        "boolean": true,
                        "int": 4,
                        "string": "bar",
                    }
                },
                {
                    "boolean": false,
                    "int": 5,
                    "string": "baz",
                    "object": {
                        "boolean": true,
                        "int": 5,
                        "string": "baz",
                    }
                },
            ]
        });
        let errors = validate_graphql_result(&payload, &query, &schema_types);
        assert_eq!(errors, Ok(()));
    }

    #[test]
    fn validate_result_aliased() {
        let schema = "
        type Query {
            foo: String!
        }
        ";
        let schema = parse_schema::<Cow<str>>(schema).unwrap();
        let schema_types = parse_graphql_schema_types(schema).unwrap();

        let query = GraphQlQuery::from(
            &parse_query(
                "query {
                bar: foo
            }",
            )
            .unwrap()
            .into_static(),
        );

        let payload = json!({
            "bar": "foo",
        });
        let errors = validate_graphql_result(&payload, &query, &schema_types);
        assert_eq!(errors, Ok(()));

        let payload = json!({
            "foo": "foo",
        });
        let errors = validate_graphql_result(&payload, &query, &schema_types);
        assert_eq!(
            errors,
            Err(vec![
                json!({ "path": ["bar"], "message": "GraphQL validation error: Missing field, expected String!" }),
            ])
        );
    }

    #[test]
    fn validate_result_optional() {
        let schema = "
        type Query {
            string: String
        }
        ";
        let schema = parse_schema::<Cow<str>>(schema).unwrap();
        let schema_types = parse_graphql_schema_types(schema).unwrap();

        let query = GraphQlQuery::from(
            &parse_query(
                "query {
                string
            }",
            )
            .unwrap()
            .into_static(),
        );

        let payload = json!({
            "string": reflex_json::JsonValue::Null,
        });
        let errors = validate_graphql_result(&payload, &query, &schema_types);
        assert_eq!(errors, Ok(()));

        let payload = json!({});
        let errors = validate_graphql_result(&payload, &query, &schema_types);
        assert_eq!(
            errors,
            Err(vec![
                json!({ "path": ["string"], "message": "GraphQL validation error: Missing field, expected String" }),
            ])
        );
    }

    #[test]
    fn validate_result_fragments() {
        let schema = "
        type Query {
            foo: String!
            bar: String!
            baz: String!
            qux: String!
        }
        ";
        let schema = parse_schema::<Cow<str>>(schema).unwrap();
        let schema_types = parse_graphql_schema_types(schema).unwrap();

        let query = GraphQlQuery::from(
            &parse_query(
                "query {
                ...Foo
                ...Bar
                baz
            }
            fragment Foo on Query {
                foo
            }
            fragment Bar on Query {
                bar
            }
            ",
            )
            .unwrap()
            .into_static(),
        );

        let payload = json!({
            "foo": "foo",
            "bar": "bar",
            "baz": "baz",
        });
        let errors = validate_graphql_result(&payload, &query, &schema_types);
        assert_eq!(errors, Ok(()));

        let payload = json!({});
        let errors = validate_graphql_result(&payload, &query, &schema_types);
        assert_eq!(
            errors,
            Err(vec![
                json!({ "path": ["foo"], "message": "GraphQL validation error: Missing field, expected String!" }),
                json!({ "path": ["bar"], "message": "GraphQL validation error: Missing field, expected String!" }),
                json!({ "path": ["baz"], "message": "GraphQL validation error: Missing field, expected String!" }),
            ])
        );
    }
}
