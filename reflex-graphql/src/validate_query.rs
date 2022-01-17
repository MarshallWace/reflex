// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{hash_map::Entry, HashMap},
    iter::once,
};

use graphql_parser::{query, schema};

use crate::{get_root_operation, GraphQlQueryTransform, GraphQlText};

#[derive(Clone, Debug)]
struct SchemaTypes<'src, T: GraphQlText<'src>> {
    types: HashMap<String, schema::TypeDefinition<'src, T>>,
    query: Option<String>,
    mutation: Option<String>,
    subscription: Option<String>,
}
impl<'src, T: GraphQlText<'src>> SchemaTypes<'src, T> {
    fn get_type(&self, name: &str) -> Option<&schema::TypeDefinition<'src, T>> {
        self.types.get(name)
    }
}
type QueryFragments<'src, 'a, T> = HashMap<&'a T, &'a query::FragmentDefinition<'src, T>>;

/// GraphQL transform that validates the incoming GraphQL query against the provided GraphQL schema.
///
/// Any optional schema field arguments not supplied by the query will be substituted with null values.
///
/// # Examples
///
/// ```
/// use reflex_graphql::validate_query::ValidateQueryGraphQlTransform;
///
/// let schema = graphql_parser::parse_schema::<'static, String>("
///     type Query {
///       greet(user: String): String!
///     }
/// ").unwrap();
///
/// let transform = ValidateQueryGraphQlTransform::new(schema);
/// ```
#[derive(Clone)]
pub struct ValidateQueryGraphQlTransform<'schema, TSchema: GraphQlText<'schema>> {
    schema_types: SchemaTypes<'schema, TSchema>,
}
impl<'schema, TSchema: GraphQlText<'schema>> ValidateQueryGraphQlTransform<'schema, TSchema> {
    pub fn new(schema: schema::Document<'schema, TSchema>) -> Result<Self, String> {
        Ok(Self {
            schema_types: parse_schema_types(schema)?,
        })
    }
}
impl<'schema, TSchema: GraphQlText<'schema>> GraphQlQueryTransform
    for ValidateQueryGraphQlTransform<'schema, TSchema>
{
    fn transform<'query, TQuery: GraphQlText<'query>>(
        &self,
        document: query::Document<'query, TQuery>,
    ) -> Result<query::Document<'query, TQuery>, String> {
        validate_document(&document, &self.schema_types).map(|transformed| match transformed {
            Some(document) => document,
            None => document,
        })
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

fn parse_schema_types<'schema, TSchema: GraphQlText<'schema>>(
    schema: schema::Document<'schema, TSchema>,
) -> Result<SchemaTypes<'schema, TSchema>, String> {
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
    Ok(SchemaTypes {
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

fn validate_document<
    'schema,
    'query,
    TSchema: GraphQlText<'schema>,
    TQuery: GraphQlText<'query>,
>(
    document: &query::Document<'query, TQuery>,
    schema_types: &SchemaTypes<'schema, TSchema>,
) -> Result<Option<query::Document<'query, TQuery>>, String> {
    let fragments = document
        .definitions
        .iter()
        .filter_map(|definition| match definition {
            query::Definition::Fragment(fragment) => Some((&fragment.name, fragment)),
            _ => None,
        })
        .collect::<QueryFragments<'query, '_, TQuery>>();
    get_root_operation(document).and_then(|operation| {
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

fn get_operation_root_type<
    'a,
    'schema,
    'query,
    TSchema: GraphQlText<'schema>,
    TQuery: GraphQlText<'query>,
>(
    operation: &query::OperationDefinition<'query, TQuery>,
    schema_types: &'a SchemaTypes<'schema, TSchema>,
) -> Option<&'a schema::TypeDefinition<'schema, TSchema>> {
    let root_type_name = match operation {
        query::OperationDefinition::Query(_) | query::OperationDefinition::SelectionSet(_) => {
            schema_types.query.as_ref()
        }
        query::OperationDefinition::Mutation(_) => schema_types.mutation.as_ref(),
        query::OperationDefinition::Subscription(_) => schema_types.subscription.as_ref(),
    }?;
    schema_types.get_type(root_type_name)
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

fn validate_operation<
    'schema,
    'query,
    TSchema: GraphQlText<'schema>,
    TQuery: GraphQlText<'query>,
>(
    operation: &query::OperationDefinition<'query, TQuery>,
    schema_types: &SchemaTypes<'schema, TSchema>,
    fragments: &QueryFragments<'query, '_, TQuery>,
) -> Result<Option<query::OperationDefinition<'query, TQuery>>, String> {
    let operation_type = get_operation_root_type(operation, schema_types).ok_or_else(|| {
        format!(
            "Unsupported root operation type: {}",
            match operation {
                query::OperationDefinition::Query(_)
                | query::OperationDefinition::SelectionSet(_) => {
                    "query"
                }
                query::OperationDefinition::Mutation(_) => "mutation",
                query::OperationDefinition::Subscription(_) => "subscription",
            }
        )
    })?;
    match operation {
        query::OperationDefinition::Query(operation) => validate_selection_set(
            &operation.selection_set,
            operation_type,
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
            operation_type,
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
            operation_type,
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
            validate_selection_set(root, operation_type, schema_types, fragments)
                .map(|result| result.map(query::OperationDefinition::SelectionSet))
        }
    }
}

fn validate_selection_set<
    'schema,
    'query,
    TSchema: GraphQlText<'schema>,
    TQuery: GraphQlText<'query>,
>(
    selection_set: &query::SelectionSet<'query, TQuery>,
    schema_type: &schema::TypeDefinition<'schema, TSchema>,
    schema_types: &SchemaTypes<'schema, TSchema>,
    fragments: &QueryFragments<'query, '_, TQuery>,
) -> Result<Option<query::SelectionSet<'query, TQuery>>, String> {
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

fn validate_scalar_selection_set<
    'schema,
    'query,
    TSchema: GraphQlText<'schema>,
    TQuery: GraphQlText<'query>,
>(
    selection_set: &query::SelectionSet<'query, TQuery>,
    schema_type: &schema::ScalarType<'schema, TSchema>,
) -> Result<Option<query::SelectionSet<'query, TQuery>>, String> {
    if selection_set.items.is_empty() {
        Ok(None)
    } else {
        Err(format!(
            "Scalar type {} does not support child selections",
            schema_type.name.as_ref()
        ))
    }
}

fn validate_enum_selection_set<
    'schema,
    'query,
    TSchema: GraphQlText<'schema>,
    TQuery: GraphQlText<'query>,
>(
    selection_set: &query::SelectionSet<'query, TQuery>,
    schema_type: &schema::EnumType<'schema, TSchema>,
) -> Result<Option<query::SelectionSet<'query, TQuery>>, String> {
    if selection_set.items.is_empty() {
        Ok(None)
    } else {
        Err(format!(
            "Enum type {} does not support child selections",
            schema_type.name.as_ref()
        ))
    }
}

fn validate_object_selection_set<
    'schema,
    'query,
    TSchema: GraphQlText<'schema>,
    TQuery: GraphQlText<'query>,
>(
    selection_set: &query::SelectionSet<'query, TQuery>,
    schema_type: &schema::ObjectType<'schema, TSchema>,
    schema_types: &SchemaTypes<'schema, TSchema>,
    fragments: &QueryFragments<'query, '_, TQuery>,
) -> Result<Option<query::SelectionSet<'query, TQuery>>, String> {
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

fn validate_object_selection<
    'schema,
    'query,
    TSchema: GraphQlText<'schema>,
    TQuery: GraphQlText<'query>,
>(
    selection: &query::Selection<'query, TQuery>,
    schema_type: &schema::ObjectType<'schema, TSchema>,
    schema_types: &SchemaTypes<'schema, TSchema>,
    fragments: &QueryFragments<'query, '_, TQuery>,
) -> Result<Option<Vec<query::Selection<'query, TQuery>>>, String> {
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

fn validate_object_fragment_spread_selection<
    'schema,
    'query,
    TSchema: GraphQlText<'schema>,
    TQuery: GraphQlText<'query>,
>(
    fragment: &query::FragmentSpread<'query, TQuery>,
    schema_type: &schema::ObjectType<'schema, TSchema>,
    schema_types: &SchemaTypes<'schema, TSchema>,
    fragments: &QueryFragments<'query, '_, TQuery>,
) -> Result<Option<Vec<query::Selection<'query, TQuery>>>, String> {
    fragments
        .get(&fragment.fragment_name)
        .ok_or_else(|| format!("Undefined fragment: {}", fragment.fragment_name.as_ref()))
        .and_then(|fragment| {
            validate_object_selection_set(
                &fragment.selection_set,
                schema_type,
                schema_types,
                fragments,
            )
            .map(|result| result.map(|selection_set| selection_set.items))
        })
}

fn validate_object_field_selection<
    'schema,
    'query,
    TSchema: GraphQlText<'schema>,
    TQuery: GraphQlText<'query>,
>(
    query_field: &query::Field<'query, TQuery>,
    schema_type: &schema::ObjectType<'schema, TSchema>,
    schema_types: &SchemaTypes<'schema, TSchema>,
    fragments: &QueryFragments<'query, '_, TQuery>,
) -> Result<Option<query::Field<'query, TQuery>>, String> {
    let schema_field = schema_type
        .fields
        .iter()
        .find(|schema_field| schema_field.name.as_ref() == query_field.name.as_ref())
        .ok_or_else(|| {
            format!(
                "Field \"{}\" not found on type {}",
                query_field.name.as_ref(),
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
            query_field.name.as_ref(),
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
            query_field.name.as_ref(),
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

fn validate_object_field_arguments<
    'schema,
    'query,
    TSchema: GraphQlText<'schema>,
    TQuery: GraphQlText<'query>,
>(
    query_field: &query::Field<'query, TQuery>,
    schema_field: &schema::Field<'schema, TSchema>,
) -> Result<Option<Vec<(TQuery, query::Value<'query, TQuery>)>>, String> {
    let (missing_required_arguments, missing_optional_arguments): (Vec<_>, Vec<_>) = schema_field
        .arguments
        .iter()
        .filter(|schema_argument| {
            query_field
                .arguments
                .iter()
                .find(|(key, _)| key.as_ref() == schema_argument.name.as_ref())
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
            .find(|schema_argument| schema_argument.name.as_ref() == key.as_ref())
            .is_none()
    });
    if let Some((key, _value)) = undeclared_arguments.next() {
        return Err(format!("Unexpected argument \"{}\"", key.as_ref(),));
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

fn get_default_argument_value<
    'schema,
    'query,
    TSchema: GraphQlText<'schema>,
    TQuery: GraphQlText<'query>,
>(
    schema_argument: &schema::InputValue<'schema, TSchema>,
) -> query::Value<'query, TQuery> {
    match &schema_argument.default_value {
        None => query::Value::Null,
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
fn instantiate_schema_value<
    'schema,
    'query,
    TSchema: GraphQlText<'schema>,
    TQuery: GraphQlText<'query>,
>(
    value: &schema::Value<'schema, TSchema>,
) -> query::Value<'query, TQuery> {
    match value {
        schema::Value::Variable(name) => query::Value::Variable(String::from(name.as_ref()).into()),
        schema::Value::Int(value) => query::Value::Int(value.clone()),
        schema::Value::Float(value) => query::Value::Float(*value),
        schema::Value::String(value) => query::Value::String(value.clone()),
        schema::Value::Boolean(value) => query::Value::Boolean(*value),
        schema::Value::Null => query::Value::Null,
        schema::Value::Enum(variant) => query::Value::Enum(String::from(variant.as_ref()).into()),
        schema::Value::List(items) => {
            query::Value::List(items.iter().map(instantiate_schema_value).collect())
        }
        schema::Value::Object(fields) => query::Value::Object(
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

    use crate::GraphQlQueryTransform;

    use super::ValidateQueryGraphQlTransform;

    fn assert_graphql_transform(
        transform: &impl GraphQlQueryTransform,
        input: &str,
        expected: &str,
    ) {
        let input = parse_query::<Cow<str>>(input).unwrap();
        let expected = parse_query::<Cow<str>>(expected).unwrap();
        let result = transform.transform(input);
        assert_eq!(
            result.map(|result| format!("{}", result)),
            Ok(format!("{}", expected))
        );
    }

    #[test]
    fn inject_optional_args() {
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
        let transform = ValidateQueryGraphQlTransform::new(schema).unwrap();

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
    fn custom_resolver_roots() {
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
        let transform = ValidateQueryGraphQlTransform::new(schema).unwrap();

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
}
