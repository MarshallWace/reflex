// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{collections::HashMap, iter::once};

use graphql_parser::query::{
    Definition, Document, Field, FragmentDefinition, FragmentSpread, Mutation, OperationDefinition,
    Query, Selection, SelectionSet, Subscription, Value,
};
use reflex_json::JsonValue;

use crate::{get_root_operation, GraphQlQueryTransform, GraphQlText};

#[derive(Clone, PartialEq, Debug)]
pub struct InjectedQueryArguments {
    query: Option<NestedArgs>,
    mutation: Option<NestedArgs>,
    subscription: Option<NestedArgs>,
}
#[derive(Clone, PartialEq, Debug)]
pub struct NestedArgs {
    value: Option<Vec<(String, Value<'static, String>)>>,
    children: Vec<(String, NestedArgs)>,
}

type QueryFragments<'src, 'a, T> = HashMap<&'a T, &'a FragmentDefinition<'src, T>>;

/// Generates a nested tree of injected query arguments from the provided GraphQL template.
///
/// The argument template can contain `query`, `subscription`, `mutation` and `fragment` nodes to specify which arguments to inject into the given operation type.
/// The output value will contain all field arguments encountered while traversing the template.
///
/// Any GraphQL variables within the argument template refer to the values passed in as the `variables` argument.
///
/// This helper is intended for use with the [InjectQueryArgumentsGraphQlTransform] transform.
///
/// # Examples
///
/// ```
/// use reflex_graphql::inject_args::{parse_argument_template, InjectQueryArgumentsGraphQlTransform};
///
/// let template = graphql_parser::parse_query("
///     query {
///         ...QueryArguments
///     }
///     mutation {
///         foo(bar: 3)
///     }
///     fragment QueryArguments on Query {
///         profile(user: $username) {
///             lastLogin(ip: $ipAddress)
///         }
///     }
/// ").unwrap();
///
/// let injected_args = parse_argument_template(&template, [
///     (String::from("username"), reflex_json::JsonValue::String(String::from("jdoe"))),
///     (String::from("ipAddress"), reflex_json::JsonValue::String(String::from("123.0.0.1"))),
/// ]).unwrap();
///
/// let transform = InjectQueryArgumentsGraphQlTransform::new(injected_args);
/// ```
pub fn parse_argument_template(
    template: &Document<String>,
    variables: impl IntoIterator<Item = (String, JsonValue)>,
) -> Result<InjectedQueryArguments, String> {
    let variables = variables.into_iter().collect::<HashMap<_, _>>();
    let (query, mutation, subscription, fragments) = template.definitions.iter().fold(
        Ok((Vec::new(), Vec::new(), Vec::new(), HashMap::new())),
        |results, definition| {
            let (mut queries, mut mutations, mut subscriptions, mut fragments) = results?;
            match definition {
                Definition::Operation(operation) => match operation {
                    OperationDefinition::Query(query) => queries.push(query),
                    OperationDefinition::Mutation(mutation) => mutations.push(mutation),
                    OperationDefinition::Subscription(subscription) => {
                        subscriptions.push(subscription)
                    }
                    OperationDefinition::SelectionSet(_) => {
                        return Err(format!("Unexpected root selection node"))
                    }
                },
                Definition::Fragment(fragment) => {
                    fragments.insert(String::from(&fragment.name), fragment);
                }
            }
            Ok((queries, mutations, subscriptions, fragments))
        },
    )?;
    if query.len() > 1 {
        return Err(String::from("Multiple query root nodes"));
    } else if mutation.len() > 1 {
        return Err(String::from("Multiple mutation root nodes"));
    } else if subscription.len() > 1 {
        return Err(String::from("Multiple subscription root nodes"));
    }
    Ok(InjectedQueryArguments {
        query: match query.into_iter().next() {
            None => None,
            Some(query) => parse_operation_root_argument_template(
                &query.selection_set,
                &variables,
                &fragments,
            )?,
        },
        mutation: match mutation.into_iter().next() {
            None => None,
            Some(mutation) => parse_operation_root_argument_template(
                &mutation.selection_set,
                &variables,
                &fragments,
            )?,
        },
        subscription: match subscription.into_iter().next() {
            None => None,
            Some(subscription) => parse_operation_root_argument_template(
                &subscription.selection_set,
                &variables,
                &fragments,
            )?,
        },
    })
}

fn parse_operation_root_argument_template<'src>(
    selection_set: &SelectionSet<'src, String>,
    variables: &HashMap<String, JsonValue>,
    fragments: &HashMap<String, &FragmentDefinition<'src, String>>,
) -> Result<Option<NestedArgs>, String> {
    parse_selection_set_argument_template(selection_set, variables, fragments).map(|result| {
        result.map(|children| NestedArgs {
            value: None,
            children,
        })
    })
}

fn parse_selection_set_argument_template<'src>(
    selection_set: &SelectionSet<'src, String>,
    variables: &HashMap<String, JsonValue>,
    fragments: &HashMap<String, &FragmentDefinition<'src, String>>,
) -> Result<Option<Vec<(String, NestedArgs)>>, String> {
    let fields = selection_set
        .items
        .iter()
        .map(|selection| match selection {
            Selection::Field(field) => parse_field_argument_template(field, variables, fragments)
                .map(|result| {
                    result
                        .map(|value| vec![(String::from(&field.name), value)])
                        .unwrap_or_else(|| Vec::new())
                }),
            Selection::FragmentSpread(field) => fragments
                .get(&field.fragment_name)
                .ok_or_else(|| format!("Invalid fragment name: {}", field.fragment_name))
                .and_then(|fragment| {
                    parse_selection_set_argument_template(
                        &fragment.selection_set,
                        variables,
                        fragments,
                    )
                    .map(|fields| fields.unwrap_or_else(|| Vec::new()))
                }),
            Selection::InlineFragment(_) => Err(String::from("Unexpected inline fragment")),
        })
        .collect::<Result<Vec<_>, _>>()?;
    if fields.is_empty() {
        Ok(None)
    } else {
        Ok(Some(fields.into_iter().flatten().collect()))
    }
}

fn parse_field_argument_template<'src>(
    field: &Field<'src, String>,
    variables: &HashMap<String, JsonValue>,
    fragments: &HashMap<String, &FragmentDefinition<'src, String>>,
) -> Result<Option<NestedArgs>, String> {
    let injected_arguments = field
        .arguments
        .iter()
        .map(|(key, value)| {
            match value {
                Value::Variable(name) => variables
                    .get(name)
                    .ok_or_else(|| format!("Unrecognized variable name: {}", name))
                    .and_then(|value| parse_json_value(value)),
                _ => Ok(instantiate_template_arg_value(value)),
            }
            .map(|value| (String::from(key), value))
        })
        .collect::<Result<Vec<_>, _>>()?;
    let children =
        parse_selection_set_argument_template(&field.selection_set, variables, fragments)
            .map(|fields| fields.unwrap_or_else(|| Vec::new()))?;
    Ok(if injected_arguments.is_empty() && children.is_empty() {
        None
    } else {
        Some(NestedArgs {
            value: if injected_arguments.is_empty() {
                None
            } else {
                Some(injected_arguments)
            },
            children,
        })
    })
}

/// GraphQL transform that injects the provided field arguments into a GraphQL query.
///
/// This can be used by a server to 'partially apply' field arguments into a user-provided query.
/// Only the fields specified in the query will have arguments injected.
///
/// The transform `arguments` are typically generated by the [parse_argument_template] helper.
///
/// # Examples
///
/// ```
/// use reflex_graphql::inject_args::{parse_argument_template, InjectQueryArgumentsGraphQlTransform};
///
/// let template = graphql_parser::parse_query("
///     query {
///         ...QueryArguments
///     }
///     mutation {
///         foo(bar: 3)
///     }
///     fragment QueryArguments on Query {
///         profile(user: $username) {
///             lastLogin(ip: $ipAddress)
///         }
///     }
/// ").unwrap();
///
/// let injected_args = parse_argument_template(&template, [
///     (String::from("username"), reflex_json::JsonValue::String(String::from("jdoe"))),
///     (String::from("ipAddress"), reflex_json::JsonValue::String(String::from("123.0.0.1"))),
/// ]).unwrap();
///
/// let transform = InjectQueryArgumentsGraphQlTransform::new(injected_args);
pub struct InjectQueryArgumentsGraphQlTransform {
    arguments: InjectedQueryArguments,
}
impl InjectQueryArgumentsGraphQlTransform {
    pub fn new(arguments: InjectedQueryArguments) -> Self {
        Self { arguments }
    }
}
impl GraphQlQueryTransform for InjectQueryArgumentsGraphQlTransform {
    fn transform<'src, T: GraphQlText<'src>>(
        &self,
        document: Document<'src, T>,
    ) -> Result<Document<'src, T>, String> {
        inject_document_arguments(&document, &self.arguments).map(|transformed| match transformed {
            Some(document) => document,
            None => document,
        })
    }
}

fn inject_document_arguments<'src, T: GraphQlText<'src>>(
    document: &Document<'src, T>,
    arguments: &InjectedQueryArguments,
) -> Result<Option<Document<'src, T>>, String> {
    let fragments = document
        .definitions
        .iter()
        .filter_map(|definition| match definition {
            Definition::Fragment(fragment) => Some((&fragment.name, fragment)),
            _ => None,
        })
        .collect::<QueryFragments<'src, '_, T>>();
    get_root_operation(document).and_then(|operation| {
        match inject_operation_arguments(operation, arguments, &fragments) {
            None => Ok(None),
            Some(operation) => {
                let updated_root_operation = Definition::Operation(operation);
                let updated_document = find_replace_list_item(
                    &document.definitions,
                    |definition| matches!(definition, Definition::Operation(_)),
                    updated_root_operation,
                )
                .map(|definitions| Document { definitions });
                Ok(updated_document)
            }
        }
    })
}

fn inject_operation_arguments<'src, T: GraphQlText<'src>>(
    operation: &OperationDefinition<'src, T>,
    arguments: &InjectedQueryArguments,
    fragments: &QueryFragments<'src, '_, T>,
) -> Option<OperationDefinition<'src, T>> {
    match operation {
        OperationDefinition::Query(operation) => arguments.query.as_ref().and_then(|arguments| {
            inject_selection_set_arguments(&operation.selection_set, arguments, fragments).map(
                |selection_set| {
                    OperationDefinition::Query(Query {
                        selection_set,
                        ..operation.clone()
                    })
                },
            )
        }),
        OperationDefinition::Mutation(operation) => {
            arguments.mutation.as_ref().and_then(|arguments| {
                inject_selection_set_arguments(&operation.selection_set, arguments, fragments).map(
                    |selection_set| {
                        OperationDefinition::Mutation(Mutation {
                            selection_set,
                            ..operation.clone()
                        })
                    },
                )
            })
        }
        OperationDefinition::Subscription(operation) => {
            arguments.subscription.as_ref().and_then(|arguments| {
                inject_selection_set_arguments(&operation.selection_set, arguments, fragments).map(
                    |selection_set| {
                        OperationDefinition::Subscription(Subscription {
                            selection_set,
                            ..operation.clone()
                        })
                    },
                )
            })
        }
        OperationDefinition::SelectionSet(root) => arguments.query.as_ref().and_then(|arguments| {
            inject_selection_set_arguments(root, arguments, fragments)
                .map(OperationDefinition::SelectionSet)
        }),
    }
}

fn inject_selection_set_arguments<'src, T: GraphQlText<'src>>(
    selection_set: &SelectionSet<'src, T>,
    arguments: &NestedArgs,
    fragments: &QueryFragments<'src, '_, T>,
) -> Option<SelectionSet<'src, T>> {
    collect_optionally_transformed_items(
        &selection_set.items,
        |item| inject_selection_arguments(item, arguments, fragments),
        |item| vec![item.clone()],
    )
    .map(|items| SelectionSet {
        items: items.into_iter().flatten().collect::<Vec<_>>(),
        ..selection_set.clone()
    })
}

fn inject_selection_arguments<'src, T: GraphQlText<'src>>(
    selection: &Selection<'src, T>,
    arguments: &NestedArgs,
    fragments: &QueryFragments<'src, '_, T>,
) -> Option<Vec<Selection<'src, T>>> {
    match selection {
        Selection::Field(field) => inject_field_arguments(field, arguments, fragments)
            .map(|field| vec![Selection::Field(field)]),
        Selection::FragmentSpread(fragment) => {
            inject_fragment_spread_arguments(fragment, arguments, fragments)
        }
        Selection::InlineFragment(_) => None,
    }
}

fn inject_fragment_spread_arguments<'src, T: GraphQlText<'src>>(
    fragment: &FragmentSpread<'src, T>,
    arguments: &NestedArgs,
    fragments: &QueryFragments<'src, '_, T>,
) -> Option<Vec<Selection<'src, T>>> {
    fragments.get(&fragment.fragment_name).and_then(|fragment| {
        inject_selection_set_arguments(&fragment.selection_set, arguments, fragments)
            .map(|selection_set| selection_set.items)
    })
}

fn inject_field_arguments<'src, T: GraphQlText<'src>>(
    field: &Field<'src, T>,
    arguments: &NestedArgs,
    fragments: &QueryFragments<'src, '_, T>,
) -> Option<Field<'src, T>> {
    find_named_argument(arguments, field.name.as_ref()).and_then(|(_, branch)| {
        let selection_set = inject_selection_set_arguments(&field.selection_set, branch, fragments);
        match &branch.value {
            Some(injected_args) => {
                let injected_args = injected_args.iter().cloned().map(|(name, value)| {
                    (
                        T::from(String::from(name)),
                        instantiate_runtime_arg_value(&value),
                    )
                });
                let arguments = field
                    .arguments
                    .iter()
                    .cloned()
                    .chain(injected_args)
                    .collect();
                Some(Field {
                    arguments,
                    selection_set: selection_set.unwrap_or_else(|| field.selection_set.clone()),
                    ..field.clone()
                })
            }
            None => selection_set.map(|selection_set| Field {
                selection_set,
                ..field.clone()
            }),
        }
    })
}

fn find_named_argument<'a>(
    arguments: &'a NestedArgs,
    name: &'a str,
) -> Option<&'a (String, NestedArgs)> {
    arguments
        .children
        .iter()
        .find(|(existing_name, _)| name == *existing_name)
}

/// Converts a lifetime-bound GraphQL value into a static GraphQL value
fn instantiate_template_arg_value<'a>(value: &Value<'a, String>) -> Value<'static, String> {
    match &value {
        Value::Variable(name) => Value::Variable(String::from(name)),
        Value::Int(value) => Value::Int(value.clone()),
        Value::Float(value) => Value::Float(*value),
        Value::String(value) => Value::String(value.clone()),
        Value::Boolean(value) => Value::Boolean(*value),
        Value::Null => Value::Null,
        Value::Enum(variant) => Value::Enum(String::from(variant)),
        Value::List(items) => {
            Value::List(items.iter().map(instantiate_template_arg_value).collect())
        }
        Value::Object(fields) => Value::Object(
            fields
                .iter()
                .map(|(key, value)| (String::from(key), instantiate_template_arg_value(value)))
                .collect(),
        ),
    }
}

/// Converts a static GraphQL value into a lifetime-bound GraphQL value
fn instantiate_runtime_arg_value<'a, T: GraphQlText<'a>>(
    value: &Value<'static, String>,
) -> Value<'a, T> {
    match &value {
        Value::Variable(name) => Value::Variable(T::from(String::from(name))),
        Value::Int(value) => Value::Int(value.clone()),
        Value::Float(value) => Value::Float(*value),
        Value::String(value) => Value::String(value.clone()),
        Value::Boolean(value) => Value::Boolean(*value),
        Value::Null => Value::Null,
        Value::Enum(variant) => Value::Enum(T::from(String::from(variant))),
        Value::List(items) => {
            Value::List(items.iter().map(instantiate_runtime_arg_value).collect())
        }
        Value::Object(fields) => Value::Object(
            fields
                .iter()
                .map(|(key, value)| {
                    (
                        T::from(String::from(key)),
                        instantiate_runtime_arg_value(value),
                    )
                })
                .collect(),
        ),
    }
}

/// Converts a JSON value into a static GraphQL value
fn parse_json_value(value: &JsonValue) -> Result<Value<'static, String>, String> {
    match value {
        JsonValue::Null => Ok(Value::Null),
        JsonValue::Bool(value) => Ok(Value::Boolean(*value)),
        JsonValue::Number(value) => match value.as_i64() {
            Some(value) => Ok(Value::Int((value as i32).into())),
            None => match value.as_f64() {
                Some(value) => Ok(Value::Float(value)),
                None => Err(format!("Invalid number: {}", value)),
            },
        },
        JsonValue::String(value) => Ok(Value::String(String::from(value))),
        JsonValue::Array(value) => Ok(Value::List(
            value
                .iter()
                .map(parse_json_value)
                .collect::<Result<Vec<_>, _>>()?,
        )),
        JsonValue::Object(value) => Ok(Value::Object(
            value
                .iter()
                .map(|(key, value)| parse_json_value(value).map(|value| (String::from(key), value)))
                .collect::<Result<_, _>>()?,
        )),
    }
}

/// Applies the given `transform` to each of the `items`, returning an ordered list of results if any of the transformations return a value.
/// If no items return a transformed value, `None` is returned.
///
/// For any items that do not return a transformed value (but other items in the list do return a transformed value), the `fallback` will be invoked to determine that item's output value.
fn collect_optionally_transformed_items<T, V>(
    items: &[T],
    transform: impl Fn(&T) -> Option<V>,
    fallback: impl Fn(&T) -> V,
) -> Option<Vec<V>> {
    // We want to avoid allocating a vector if none of the items need transforming - so we iterate
    // until we find the first item that needs to be transformed (if any), at which point we allocate
    // a vector with the correct capacity and back-fill any preceding entries with clones of the
    // original items. From that point forwards, each subsequent result will be pushed to the vector
    // (whether a transformed item or a clone of the original).
    items
        .iter()
        .enumerate()
        .fold(None, |results, (index, item)| {
            let transformed_items = transform(item);
            match transformed_items {
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
            }
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

    use graphql_parser::parse_query;
    use reflex_json::JsonValue;

    use crate::GraphQlQueryTransform;

    use super::{parse_argument_template, InjectQueryArgumentsGraphQlTransform};

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
    fn inject_query_args() {
        let template = "
        query {
            first(foo: $foo, extra: true)
            deeply {
                nested(bar: $bar) {
                    value(baz: $baz)
                }
            }
        }
        ";
        let injected_args = parse_argument_template(
            &parse_query(template).unwrap(),
            [
                (
                    String::from("foo"),
                    JsonValue::String(String::from("value:foo")),
                ),
                (
                    String::from("bar"),
                    JsonValue::String(String::from("value:bar")),
                ),
                (
                    String::from("baz"),
                    JsonValue::String(String::from("value:baz")),
                ),
            ],
        )
        .unwrap();

        let transform = InjectQueryArgumentsGraphQlTransform::new(injected_args);
        let input = "query {
            foo
        }";
        let expected = "query {
            foo
        }";
        assert_graphql_transform(&transform, input, expected);

        let input = "query {
            first(asdf: 3)
            second
            third
            deeply {
                nested {
                    value {
                        inner
                    }
                }
            }
        }";
        let expected = "query {
            first(asdf: 3, foo: \"value:foo\", extra: true)
            second
            third
            deeply {
                nested(bar: \"value:bar\") {
                    value(baz: \"value:baz\") {
                        inner
                    }
                }
            }
        }";
        assert_graphql_transform(&transform, input, expected);
    }

    #[test]
    fn injected_arg_fragments() {
        let template = "
        query {
            ...QueryArguments
        }
        subscription {
            ...QueryArguments
        }
        fragment QueryArguments on Query {
            foo(bar: true)
        }
        ";
        let injected_args = parse_argument_template(&parse_query(template).unwrap(), []).unwrap();
        let transform = InjectQueryArgumentsGraphQlTransform::new(injected_args);

        let input = "query {
            foo
        }";
        let expected = "query {
            foo(bar: true)
        }";
        assert_graphql_transform(&transform, input, expected);

        let input = "subscription {
            foo
        }";
        let expected = "subscription {
            foo(bar: true)
        }";
        assert_graphql_transform(&transform, input, expected);

        let input = "mutation {
            foo
        }";
        let expected = "mutation {
            foo
        }";
        assert_graphql_transform(&transform, input, expected);
    }
}
