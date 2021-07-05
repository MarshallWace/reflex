// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{any::TypeId, collections::HashMap, iter::once};

use either::Either;
use graphql_parser::{parse_query, query::*};
use reflex::{
    core::{
        ApplicationTerm, Arity, Expression, LambdaTerm, NativeFunction, Signal, SignalTerm,
        StackOffset, StructPrototype, StructTerm, Term, VariableTerm,
    },
    hash::{hash_object, HashId},
    serialize::{SerializedListTerm, SerializedObjectTerm, SerializedTerm},
    stdlib::{
        builtin::BuiltinTerm,
        collection::{vector::VectorTerm, CollectionTerm},
        signal::SignalType,
        value::{StringValue, ValueTerm},
    },
};

mod loader;
pub use loader::graphql_loader;
mod operation;
pub use operation::{deserialize_graphql_operation, GraphQlOperationPayload};

pub mod subscriptions;

type QueryVariables<'a> = HashMap<&'a str, Expression>;
type QueryFragments<'a> = HashMap<&'a str, &'a FragmentDefinition<'a, &'a str>>;

pub fn create_introspection_query_response() -> Expression {
    Expression::new(Term::Signal(SignalTerm::new(Signal::new(
        SignalType::Error,
        vec![Expression::new(Term::Value(ValueTerm::String(
            StringValue::from("Introspection query not yet implemented"),
        )))],
    ))))
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

const DEFAULT_OPERATION_TYPE: &str = "query";

pub fn parse<'v>(
    source: &str,
    variables: impl IntoIterator<Item = (&'v str, Expression)>,
) -> Result<Expression, String> {
    let variables = &variables.into_iter().collect::<QueryVariables>();
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
                Ok(operation) => match operation {
                    OperationDefinition::SelectionSet(selection_set) => parse_root_operation(
                        DEFAULT_OPERATION_TYPE,
                        selection_set,
                        variables,
                        &fragments,
                    ),
                    _ => parse_operation(operation, variables, &fragments),
                },
            }
        }
        Err(error) => Err(format!("{}", error)),
    }
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
) -> Result<Expression, String> {
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
) -> Result<Expression, String> {
    parse_root_operation("query", &operation.selection_set, variables, fragments)
}

fn parse_mutation_operation<'src>(
    operation: &Mutation<'src, &'src str>,
    variables: &QueryVariables,
    fragments: &QueryFragments,
) -> Result<Expression, String> {
    parse_root_operation("mutation", &operation.selection_set, variables, fragments)
}

fn parse_subscription_operation<'src>(
    operation: &Subscription<'src, &'src str>,
    variables: &QueryVariables,
    fragments: &QueryFragments,
) -> Result<Expression, String> {
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
) -> Result<Expression, String> {
    let query = parse_selection_set(selection_set, variables, fragments)?;
    Ok(create_lambda(
        Arity::from(1, 0, None),
        create_function_application(
            query,
            vec![create_function_application(
                create_builtin(BuiltinTerm::Get),
                vec![
                    create_scoped_variable(0),
                    create_string(String::from(operation_type)),
                ],
            )],
        ),
    ))
}

fn parse_selection_set<'src>(
    selection_set: &SelectionSet<'src, &'src str>,
    variables: &QueryVariables,
    fragments: &QueryFragments,
) -> Result<Expression, String> {
    if selection_set.items.is_empty() {
        parse_leaf()
    } else {
        let fields = parse_selection_set_fields(selection_set, variables, fragments)?;
        Ok(create_query_branch(create_lambda(
            Arity::from(1, 0, None),
            Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Builtin(BuiltinTerm::CollectStruct)),
                fields
                    .into_iter()
                    .flat_map(|(key, query)| {
                        once(Expression::new(Term::Value(ValueTerm::String(key)))).chain(once(
                            create_function_application(query, vec![create_scoped_variable(0)]),
                        ))
                    })
                    .collect::<Vec<_>>(),
            ))),
        )))
    }
}

fn parse_leaf() -> Result<Expression, String> {
    Ok(flatten_deep())
}

fn parse_selection_set_fields<'src>(
    selection_set: &SelectionSet<'src, &'src str>,
    variables: &QueryVariables,
    fragments: &QueryFragments,
) -> Result<Vec<(String, Expression)>, String> {
    selection_set
        .items
        .iter()
        .flat_map(|field| match field {
            Selection::Field(field) => match parse_field(field, variables, fragments) {
                Ok(result) => {
                    let key = field.alias.unwrap_or(field.name);
                    Either::Left(once(Ok((String::from(key), result))))
                }
                Err(error) => Either::Left(once(Err(error))),
            },
            Selection::FragmentSpread(fragment) => match fragments.get(fragment.fragment_name) {
                Some(fragment) => {
                    let fields = parse_fragment_fields(fragment, variables, fragments);
                    Either::Right(fields.into_iter())
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
) -> impl IntoIterator<Item = Result<(String, Expression), String>> {
    match parse_selection_set_fields(&fragment.selection_set, variables, fragments) {
        Err(error) => Either::Left(once(Err(error))),
        Ok(fields) => Either::Right(fields.into_iter().map(Ok)),
    }
}

fn parse_field<'src>(
    field: &Field<'src, &'src str>,
    variables: &QueryVariables,
    fragments: &QueryFragments,
) -> Result<Expression, String> {
    let field_args = if field.arguments.is_empty() {
        None
    } else {
        Some(parse_field_arguments(
            &field.arguments,
            variables,
            fragments,
        )?)
    };
    let body = parse_selection_set(&field.selection_set, variables, fragments)?;
    Ok(create_lambda(
        Arity::from(1, 0, None),
        create_function_application(
            body,
            vec![{
                let field = create_function_application(
                    create_builtin(BuiltinTerm::Get),
                    vec![
                        create_scoped_variable(0),
                        create_string(String::from(field.name)),
                    ],
                );
                match field_args {
                    None => field,
                    Some(field_args) => create_function_application(field, field_args),
                }
            }],
        ),
    ))
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

fn create_query_branch(shape: Expression) -> Expression {
    create_lambda(
        Arity::from(1, 0, None),
        create_function_application(
            dynamic_query_branch(),
            vec![create_scoped_variable(0), shape],
        ),
    )
}

fn flatten_deep() -> Expression {
    Expression::new(Term::Native(NativeFunction::new(
        FlattenDeep::hash(),
        FlattenDeep::arity(),
        FlattenDeep::apply,
    )))
}
struct FlattenDeep {}
impl FlattenDeep {
    fn hash() -> HashId {
        hash_object(&TypeId::of::<Self>())
    }
    fn arity() -> Arity {
        Arity::from(1, 0, None)
    }
    fn apply(args: Vec<Expression>) -> Expression {
        if args.len() != 1 {
            return Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![Expression::new(Term::Value(ValueTerm::String(
                    StringValue::from(format!("Expected 1 argument, received {}", args.len())),
                )))],
            ))));
        }
        let mut args = args.into_iter();
        let target = args.next().unwrap();
        match target.value() {
            Term::Value(ValueTerm::Null) => target,
            Term::Collection(CollectionTerm::Vector(collection)) => {
                Expression::new(Term::Application(ApplicationTerm::new(
                    Expression::new(Term::Builtin(BuiltinTerm::CollectVector)),
                    collection
                        .iterate()
                        .into_iter()
                        .map(|item| {
                            Expression::new(Term::Application(ApplicationTerm::new(
                                flatten_deep(),
                                vec![item],
                            )))
                        })
                        .collect(),
                )))
            }
            _ => target,
        }
    }
}

fn dynamic_query_branch() -> Expression {
    Expression::new(Term::Native(NativeFunction::new(
        DynamicQueryBranch::hash(),
        DynamicQueryBranch::arity(),
        DynamicQueryBranch::apply,
    )))
}
struct DynamicQueryBranch {}
impl DynamicQueryBranch {
    fn hash() -> HashId {
        hash_object(&TypeId::of::<Self>())
    }
    fn arity() -> Arity {
        Arity::from(2, 0, None)
    }
    fn apply(args: Vec<Expression>) -> Expression {
        if args.len() != 2 {
            return Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![Expression::new(Term::Value(ValueTerm::String(
                    StringValue::from(format!("Expected 2 arguments, received {}", args.len())),
                )))],
            ))));
        }
        let mut args = args.into_iter();
        let target = args.next().unwrap();
        let shape = args.next().unwrap();
        match target.value() {
            Term::Value(ValueTerm::Null) => target,
            Term::Collection(CollectionTerm::Vector(collection)) => {
                Expression::new(Term::Application(ApplicationTerm::new(
                    Expression::new(Term::Builtin(BuiltinTerm::CollectVector)),
                    collection
                        .iterate()
                        .into_iter()
                        .map(|item| {
                            Expression::new(Term::Application(ApplicationTerm::new(
                                dynamic_query_branch(),
                                vec![item, Expression::clone(&shape)],
                            )))
                        })
                        .collect(),
                )))
            }
            _ => Expression::new(Term::Application(ApplicationTerm::new(shape, vec![target]))),
        }
    }
}

fn create_function_application(target: Expression, args: Vec<Expression>) -> Expression {
    Expression::new(Term::Application(ApplicationTerm::new(target, args)))
}

fn create_lambda(arity: Arity, body: Expression) -> Expression {
    Expression::new(Term::Lambda(LambdaTerm::new(arity, body)))
}

fn create_scoped_variable(offset: StackOffset) -> Expression {
    Expression::new(Term::Variable(VariableTerm::scoped(offset)))
}

fn create_builtin(target: BuiltinTerm) -> Expression {
    Expression::new(Term::Builtin(target))
}

fn create_string(value: String) -> Expression {
    Expression::new(Term::Value(ValueTerm::String(value)))
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

#[cfg(test)]
mod tests {
    use reflex::{
        cache::GenerationalGc,
        core::{
            ApplicationTerm, Arity, DependencyList, DynamicState, EvaluationResult, Expression,
            LambdaTerm, StackOffset, StructPrototype, StructTerm, Term, VariableTerm,
        },
        stdlib::{
            builtin::BuiltinTerm,
            collection::{vector::VectorTerm, CollectionTerm},
            value::ValueTerm,
        },
    };

    use crate::create_function_application;

    use super::parse;

    #[test]
    fn leaf_queries() {
        let root = create_struct(vec![
            (
                "query",
                create_struct(vec![
                    ("first", create_integer(3)),
                    ("second", create_integer(4)),
                    ("third", create_integer(5)),
                ]),
            ),
            ("mutation", create_null()),
            ("subscription", create_null()),
        ]);
        let variables = Vec::new();
        let query = parse(
            "
                query {
                    second
                    third
                }
            ",
            variables,
        )
        .unwrap();
        let result = apply_query(query, root);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_struct(vec![
                    ("second", create_integer(4)),
                    ("third", create_integer(5)),
                ]),
                DependencyList::empty()
            )
        );
    }

    #[test]
    fn computed_leaf_queries() {
        let root = create_struct(vec![
            (
                "query",
                create_function_application(
                    create_lambda(
                        Arity::from(0, 1, None),
                        create_struct(vec![
                            (
                                "first",
                                create_function_application(
                                    create_builtin(BuiltinTerm::Add),
                                    vec![create_integer(3), create_scoped_variable(0)],
                                ),
                            ),
                            (
                                "second",
                                create_function_application(
                                    create_builtin(BuiltinTerm::Add),
                                    vec![create_integer(4), create_scoped_variable(0)],
                                ),
                            ),
                            (
                                "third",
                                create_function_application(
                                    create_builtin(BuiltinTerm::Add),
                                    vec![create_integer(5), create_scoped_variable(0)],
                                ),
                            ),
                        ]),
                    ),
                    vec![create_integer(10)],
                ),
            ),
            ("mutation", create_null()),
            ("subscription", create_null()),
        ]);
        let variables = Vec::new();
        let query = parse(
            "
                query {
                    second
                    third
                }
            ",
            variables,
        )
        .unwrap();
        let result = apply_query(query, root);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_struct(vec![
                    ("second", create_integer(4 + 10)),
                    ("third", create_integer(5 + 10)),
                ]),
                DependencyList::empty()
            )
        );
    }

    #[test]
    fn list_leaf_queries() {
        let root = create_struct(vec![
            (
                "query",
                create_struct(vec![
                    ("foo", create_null()),
                    (
                        "items",
                        create_vector(vec![
                            create_integer(3),
                            create_integer(4),
                            create_integer(5),
                        ]),
                    ),
                    ("bar", create_null()),
                ]),
            ),
            ("mutation", create_null()),
            ("subscription", create_null()),
        ]);
        let variables = Vec::new();
        let query = parse(
            "
                query {
                    items
                }
            ",
            variables,
        )
        .unwrap();
        let result = apply_query(query, root);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_struct(vec![(
                    "items",
                    create_vector(vec![
                        create_integer(3),
                        create_integer(4),
                        create_integer(5)
                    ])
                ),]),
                DependencyList::empty()
            )
        );
    }

    #[test]
    fn deeply_nested_list_leaf_queries() {
        let root = create_struct(vec![
            (
                "query",
                create_struct(vec![
                    ("foo", create_null()),
                    (
                        "items",
                        create_vector(vec![
                            create_vector(vec![
                                create_vector(vec![
                                    create_float(1.1),
                                    create_float(1.2),
                                    create_float(1.3),
                                ]),
                                create_vector(vec![
                                    create_float(1.4),
                                    create_float(1.5),
                                    create_float(1.6),
                                ]),
                                create_vector(vec![
                                    create_float(1.7),
                                    create_float(1.8),
                                    create_float(1.9),
                                ]),
                            ]),
                            create_vector(vec![
                                create_vector(vec![
                                    create_float(2.1),
                                    create_float(2.2),
                                    create_float(2.3),
                                ]),
                                create_vector(vec![
                                    create_float(2.4),
                                    create_float(2.5),
                                    create_float(2.6),
                                ]),
                                create_vector(vec![
                                    create_float(2.7),
                                    create_float(2.8),
                                    create_float(2.9),
                                ]),
                            ]),
                            create_vector(vec![
                                create_vector(vec![
                                    create_float(3.1),
                                    create_float(3.2),
                                    create_float(3.3),
                                ]),
                                create_vector(vec![
                                    create_float(3.4),
                                    create_float(3.5),
                                    create_float(3.6),
                                ]),
                                create_vector(vec![
                                    create_float(3.7),
                                    create_float(3.8),
                                    create_float(3.9),
                                ]),
                            ]),
                        ]),
                    ),
                    ("bar", create_null()),
                ]),
            ),
            ("mutation", create_null()),
            ("subscription", create_null()),
        ]);
        let variables = Vec::new();
        let query = parse(
            "
                query {
                    items
                }
            ",
            variables,
        )
        .unwrap();
        let result = apply_query(query, root);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_struct(vec![(
                    "items",
                    create_vector(vec![
                        create_vector(vec![
                            create_vector(vec![
                                create_float(1.1),
                                create_float(1.2),
                                create_float(1.3),
                            ]),
                            create_vector(vec![
                                create_float(1.4),
                                create_float(1.5),
                                create_float(1.6),
                            ]),
                            create_vector(vec![
                                create_float(1.7),
                                create_float(1.8),
                                create_float(1.9),
                            ]),
                        ]),
                        create_vector(vec![
                            create_vector(vec![
                                create_float(2.1),
                                create_float(2.2),
                                create_float(2.3),
                            ]),
                            create_vector(vec![
                                create_float(2.4),
                                create_float(2.5),
                                create_float(2.6),
                            ]),
                            create_vector(vec![
                                create_float(2.7),
                                create_float(2.8),
                                create_float(2.9),
                            ]),
                        ]),
                        create_vector(vec![
                            create_vector(vec![
                                create_float(3.1),
                                create_float(3.2),
                                create_float(3.3),
                            ]),
                            create_vector(vec![
                                create_float(3.4),
                                create_float(3.5),
                                create_float(3.6),
                            ]),
                            create_vector(vec![
                                create_float(3.7),
                                create_float(3.8),
                                create_float(3.9),
                            ]),
                        ]),
                    ]),
                ),]),
                DependencyList::empty()
            )
        );
    }

    fn apply_query(query: Expression, root: Expression) -> EvaluationResult {
        Expression::new(Term::Application(ApplicationTerm::new(query, vec![root])))
            .evaluate(&DynamicState::new(), &mut GenerationalGc::new())
    }

    fn create_struct<'a>(fields: impl IntoIterator<Item = (&'a str, Expression)>) -> Expression {
        let (keys, values) = fields
            .into_iter()
            .map(|(key, value)| (ValueTerm::String(String::from(key)), value))
            .unzip();
        Expression::new(Term::Struct(StructTerm::new(
            Some(StructPrototype::new(keys)),
            values,
        )))
    }

    fn create_vector(items: impl IntoIterator<Item = Expression>) -> Expression {
        Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
            items,
        ))))
    }

    fn create_builtin(target: BuiltinTerm) -> Expression {
        Expression::new(Term::Builtin(target))
    }

    fn create_integer(value: i32) -> Expression {
        Expression::new(Term::Value(ValueTerm::Int(value)))
    }

    fn create_float(value: f64) -> Expression {
        Expression::new(Term::Value(ValueTerm::Float(value)))
    }

    fn create_null() -> Expression {
        Expression::new(Term::Value(ValueTerm::Null))
    }

    fn create_lambda(arity: Arity, body: Expression) -> Expression {
        Expression::new(Term::Lambda(LambdaTerm::new(arity, body)))
    }

    fn create_scoped_variable(offset: StackOffset) -> Expression {
        Expression::new(Term::Variable(VariableTerm::scoped(offset)))
    }
}
