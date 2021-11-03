// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{borrow::Cow, collections::HashMap, iter::once, str::FromStr};

use either::Either;
use graphql_parser::{parse_query, query::*};
use reflex::{
    core::{
        ArgType, Arity, Expression, ExpressionFactory, ExpressionList, FunctionArity,
        HeapAllocator, NativeAllocator, SignalType,
    },
    lang::{create_struct, term::SignalTerm, BuiltinTerm, NativeFunction, ValueTerm},
};

mod loader;
pub use loader::graphql_loader;
mod operation;
pub use operation::{deserialize_graphql_operation, GraphQlOperationPayload};
use reflex_json::{json_array, json_object, sanitize, JsonValue};
use uuid::Uuid;

pub mod inject_args;
pub mod subscriptions;
pub use graphql_parser::query as graphql;
pub type GraphQlAst<'a> = Document<'a, GraphQlText<'a>>;
pub type GraphQlText<'a> = std::borrow::Cow<'a, str>;

#[allow(type_alias_bounds)]
type QueryVariables<'a, T: Expression> = HashMap<GraphQlText<'a>, T>;
type QueryFragments<'src, 'a> =
    HashMap<GraphQlText<'src>, &'a FragmentDefinition<'src, GraphQlText<'src>>>;

pub fn graphql_plugins<T: Expression>() -> impl IntoIterator<Item = NativeFunction<T>> {
    vec![dynamic_query_branch(), flatten_deep()]
}

pub fn create_introspection_query_response<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_signal_term(allocator.create_signal_list(once(allocator.create_signal(
        SignalType::Error,
        allocator.create_unit_list(factory.create_value_term(ValueTerm::String(
            allocator.create_string(String::from("Introspection query not yet implemented")),
        ))),
    ))))
}

pub fn create_graphql_success_response<T: Expression>(
    value: T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    create_struct(once((String::from("data"), value)), factory, allocator)
}

pub fn create_graphql_error_response(errors: impl IntoIterator<Item = JsonValue>) -> JsonValue {
    json_object(once((
        String::from("errors"),
        json_array(errors.into_iter().flat_map(parse_graphql_error_payload)),
    )))
}

fn parse_graphql_error_payload(payload: JsonValue) -> Vec<JsonValue> {
    match payload {
        JsonValue::Array(errors) => errors
            .into_iter()
            .flat_map(parse_graphql_error_payload)
            .collect(),
        JsonValue::Object(_) => vec![payload],
        payload => vec![json_object(once((String::from("message"), payload)))],
    }
}

pub fn create_json_error_object(message: String) -> JsonValue {
    json_object(once((String::from("message"), JsonValue::String(message))))
}

pub fn sanitize_signal_errors<T: Expression>(signal: &SignalTerm<T>) -> Vec<JsonValue> {
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
                .unwrap_or_else(|| JsonValue::Null)
        })
        .collect()
}

const DEFAULT_OPERATION_TYPE: &str = "query";

pub trait GraphQlQueryTransform {
    fn transform<'a>(&self, document: GraphQlAst<'a>) -> Result<GraphQlAst<'a>, String>;
}
impl<T> GraphQlQueryTransform for T
where
    T: for<'a> Fn(GraphQlAst<'a>) -> Result<GraphQlAst<'a>, String>,
{
    fn transform<'a>(&self, document: GraphQlAst<'a>) -> Result<GraphQlAst<'a>, String> {
        self(document)
    }
}

pub trait AsyncGraphQlQueryTransform: GraphQlQueryTransform + Send + Sync + 'static {}
impl<T> AsyncGraphQlQueryTransform for T where T: GraphQlQueryTransform + Send + Sync + 'static {}

#[derive(Default)]
pub struct NoopGraphQlQueryTransform {}
impl GraphQlQueryTransform for NoopGraphQlQueryTransform {
    fn transform<'a>(&self, document: GraphQlAst<'a>) -> Result<GraphQlAst<'a>, String> {
        Ok(document)
    }
}

pub fn parse_graphql_operation<T: Expression>(
    operation: &GraphQlOperationPayload,
    root: &T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    transform: &impl GraphQlQueryTransform,
) -> Result<T, String> {
    let variables = operation
        .variables()
        .into_iter()
        .map(|(key, value)| {
            reflex_json::hydrate(value.clone(), factory, allocator).map(|value| (key, value))
        })
        .collect::<Result<Vec<_>, _>>()?;
    let query = parse(operation.query(), variables, factory, allocator, transform)?;
    Ok(factory.create_application_term(query, allocator.create_unit_list(root.clone())))
}

pub fn parse<'vars, T: Expression>(
    source: &str,
    variables: impl IntoIterator<Item = (&'vars str, T)>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    transform: &impl GraphQlQueryTransform,
) -> Result<T, String> {
    let variables = &variables
        .into_iter()
        .map(|(key, value)| (Cow::Owned(String::from(key)), value))
        .collect::<QueryVariables<T>>();
    match parse_query::<GraphQlText>(source) {
        Ok(document) => {
            let document = transform.transform(document)?;
            let fragments = document
                .definitions
                .iter()
                .filter_map(|definition| match definition {
                    Definition::Fragment(fragment) => Some((fragment.name.clone(), fragment)),
                    _ => None,
                })
                .collect::<QueryFragments>();
            match get_root_operation(&document) {
                Err(error) => Err(error),
                Ok(operation) => match operation {
                    OperationDefinition::SelectionSet(selection_set) => parse_root_operation(
                        DEFAULT_OPERATION_TYPE,
                        selection_set,
                        &Vec::new(),
                        variables,
                        &fragments,
                        factory,
                        allocator,
                    ),
                    _ => parse_operation(operation, variables, &fragments, factory, allocator),
                },
            }
        }
        Err(error) => Err(format!("{}", error)),
    }
}

fn get_root_operation<'src, 'a, TText: Text<'src>>(
    document: &'a Document<'src, TText>,
) -> Result<&'a OperationDefinition<'src, TText>, String> {
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

fn parse_operation<'src, T: Expression>(
    operation: &OperationDefinition<'src, GraphQlText<'src>>,
    variables: &QueryVariables<'src, T>,
    fragments: &QueryFragments<'src, '_>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, String> {
    match operation {
        OperationDefinition::Query(operation) => {
            parse_query_operation(operation, variables, fragments, factory, allocator)
        }
        OperationDefinition::Mutation(operation) => {
            parse_mutation_operation(operation, variables, fragments, factory, allocator)
        }
        OperationDefinition::Subscription(operation) => {
            parse_subscription_operation(operation, variables, fragments, factory, allocator)
        }
        OperationDefinition::SelectionSet(selection) => {
            parse_selection_set(selection, variables, fragments, factory, allocator)
        }
    }
}

fn parse_query_operation<'src, T: Expression>(
    operation: &Query<'src, GraphQlText<'src>>,
    variables: &QueryVariables<'src, T>,
    fragments: &QueryFragments<'src, '_>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, String> {
    parse_root_operation(
        "query",
        &operation.selection_set,
        &operation.variable_definitions,
        variables,
        fragments,
        factory,
        allocator,
    )
}

fn parse_mutation_operation<'src, T: Expression>(
    operation: &Mutation<'src, GraphQlText<'src>>,
    variables: &QueryVariables<'src, T>,
    fragments: &QueryFragments<'src, '_>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, String> {
    parse_root_operation(
        "mutation",
        &operation.selection_set,
        &operation.variable_definitions,
        variables,
        fragments,
        factory,
        allocator,
    )
}

fn parse_subscription_operation<'src, T: Expression>(
    operation: &Subscription<'src, GraphQlText<'src>>,
    variables: &QueryVariables<'src, T>,
    fragments: &QueryFragments<'src, '_>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, String> {
    parse_root_operation(
        "subscription",
        &operation.selection_set,
        &operation.variable_definitions,
        variables,
        fragments,
        factory,
        allocator,
    )
}

fn parse_root_operation<'src, T: Expression>(
    operation_type: &str,
    selection_set: &SelectionSet<'src, GraphQlText<'src>>,
    variable_definitions: &[VariableDefinition<'src, GraphQlText<'src>>],
    variables: &QueryVariables<'src, T>,
    fragments: &QueryFragments<'src, '_>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, String> {
    let variables = parse_operation_variables(variable_definitions, variables, factory, allocator)?;
    let query = parse_selection_set(selection_set, &variables, fragments, factory, allocator)?;
    Ok(factory.create_lambda_term(
        1,
        factory.create_application_term(
            query,
            allocator.create_unit_list(factory.create_application_term(
                factory.create_builtin_term(BuiltinTerm::Get),
                allocator.create_pair(
                    factory.create_static_variable_term(0),
                    factory.create_value_term(ValueTerm::String(
                        allocator.create_string(operation_type.into()),
                    )),
                ),
            )),
        ),
    ))
}

fn parse_operation_variables<'src, T: Expression>(
    variable_definitions: &[VariableDefinition<'src, GraphQlText<'src>>],
    variables: &QueryVariables<'src, T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<QueryVariables<'src, T>, String> {
    variable_definitions
        .iter()
        .map(|definition| {
            let value = variables.get(&definition.name).cloned();
            let value = match value {
                Some(value) => Some(value),
                None => match &definition.default_value {
                    Some(value) => Some(parse_value(
                        value,
                        &QueryVariables::new(),
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

fn validate_variable<'src, T: Expression>(
    value: Option<T>,
    name: &GraphQlText<'src>,
    var_type: &Type<'src, GraphQlText<'src>>,
    factory: &impl ExpressionFactory<T>,
) -> Result<T, String> {
    match var_type {
        Type::NonNullType(_) => {
            value.ok_or_else(|| format!("Missing required query variable: {}", name))
        }
        // TODO: Validate query variable types
        // TODO: Differentiate between missing optional variables and null values
        _ => Ok(value.unwrap_or_else(|| factory.create_value_term(ValueTerm::Null))),
    }
}

fn parse_selection_set<'src, T: Expression>(
    selection_set: &SelectionSet<'src, GraphQlText<'src>>,
    variables: &QueryVariables<'src, T>,
    fragments: &QueryFragments<'src, '_>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, String> {
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
                    factory.create_builtin_term(BuiltinTerm::CollectStruct),
                    allocator.create_sized_list(
                        values.len() + 1,
                        once(
                            factory
                                .create_constructor_term(allocator.create_struct_prototype(keys)),
                        )
                        .chain(values.into_iter().map(|query| {
                            factory.create_application_term(
                                query,
                                allocator.create_unit_list(factory.create_static_variable_term(0)),
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

fn parse_leaf<T: Expression>(factory: &impl ExpressionFactory<T>) -> Result<T, String> {
    Ok(factory.create_native_function_term(flatten_deep()))
}

fn parse_selection_set_fields<'src, T: Expression>(
    selection_set: &SelectionSet<'src, GraphQlText<'src>>,
    variables: &QueryVariables<'src, T>,
    fragments: &QueryFragments<'src, '_>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<Vec<(String, T)>, String> {
    selection_set
        .items
        .iter()
        .flat_map(|field| match field {
            Selection::Field(field) => {
                match parse_field(field, variables, fragments, factory, allocator) {
                    Ok(result) => {
                        let key = match field.alias.as_ref().unwrap_or(&field.name) {
                            Cow::Owned(value) => String::from(value),
                            Cow::Borrowed(value) => String::from(*value),
                        };
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
                    fragment.fragment_name
                )))),
            },
            Selection::InlineFragment(_) => Either::Left(once(Err(String::from(
                "Inline fragments not yet implemented",
            )))),
        })
        .collect::<Result<Vec<_>, _>>()
}

fn parse_fragment_fields<'src, T: Expression>(
    fragment: &FragmentDefinition<'src, GraphQlText<'src>>,
    variables: &QueryVariables<'src, T>,
    fragments: &QueryFragments<'src, '_>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> impl IntoIterator<Item = Result<(String, T), String>> {
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

fn parse_field<'src, T: Expression>(
    field: &Field<'src, GraphQlText<'src>>,
    variables: &QueryVariables<'src, T>,
    fragments: &QueryFragments<'src, '_>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, String> {
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
                    factory.create_builtin_term(BuiltinTerm::Get),
                    allocator.create_pair(
                        factory.create_static_variable_term(0),
                        factory.create_value_term(ValueTerm::String(allocator.create_string(
                            match &field.name {
                                Cow::Owned(value) => String::from(value),
                                Cow::Borrowed(value) => String::from(*value),
                            },
                        ))),
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

fn parse_field_arguments<'src, T: Expression>(
    args: &Vec<(GraphQlText<'src>, Value<'src, GraphQlText<'src>>)>,
    variables: &QueryVariables<'src, T>,
    fragments: &QueryFragments<'src, '_>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<ExpressionList<T>, String> {
    let arg_fields = args
        .iter()
        .map(
            |(key, value)| match parse_value(value, variables, fragments, factory, allocator) {
                Ok(value) => Ok((
                    match key {
                        Cow::Owned(value) => String::from(value),
                        Cow::Borrowed(value) => String::from(*value),
                    },
                    value,
                )),
                Err(error) => Err(error),
            },
        )
        .collect::<Result<Vec<_>, _>>()?;
    let arg = create_struct(arg_fields, factory, allocator);
    Ok(allocator.create_unit_list(arg))
}

fn parse_value<'src, T: Expression>(
    value: &Value<'src, GraphQlText<'src>>,
    variables: &QueryVariables<'src, T>,
    fragments: &QueryFragments<'src, '_>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, String> {
    match value {
        Value::Variable(name) => match variables.get(name) {
            Some(value) => Ok(value.clone()),
            None => Err(format!("Undeclared query variable: {}", name)),
        },
        Value::Int(value) => Ok(factory.create_value_term(ValueTerm::Int(
            value
                .as_i64()
                .ok_or_else(|| format!("Invalid integer argument: {:?}", value))?
                as i32,
        ))),
        Value::Float(value) => Ok(factory.create_value_term(ValueTerm::Float(*value))),
        Value::String(value) => {
            Ok(factory.create_value_term(ValueTerm::String(allocator.create_string(value.into()))))
        }
        Value::Boolean(value) => Ok(factory.create_value_term(ValueTerm::Boolean(*value))),
        Value::Null => Ok(factory.create_value_term(ValueTerm::Null)),
        Value::Enum(value) => Ok(factory.create_value_term(ValueTerm::String(
            allocator.create_string(match value {
                Cow::Owned(value) => String::from(value),
                Cow::Borrowed(value) => String::from(*value),
            }),
        ))),
        Value::List(value) => {
            let values = value
                .iter()
                .map(|item| parse_value(item, variables, fragments, factory, allocator))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(factory.create_vector_term(allocator.create_list(values)))
        }
        Value::Object(value) => {
            let entries = value
                .iter()
                .map(|(key, value)| {
                    match parse_value(value, variables, fragments, factory, allocator) {
                        Ok(value) => Ok((
                            match key {
                                Cow::Owned(value) => String::from(value),
                                Cow::Borrowed(value) => String::from(*value),
                            },
                            value,
                        )),
                        Err(error) => Err(error),
                    }
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(create_struct(entries, factory, allocator))
        }
    }
}

fn create_query_branch<T: Expression>(
    shape: T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_lambda_term(
        1,
        factory.create_application_term(
            factory.create_native_function_term(dynamic_query_branch()),
            allocator.create_pair(factory.create_static_variable_term(0), shape),
        ),
    )
}

fn flatten_deep<T: Expression>() -> NativeFunction<T> {
    NativeFunction::new_with_uuid(
        FlattenDeep::uid(),
        FlattenDeep::name(),
        FlattenDeep::arity(),
        FlattenDeep::apply,
    )
}
struct FlattenDeep {}
impl FlattenDeep {
    const NAME: &'static str = "FlattenDeep";
    const UUID: &'static str = "52299fbb-a84b-488e-81ef-98c439869f74";
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
    fn uid() -> Uuid {
        Uuid::from_str(Self::UUID).unwrap()
    }
    fn name() -> Option<&'static str> {
        Some(Self::NAME)
    }
    fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
    fn apply<T: Expression>(
        args: ExpressionList<T>,
        factory: &dyn ExpressionFactory<T>,
        allocator: &dyn NativeAllocator<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
        let target = args.next().unwrap();
        if let Some(ValueTerm::Null) = factory.match_value_term(&target) {
            Ok(target)
        } else if let Some(list) = factory.match_vector_term(&target) {
            Ok(factory.create_application_term(
                factory.create_builtin_term(BuiltinTerm::CollectVector),
                allocator.create_list(
                    list.items()
                        .iter()
                        .map(|item| {
                            factory.create_application_term(
                                factory.create_native_function_term(flatten_deep()),
                                allocator.create_unit_list(item.clone()),
                            )
                        })
                        .collect(),
                ),
            ))
        } else {
            Ok(target)
        }
    }
}

fn dynamic_query_branch<T: Expression>() -> NativeFunction<T> {
    NativeFunction::new_with_uuid(
        DynamicQueryBranch::uid(),
        DynamicQueryBranch::name(),
        DynamicQueryBranch::arity(),
        DynamicQueryBranch::apply,
    )
}
struct DynamicQueryBranch {}
impl DynamicQueryBranch {
    const NAME: &'static str = "DynamicQueryBranch";
    const UUID: &'static str = "58dd19b7-c9f0-473b-84c5-34af607c176b";
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
    fn uid() -> Uuid {
        Uuid::from_str(Self::UUID).unwrap()
    }
    fn name() -> Option<&'static str> {
        Some(Self::NAME)
    }
    fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
    fn apply<T: Expression>(
        args: ExpressionList<T>,
        factory: &dyn ExpressionFactory<T>,
        allocator: &dyn NativeAllocator<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
        let target = args.next().unwrap();
        let shape = args.next().unwrap();
        if let Some(ValueTerm::Null) = factory.match_value_term(&target) {
            Ok(target)
        } else if let Some(list) = factory.match_vector_term(&target) {
            Ok(factory.create_application_term(
                factory.create_builtin_term(BuiltinTerm::CollectVector),
                allocator.create_list(
                    list.items()
                        .iter()
                        .map(|item| {
                            factory.create_application_term(
                                factory.create_native_function_term(dynamic_query_branch()),
                                allocator.create_pair(item.clone(), shape.clone()),
                            )
                        })
                        .collect(),
                ),
            ))
        } else {
            Ok(factory.create_application_term(shape, allocator.create_unit_list(target)))
        }
    }
}

#[cfg(test)]
mod tests {
    use reflex::{
        allocator::DefaultAllocator,
        cache::SubstitutionCache,
        core::{
            evaluate, DependencyList, Evaluate, EvaluationResult, Expression, ExpressionFactory,
            HeapAllocator, Reducible, Rewritable, StateCache,
        },
        lang::{create_struct, BuiltinTerm, TermFactory, ValueTerm},
    };

    use super::{parse, NoopGraphQlQueryTransform};

    #[test]
    fn leaf_queries() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let root = create_struct(
            vec![
                (
                    String::from("query"),
                    create_struct(
                        vec![
                            (
                                String::from("first"),
                                factory.create_value_term(ValueTerm::Int(3)),
                            ),
                            (
                                String::from("second"),
                                factory.create_value_term(ValueTerm::Int(4)),
                            ),
                            (
                                String::from("third"),
                                factory.create_value_term(ValueTerm::Int(5)),
                            ),
                        ],
                        &factory,
                        &allocator,
                    ),
                ),
                (
                    String::from("mutation"),
                    factory.create_value_term(ValueTerm::Null),
                ),
                (
                    String::from("subscription"),
                    factory.create_value_term(ValueTerm::Null),
                ),
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
            &NoopGraphQlQueryTransform::default(),
        )
        .unwrap();
        let result = apply_query(query, root, &factory, &allocator);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_struct(
                    vec![
                        (
                            String::from("second"),
                            factory.create_value_term(ValueTerm::Int(4))
                        ),
                        (
                            String::from("third"),
                            factory.create_value_term(ValueTerm::Int(5))
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
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let root = create_struct(
            vec![
                (
                    String::from("query"),
                    factory.create_application_term(
                        factory.create_lambda_term(
                            1,
                            create_struct(
                                vec![
                                    (
                                        String::from("first"),
                                        factory.create_application_term(
                                            factory.create_builtin_term(BuiltinTerm::Add),
                                            allocator.create_pair(
                                                factory.create_value_term(ValueTerm::Int(3)),
                                                factory.create_static_variable_term(0),
                                            ),
                                        ),
                                    ),
                                    (
                                        String::from("second"),
                                        factory.create_application_term(
                                            factory.create_builtin_term(BuiltinTerm::Add),
                                            allocator.create_pair(
                                                factory.create_value_term(ValueTerm::Int(4)),
                                                factory.create_static_variable_term(0),
                                            ),
                                        ),
                                    ),
                                    (
                                        String::from("third"),
                                        factory.create_application_term(
                                            factory.create_builtin_term(BuiltinTerm::Add),
                                            allocator.create_pair(
                                                factory.create_value_term(ValueTerm::Int(5)),
                                                factory.create_static_variable_term(0),
                                            ),
                                        ),
                                    ),
                                ],
                                &factory,
                                &allocator,
                            ),
                        ),
                        allocator.create_unit_list(factory.create_value_term(ValueTerm::Int(10))),
                    ),
                ),
                (
                    String::from("mutation"),
                    factory.create_value_term(ValueTerm::Null),
                ),
                (
                    String::from("subscription"),
                    factory.create_value_term(ValueTerm::Null),
                ),
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
            &NoopGraphQlQueryTransform::default(),
        )
        .unwrap();
        let result = apply_query(query, root, &factory, &allocator);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_struct(
                    vec![
                        (
                            String::from("second"),
                            factory.create_value_term(ValueTerm::Int(4 + 10))
                        ),
                        (
                            String::from("third"),
                            factory.create_value_term(ValueTerm::Int(5 + 10))
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
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let root = create_struct(
            vec![
                (
                    String::from("query"),
                    create_struct(
                        vec![
                            (
                                String::from("foo"),
                                factory.create_value_term(ValueTerm::Null),
                            ),
                            (
                                String::from("items"),
                                factory.create_vector_term(allocator.create_list(vec![
                                    factory.create_value_term(ValueTerm::Int(3)),
                                    factory.create_value_term(ValueTerm::Int(4)),
                                    factory.create_value_term(ValueTerm::Int(5)),
                                ])),
                            ),
                            (
                                String::from("bar"),
                                factory.create_value_term(ValueTerm::Null),
                            ),
                        ],
                        &factory,
                        &allocator,
                    ),
                ),
                (
                    String::from("mutation"),
                    factory.create_value_term(ValueTerm::Null),
                ),
                (
                    String::from("subscription"),
                    factory.create_value_term(ValueTerm::Null),
                ),
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
            &NoopGraphQlQueryTransform::default(),
        )
        .unwrap();
        let result = apply_query(query, root, &factory, &allocator);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_struct(
                    vec![(
                        String::from("items"),
                        factory.create_vector_term(allocator.create_list(vec![
                            factory.create_value_term(ValueTerm::Int(3)),
                            factory.create_value_term(ValueTerm::Int(4)),
                            factory.create_value_term(ValueTerm::Int(5))
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
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let root = create_struct(
            vec![
                (
                    String::from("query"),
                    create_struct(
                        vec![
                            (
                                String::from("foo"),
                                factory.create_value_term(ValueTerm::Null),
                            ),
                            (
                                String::from("items"),
                                factory.create_vector_term(allocator.create_list(vec![
                                    factory.create_vector_term(allocator.create_list(vec![
                                        factory.create_vector_term(allocator.create_list(vec![
                                            factory.create_value_term(ValueTerm::Float(1.1)),
                                            factory.create_value_term(ValueTerm::Float(1.2)),
                                            factory.create_value_term(ValueTerm::Float(1.3)),
                                        ])),
                                        factory.create_vector_term(allocator.create_list(vec![
                                            factory.create_value_term(ValueTerm::Float(1.4)),
                                            factory.create_value_term(ValueTerm::Float(1.5)),
                                            factory.create_value_term(ValueTerm::Float(1.6)),
                                        ])),
                                        factory.create_vector_term(allocator.create_list(vec![
                                            factory.create_value_term(ValueTerm::Float(1.7)),
                                            factory.create_value_term(ValueTerm::Float(1.8)),
                                            factory.create_value_term(ValueTerm::Float(1.9)),
                                        ])),
                                    ])),
                                    factory.create_vector_term(allocator.create_list(vec![
                                        factory.create_vector_term(allocator.create_list(vec![
                                            factory.create_value_term(ValueTerm::Float(2.1)),
                                            factory.create_value_term(ValueTerm::Float(2.2)),
                                            factory.create_value_term(ValueTerm::Float(2.3)),
                                        ])),
                                        factory.create_vector_term(allocator.create_list(vec![
                                            factory.create_value_term(ValueTerm::Float(2.4)),
                                            factory.create_value_term(ValueTerm::Float(2.5)),
                                            factory.create_value_term(ValueTerm::Float(2.6)),
                                        ])),
                                        factory.create_vector_term(allocator.create_list(vec![
                                            factory.create_value_term(ValueTerm::Float(2.7)),
                                            factory.create_value_term(ValueTerm::Float(2.8)),
                                            factory.create_value_term(ValueTerm::Float(2.9)),
                                        ])),
                                    ])),
                                    factory.create_vector_term(allocator.create_list(vec![
                                        factory.create_vector_term(allocator.create_list(vec![
                                            factory.create_value_term(ValueTerm::Float(3.1)),
                                            factory.create_value_term(ValueTerm::Float(3.2)),
                                            factory.create_value_term(ValueTerm::Float(3.3)),
                                        ])),
                                        factory.create_vector_term(allocator.create_list(vec![
                                            factory.create_value_term(ValueTerm::Float(3.4)),
                                            factory.create_value_term(ValueTerm::Float(3.5)),
                                            factory.create_value_term(ValueTerm::Float(3.6)),
                                        ])),
                                        factory.create_vector_term(allocator.create_list(vec![
                                            factory.create_value_term(ValueTerm::Float(3.7)),
                                            factory.create_value_term(ValueTerm::Float(3.8)),
                                            factory.create_value_term(ValueTerm::Float(3.9)),
                                        ])),
                                    ])),
                                ])),
                            ),
                            (
                                String::from("bar"),
                                factory.create_value_term(ValueTerm::Null),
                            ),
                        ],
                        &factory,
                        &allocator,
                    ),
                ),
                (
                    String::from("mutation"),
                    factory.create_value_term(ValueTerm::Null),
                ),
                (
                    String::from("subscription"),
                    factory.create_value_term(ValueTerm::Null),
                ),
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
            &NoopGraphQlQueryTransform::default(),
        )
        .unwrap();
        let result = apply_query(query, root, &factory, &allocator);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_struct(
                    vec![(
                        String::from("items"),
                        factory.create_vector_term(allocator.create_list(vec![
                            factory.create_vector_term(allocator.create_list(vec![
                                factory.create_vector_term(allocator.create_list(vec![
                                    factory.create_value_term(ValueTerm::Float(1.1)),
                                    factory.create_value_term(ValueTerm::Float(1.2)),
                                    factory.create_value_term(ValueTerm::Float(1.3)),
                                ])),
                                factory.create_vector_term(allocator.create_list(vec![
                                    factory.create_value_term(ValueTerm::Float(1.4)),
                                    factory.create_value_term(ValueTerm::Float(1.5)),
                                    factory.create_value_term(ValueTerm::Float(1.6)),
                                ])),
                                factory.create_vector_term(allocator.create_list(vec![
                                    factory.create_value_term(ValueTerm::Float(1.7)),
                                    factory.create_value_term(ValueTerm::Float(1.8)),
                                    factory.create_value_term(ValueTerm::Float(1.9)),
                                ])),
                            ])),
                            factory.create_vector_term(allocator.create_list(vec![
                                factory.create_vector_term(allocator.create_list(vec![
                                    factory.create_value_term(ValueTerm::Float(2.1)),
                                    factory.create_value_term(ValueTerm::Float(2.2)),
                                    factory.create_value_term(ValueTerm::Float(2.3)),
                                ])),
                                factory.create_vector_term(allocator.create_list(vec![
                                    factory.create_value_term(ValueTerm::Float(2.4)),
                                    factory.create_value_term(ValueTerm::Float(2.5)),
                                    factory.create_value_term(ValueTerm::Float(2.6)),
                                ])),
                                factory.create_vector_term(allocator.create_list(vec![
                                    factory.create_value_term(ValueTerm::Float(2.7)),
                                    factory.create_value_term(ValueTerm::Float(2.8)),
                                    factory.create_value_term(ValueTerm::Float(2.9)),
                                ])),
                            ])),
                            factory.create_vector_term(allocator.create_list(vec![
                                factory.create_vector_term(allocator.create_list(vec![
                                    factory.create_value_term(ValueTerm::Float(3.1)),
                                    factory.create_value_term(ValueTerm::Float(3.2)),
                                    factory.create_value_term(ValueTerm::Float(3.3)),
                                ])),
                                factory.create_vector_term(allocator.create_list(vec![
                                    factory.create_value_term(ValueTerm::Float(3.4)),
                                    factory.create_value_term(ValueTerm::Float(3.5)),
                                    factory.create_value_term(ValueTerm::Float(3.6)),
                                ])),
                                factory.create_vector_term(allocator.create_list(vec![
                                    factory.create_value_term(ValueTerm::Float(3.7)),
                                    factory.create_value_term(ValueTerm::Float(3.8)),
                                    factory.create_value_term(ValueTerm::Float(3.9)),
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
