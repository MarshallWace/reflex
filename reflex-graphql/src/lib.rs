// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{borrow::Cow, collections::HashMap, iter::once};

use either::Either;
use graphql_parser::{parse_query, query::*};
use reflex::{
    core::{Expression, ExpressionFactory, ExpressionList, HeapAllocator, SignalType},
    lang::{create_struct, term::SignalTerm, ValueTerm},
    stdlib::Stdlib,
};
use reflex_json::{json_object, sanitize, JsonMap, JsonValue};

pub use graphql_parser;

mod operation;
pub use operation::{
    deserialize_graphql_operation, graphql_variables_are_equal, GraphQlOperationPayload,
    GraphQlQuery,
};
pub mod stdlib;
pub mod transform;
pub mod validate_query;
use stdlib::Stdlib as GraphQlStdlib;
pub mod subscriptions;

#[allow(type_alias_bounds)]
type QueryVariables<'a, TText: Text<'a>, T: Expression> = HashMap<TText::Value, T>;
#[allow(type_alias_bounds)]
type QueryFragments<'src, 'a, TText: GraphQlText<'src>> =
    HashMap<TText::Value, &'a FragmentDefinition<'src, TText>>;

pub trait GraphQlQueryTransform {
    fn transform<'src, T: GraphQlText<'src>>(
        &self,
        document: Document<'src, T>,
        extensions: GraphQlExtensions,
    ) -> Result<(Document<'src, T>, GraphQlExtensions), String>;
}

pub struct ChainedGraphQlQueryTransform<T1: GraphQlQueryTransform, T2: GraphQlQueryTransform> {
    left: T1,
    right: T2,
}
impl<T1: GraphQlQueryTransform, T2: GraphQlQueryTransform> GraphQlQueryTransform
    for ChainedGraphQlQueryTransform<T1, T2>
{
    fn transform<'src, T: GraphQlText<'src>>(
        &self,
        document: Document<'src, T>,
        extensions: GraphQlExtensions,
    ) -> Result<(Document<'src, T>, GraphQlExtensions), String> {
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
    Text<'src, Value = Self>
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

pub type GraphQlExtensions = JsonMap<String, JsonValue>;

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
    message: String,
    metadata: impl IntoIterator<Item = (String, JsonValue)>,
) -> JsonValue {
    json_object(once((String::from("message"), JsonValue::String(message))).chain(metadata))
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
    operation: &GraphQlOperationPayload,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, String>
where
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
{
    let variables = operation
        .variables()
        .map(|(key, value)| {
            reflex_json::hydrate(value.clone(), factory, allocator).map(|value| (key, value))
        })
        .collect::<Result<Vec<_>, _>>()?;
    parse(operation.query(), variables, factory, allocator)
}

pub fn parse<'vars, 'a, T: Expression>(
    query: &'a GraphQlQuery,
    variables: impl IntoIterator<Item = (&'vars str, T)>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, String>
where
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
{
    match query {
        GraphQlQuery::Ast(query) => parse_ast_query(
            query,
            variables
                .into_iter()
                .map(|(key, value)| (String::from(key), value)),
            factory,
            allocator,
        ),
        GraphQlQuery::Source(query) => match parse_query::<std::borrow::Cow<'a, str>>(query) {
            Ok(query) => parse_ast_query(
                &query,
                variables
                    .into_iter()
                    .map(|(key, value)| (Cow::Owned(String::from(key)), value)),
                factory,
                allocator,
            ),
            Err(error) => Err(format!("{}", error)),
        },
    }
}

pub fn parse_ast_query<'src, 'vars, T: Expression, TText: GraphQlText<'src>>(
    query: &Document<'src, TText>,
    variables: impl IntoIterator<Item = (TText::Value, T)> + 'vars,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, String>
where
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
{
    let variables = variables.into_iter().collect::<QueryVariables<TText, T>>();
    let fragments = query
        .definitions
        .iter()
        .filter_map(|definition| match definition {
            Definition::Fragment(fragment) => Some((fragment.name.clone(), fragment)),
            _ => None,
        })
        .collect::<QueryFragments<TText>>();
    match get_root_operation(&query) {
        Err(error) => Err(error),
        Ok(operation) => parse_operation(operation, &variables, &fragments, factory, allocator),
    }
}

fn get_root_operation<'src, 'a, TText: GraphQlText<'src>>(
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

fn parse_operation<'src, T: Expression, TText: GraphQlText<'src>>(
    operation: &OperationDefinition<'src, TText>,
    variables: &QueryVariables<'src, TText, T>,
    fragments: &QueryFragments<'src, '_, TText>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, String>
where
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
{
    let (selection_set, variable_definitions) = match operation {
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
    let operation_type = match operation {
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
                    factory.create_static_variable_term(0),
                    factory.create_value_term(ValueTerm::String(
                        allocator.create_static_string(operation_type),
                    )),
                ),
            )),
        ),
    )
}

fn parse_operation_variables<'src, T: Expression, TText: GraphQlText<'src>>(
    variable_definitions: &[VariableDefinition<'src, TText>],
    variables: &QueryVariables<'src, TText, T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<QueryVariables<'src, TText, T>, String> {
    variable_definitions
        .iter()
        .map(|definition| {
            let value = variables.get(definition.name.as_ref()).cloned();
            let value = match value {
                Some(value) => Some(value),
                None => match &definition.default_value {
                    Some(value) => Some(parse_value(
                        value,
                        &QueryVariables::<TText, T>::new(),
                        &QueryFragments::<TText>::new(),
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
        .collect::<Result<QueryVariables<TText, T>, _>>()
}

fn validate_variable<'src, T: Expression, TText: GraphQlText<'src>>(
    value: Option<T>,
    name: &TText,
    var_type: &Type<'src, TText>,
    factory: &impl ExpressionFactory<T>,
) -> Result<T, String> {
    match var_type {
        Type::NonNullType(_) => {
            value.ok_or_else(|| format!("Missing required query variable: {}", name.to_string()))
        }
        // TODO: Validate query variable types
        // TODO: Differentiate between missing optional variables and null values
        _ => Ok(value.unwrap_or_else(|| factory.create_value_term(ValueTerm::Null))),
    }
}

fn parse_selection_set<'src, T: Expression, TText: GraphQlText<'src>>(
    selection_set: &SelectionSet<'src, TText>,
    variables: &QueryVariables<'src, TText, T>,
    fragments: &QueryFragments<'src, '_, TText>,
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
                    factory.create_builtin_term(Stdlib::CollectStruct),
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

fn parse_leaf<T: Expression>(factory: &impl ExpressionFactory<T>) -> Result<T, String>
where
    T::Builtin: From<GraphQlStdlib>,
{
    Ok(factory.create_builtin_term(GraphQlStdlib::FlattenDeep))
}

fn parse_selection_set_fields<'src, T: Expression, TText: GraphQlText<'src>>(
    selection_set: &SelectionSet<'src, TText>,
    variables: &QueryVariables<'src, TText, T>,
    fragments: &QueryFragments<'src, '_, TText>,
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
            Selection::FragmentSpread(fragment) => match fragments
                .get(fragment.fragment_name.as_ref())
            {
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

fn parse_fragment_fields<'src, T: Expression, TText: GraphQlText<'src>>(
    fragment: &FragmentDefinition<'src, TText>,
    variables: &QueryVariables<'src, TText, T>,
    fragments: &QueryFragments<'src, '_, TText>,
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

fn parse_field<'src, T: Expression, TText: GraphQlText<'src>>(
    field: &Field<'src, TText>,
    variables: &QueryVariables<'src, TText, T>,
    fragments: &QueryFragments<'src, '_, TText>,
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
                        factory.create_static_variable_term(0),
                        factory.create_value_term(ValueTerm::String(
                            allocator.create_string(field.name.to_string()),
                        )),
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

fn parse_field_arguments<'src, T: Expression, TText: GraphQlText<'src>>(
    args: &Vec<(TText, Value<'src, TText>)>,
    variables: &QueryVariables<'src, TText, T>,
    fragments: &QueryFragments<'src, '_, TText>,
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
    let arg = create_struct(arg_fields, factory, allocator);
    Ok(allocator.create_unit_list(arg))
}

fn parse_value<'src, T: Expression, TText: GraphQlText<'src>>(
    value: &Value<'src, TText>,
    variables: &QueryVariables<'src, TText, T>,
    fragments: &QueryFragments<'src, '_, TText>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, String> {
    match value {
        Value::Variable(name) => match variables.get(name.as_ref()) {
            Some(value) => Ok(value.clone()),
            None => Err(format!("Undeclared query variable: {}", name.to_string())),
        },
        Value::Int(value) => Ok(factory.create_value_term(ValueTerm::Int(
            value
                .as_i64()
                .ok_or_else(|| format!("Invalid integer argument: {:?}", value))?
                as i32,
        ))),
        Value::Float(value) => Ok(factory.create_value_term(ValueTerm::Float(*value))),
        Value::String(value) => {
            Ok(factory
                .create_value_term(ValueTerm::String(allocator.create_string(value.as_str()))))
        }
        Value::Boolean(value) => Ok(factory.create_value_term(ValueTerm::Boolean(*value))),
        Value::Null => Ok(factory.create_value_term(ValueTerm::Null)),
        Value::Enum(value) => Ok(factory.create_value_term(ValueTerm::String(
            allocator.create_string(value.to_string()),
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
                        Ok(value) => Ok((key.to_string(), value)),
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
) -> T
where
    T::Builtin: From<GraphQlStdlib>,
{
    factory.create_lambda_term(
        1,
        factory.create_application_term(
            factory.create_builtin_term(GraphQlStdlib::DynamicQueryBranch),
            allocator.create_pair(factory.create_static_variable_term(0), shape),
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
        lang::{create_struct, SharedTermFactory, ValueTerm},
        stdlib::Stdlib,
    };
    use std::convert::{TryFrom, TryInto};

    use crate::operation::GraphQlQuery;

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
        fn arity<T: Expression<Builtin = Self> + Applicable<T>>(&self) -> Option<Arity> {
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
            &GraphQlQuery::Source(String::from(
                "
                query {
                    second
                    third
                }
            ",
            )),
            variables,
            &factory,
            &allocator,
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
        let factory = SharedTermFactory::<GraphQlTestBuiltins>::default();
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
                                            factory.create_builtin_term(Stdlib::Add),
                                            allocator.create_pair(
                                                factory.create_value_term(ValueTerm::Int(3)),
                                                factory.create_static_variable_term(0),
                                            ),
                                        ),
                                    ),
                                    (
                                        String::from("second"),
                                        factory.create_application_term(
                                            factory.create_builtin_term(Stdlib::Add),
                                            allocator.create_pair(
                                                factory.create_value_term(ValueTerm::Int(4)),
                                                factory.create_static_variable_term(0),
                                            ),
                                        ),
                                    ),
                                    (
                                        String::from("third"),
                                        factory.create_application_term(
                                            factory.create_builtin_term(Stdlib::Add),
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
            &GraphQlQuery::Source(String::from(
                "
                query {
                    second
                    third
                }
            ",
            )),
            variables,
            &factory,
            &allocator,
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
        let factory = SharedTermFactory::<GraphQlTestBuiltins>::default();
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
            &GraphQlQuery::Source(String::from(
                "
                query {
                    items
                }
            ",
            )),
            variables,
            &factory,
            &allocator,
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
        let factory = SharedTermFactory::<GraphQlTestBuiltins>::default();
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
            &GraphQlQuery::Source(String::from(
                "
                query {
                    items
                }
            ",
            )),
            variables,
            &factory,
            &allocator,
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
