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
use reflex_json::{json_array, json_object, sanitize, JsonValue};

pub use graphql_parser::query as graphql;

mod imports;
pub use imports::graphql_imports;
mod loader;
pub use loader::graphql_loader;
mod operation;
pub use operation::{deserialize_graphql_operation, GraphQlOperationPayload};
pub mod inject_args;
pub mod stdlib;
use stdlib::Stdlib as GraphQlStdlib;
pub mod subscriptions;
pub type GraphQlAst<'a> = Document<'a, GraphQlText<'a>>;
pub type GraphQlText<'a> = std::borrow::Cow<'a, str>;

#[allow(type_alias_bounds)]
type QueryVariables<'a, T: Expression> = HashMap<GraphQlText<'a>, T>;
type QueryFragments<'src, 'a> =
    HashMap<GraphQlText<'src>, &'a FragmentDefinition<'src, GraphQlText<'src>>>;

pub fn create_introspection_query_response<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_signal_term(allocator.create_signal_list(once(allocator.create_signal(
        SignalType::Error,
        allocator.create_unit_list(factory.create_value_term(ValueTerm::String(
            allocator.create_static_string("Introspection query not yet implemented"),
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

impl GraphQlQueryTransform for Box<dyn GraphQlQueryTransform> {
    fn transform<'a>(&self, document: GraphQlAst<'a>) -> Result<GraphQlAst<'a>, String> {
        (&**self).transform(document)
    }
}
impl GraphQlQueryTransform for Box<dyn AsyncGraphQlQueryTransform> {
    fn transform<'a>(&self, document: GraphQlAst<'a>) -> Result<GraphQlAst<'a>, String> {
        (&**self).transform(document)
    }
}

#[derive(Default)]
pub struct NoopGraphQlQueryTransform {}
impl GraphQlQueryTransform for NoopGraphQlQueryTransform {
    fn transform<'a>(&self, document: GraphQlAst<'a>) -> Result<GraphQlAst<'a>, String> {
        Ok(document)
    }
}

pub fn parse_graphql_operation<T: Expression>(
    operation: &GraphQlOperationPayload,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    transform: &impl GraphQlQueryTransform,
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
    parse(operation.query(), variables, factory, allocator, transform)
}

pub fn parse<'vars, T: Expression>(
    source: &str,
    variables: impl IntoIterator<Item = (&'vars str, T)>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    transform: &impl GraphQlQueryTransform,
) -> Result<T, String>
where
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
{
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
                Ok(operation) => {
                    parse_operation(operation, variables, &fragments, factory, allocator)
                }
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

fn parse_selection_set_fields<'src, T: Expression>(
    selection_set: &SelectionSet<'src, GraphQlText<'src>>,
    variables: &QueryVariables<'src, T>,
    fragments: &QueryFragments<'src, '_>,
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

fn parse_field<'src, T: Expression>(
    field: &Field<'src, GraphQlText<'src>>,
    variables: &QueryVariables<'src, T>,
    fragments: &QueryFragments<'src, '_>,
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
            Ok(factory
                .create_value_term(ValueTerm::String(allocator.create_string(value.as_str()))))
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

    use super::{parse, stdlib::Stdlib as GraphQlStdlib, NoopGraphQlQueryTransform};

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
