// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use serde::{Deserialize, Serialize};

use crate::ast::{
    common::{Directive, Type, Value},
    position::Pos,
};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Document {
    pub definitions: Vec<Definition>,
}
impl<'a> From<&'a graphql_parser::query::Document<'static, String>> for Document {
    fn from(value: &'a graphql_parser::query::Document<'static, String>) -> Self {
        let graphql_parser::query::Document { definitions } = value;
        Self {
            definitions: definitions.iter().map(|value| value.into()).collect(),
        }
    }
}
impl From<Document> for graphql_parser::query::Document<'static, String> {
    fn from(value: Document) -> Self {
        let Document { definitions } = value;
        Self {
            definitions: definitions.into_iter().map(|value| value.into()).collect(),
        }
    }
}
impl std::fmt::Display for Document {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(
            &graphql_parser::query::Document::<'static, String>::from(self.clone()),
            f,
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Definition {
    Operation(OperationDefinition),
    Fragment(FragmentDefinition),
}
impl<'a> From<&'a graphql_parser::query::Definition<'static, String>> for Definition {
    fn from(value: &'a graphql_parser::query::Definition<'static, String>) -> Self {
        match value {
            graphql_parser::query::Definition::Operation(inner) => Self::Operation(inner.into()),
            graphql_parser::query::Definition::Fragment(inner) => Self::Fragment(inner.into()),
        }
    }
}
impl From<Definition> for graphql_parser::query::Definition<'static, String> {
    fn from(value: Definition) -> Self {
        match value {
            Definition::Operation(inner) => Self::Operation(inner.into()),
            Definition::Fragment(inner) => Self::Fragment(inner.into()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct FragmentDefinition {
    pub position: Pos,
    pub name: String,
    pub type_condition: TypeCondition,
    pub directives: Vec<Directive>,
    pub selection_set: SelectionSet,
}
impl<'a> From<&'a graphql_parser::query::FragmentDefinition<'static, String>>
    for FragmentDefinition
{
    fn from(value: &'a graphql_parser::query::FragmentDefinition<'static, String>) -> Self {
        let graphql_parser::query::FragmentDefinition {
            position,
            name,
            type_condition,
            directives,
            selection_set,
        } = value;
        Self {
            position: position.into(),
            name: name.clone(),
            type_condition: type_condition.into(),
            directives: directives.iter().map(|value| value.into()).collect(),
            selection_set: selection_set.into(),
        }
    }
}
impl From<FragmentDefinition> for graphql_parser::query::FragmentDefinition<'static, String> {
    fn from(value: FragmentDefinition) -> Self {
        let FragmentDefinition {
            position,
            name,
            type_condition,
            directives,
            selection_set,
        } = value;
        Self {
            position: position.into(),
            name: name.clone(),
            type_condition: type_condition.into(),
            directives: directives.into_iter().map(|value| value.into()).collect(),
            selection_set: selection_set.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum OperationDefinition {
    SelectionSet(SelectionSet),
    Query(Query),
    Mutation(Mutation),
    Subscription(Subscription),
}
impl<'a> From<&'a graphql_parser::query::OperationDefinition<'static, String>>
    for OperationDefinition
{
    fn from(value: &'a graphql_parser::query::OperationDefinition<'static, String>) -> Self {
        match value {
            graphql_parser::query::OperationDefinition::SelectionSet(inner) => {
                Self::SelectionSet(inner.into())
            }
            graphql_parser::query::OperationDefinition::Query(inner) => Self::Query(inner.into()),
            graphql_parser::query::OperationDefinition::Mutation(inner) => {
                Self::Mutation(inner.into())
            }
            graphql_parser::query::OperationDefinition::Subscription(inner) => {
                Self::Subscription(inner.into())
            }
        }
    }
}
impl From<OperationDefinition> for graphql_parser::query::OperationDefinition<'static, String> {
    fn from(value: OperationDefinition) -> Self {
        match value {
            OperationDefinition::SelectionSet(inner) => Self::SelectionSet(inner.into()),
            OperationDefinition::Query(inner) => Self::Query(inner.into()),
            OperationDefinition::Mutation(inner) => Self::Mutation(inner.into()),
            OperationDefinition::Subscription(inner) => Self::Subscription(inner.into()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Query {
    pub position: Pos,
    pub name: Option<String>,
    pub variable_definitions: Vec<VariableDefinition>,
    pub directives: Vec<Directive>,
    pub selection_set: SelectionSet,
}
impl<'a> From<&'a graphql_parser::query::Query<'static, String>> for Query {
    fn from(value: &'a graphql_parser::query::Query<'static, String>) -> Self {
        let graphql_parser::query::Query {
            position,
            name,
            variable_definitions,
            directives,
            selection_set,
        } = value;
        Self {
            position: position.into(),
            name: name.clone(),
            variable_definitions: variable_definitions
                .iter()
                .map(|value| value.into())
                .collect(),
            directives: directives.iter().map(|value| value.into()).collect(),
            selection_set: selection_set.into(),
        }
    }
}
impl From<Query> for graphql_parser::query::Query<'static, String> {
    fn from(value: Query) -> Self {
        let Query {
            position,
            name,
            variable_definitions,
            directives,
            selection_set,
        } = value;
        Self {
            position: position.into(),
            name: name.clone(),
            variable_definitions: variable_definitions
                .into_iter()
                .map(|value| value.into())
                .collect(),
            directives: directives.into_iter().map(|value| value.into()).collect(),
            selection_set: selection_set.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Mutation {
    pub position: Pos,
    pub name: Option<String>,
    pub variable_definitions: Vec<VariableDefinition>,
    pub directives: Vec<Directive>,
    pub selection_set: SelectionSet,
}
impl<'a> From<&'a graphql_parser::query::Mutation<'static, String>> for Mutation {
    fn from(value: &'a graphql_parser::query::Mutation<'static, String>) -> Self {
        let graphql_parser::query::Mutation {
            position,
            name,
            variable_definitions,
            directives,
            selection_set,
        } = value;
        Self {
            position: position.into(),
            name: name.clone(),
            variable_definitions: variable_definitions
                .iter()
                .map(|value| value.into())
                .collect(),
            directives: directives.iter().map(|value| value.into()).collect(),
            selection_set: selection_set.into(),
        }
    }
}
impl From<Mutation> for graphql_parser::query::Mutation<'static, String> {
    fn from(value: Mutation) -> Self {
        let Mutation {
            position,
            name,
            variable_definitions,
            directives,
            selection_set,
        } = value;
        Self {
            position: position.into(),
            name: name.clone(),
            variable_definitions: variable_definitions
                .into_iter()
                .map(|value| value.into())
                .collect(),
            directives: directives.into_iter().map(|value| value.into()).collect(),
            selection_set: selection_set.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Subscription {
    pub position: Pos,
    pub name: Option<String>,
    pub variable_definitions: Vec<VariableDefinition>,
    pub directives: Vec<Directive>,
    pub selection_set: SelectionSet,
}
impl<'a> From<&'a graphql_parser::query::Subscription<'static, String>> for Subscription {
    fn from(value: &'a graphql_parser::query::Subscription<'static, String>) -> Self {
        let graphql_parser::query::Subscription {
            position,
            name,
            variable_definitions,
            directives,
            selection_set,
        } = value;
        Self {
            position: position.into(),
            name: name.clone(),
            variable_definitions: variable_definitions
                .iter()
                .map(|value| value.into())
                .collect(),
            directives: directives.iter().map(|value| value.into()).collect(),
            selection_set: selection_set.into(),
        }
    }
}
impl From<Subscription> for graphql_parser::query::Subscription<'static, String> {
    fn from(value: Subscription) -> Self {
        let Subscription {
            position,
            name,
            variable_definitions,
            directives,
            selection_set,
        } = value;
        Self {
            position: position.into(),
            name: name.clone(),
            variable_definitions: variable_definitions
                .into_iter()
                .map(|value| value.into())
                .collect(),
            directives: directives.into_iter().map(|value| value.into()).collect(),
            selection_set: selection_set.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SelectionSet {
    pub span: (Pos, Pos),
    pub items: Vec<Selection>,
}
impl<'a> From<&'a graphql_parser::query::SelectionSet<'static, String>> for SelectionSet {
    fn from(value: &'a graphql_parser::query::SelectionSet<'static, String>) -> Self {
        let graphql_parser::query::SelectionSet {
            span: (line, column),
            items,
        } = value;
        Self {
            span: (line.into(), column.into()),
            items: items.iter().map(|item| item.into()).collect(),
        }
    }
}
impl From<SelectionSet> for graphql_parser::query::SelectionSet<'static, String> {
    fn from(value: SelectionSet) -> Self {
        let SelectionSet { span, items } = value;
        Self {
            span: (span.0.into(), span.1.into()),
            items: items.into_iter().map(|item| item.into()).collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct VariableDefinition {
    pub position: Pos,
    pub name: String,
    pub var_type: Type,
    pub default_value: Option<Value>,
}
impl<'a> From<&'a graphql_parser::query::VariableDefinition<'static, String>>
    for VariableDefinition
{
    fn from(value: &'a graphql_parser::query::VariableDefinition<'static, String>) -> Self {
        let graphql_parser::query::VariableDefinition {
            position,
            name,
            var_type,
            default_value,
        } = value;
        Self {
            position: position.into(),
            name: name.clone(),
            var_type: var_type.into(),
            default_value: default_value.as_ref().map(|value| value.into()),
        }
    }
}
impl From<VariableDefinition> for graphql_parser::query::VariableDefinition<'static, String> {
    fn from(value: VariableDefinition) -> Self {
        let VariableDefinition {
            position,
            name,
            var_type,
            default_value,
        } = value;
        Self {
            position: position.into(),
            name: name.clone(),
            var_type: var_type.into(),
            default_value: default_value.map(|value| value.into()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Selection {
    Field(Field),
    FragmentSpread(FragmentSpread),
    InlineFragment(InlineFragment),
}
impl<'a> From<&'a graphql_parser::query::Selection<'static, String>> for Selection {
    fn from(value: &'a graphql_parser::query::Selection<'static, String>) -> Self {
        match value {
            graphql_parser::query::Selection::Field(inner) => Self::Field(inner.into()),
            graphql_parser::query::Selection::FragmentSpread(inner) => {
                Self::FragmentSpread(inner.into())
            }
            graphql_parser::query::Selection::InlineFragment(inner) => {
                Self::InlineFragment(inner.into())
            }
        }
    }
}
impl From<Selection> for graphql_parser::query::Selection<'static, String> {
    fn from(value: Selection) -> Self {
        match value {
            Selection::Field(inner) => Self::Field(inner.into()),
            Selection::FragmentSpread(inner) => Self::FragmentSpread(inner.into()),
            Selection::InlineFragment(inner) => Self::InlineFragment(inner.into()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Field {
    pub position: Pos,
    pub alias: Option<String>,
    pub name: String,
    pub arguments: Vec<(String, Value)>,
    pub directives: Vec<Directive>,
    pub selection_set: SelectionSet,
}
impl<'a> From<&'a graphql_parser::query::Field<'static, String>> for Field {
    fn from(value: &'a graphql_parser::query::Field<'static, String>) -> Self {
        let graphql_parser::query::Field {
            position,
            alias,
            name,
            arguments,
            directives,
            selection_set,
        } = value;
        Self {
            position: position.into(),
            alias: alias.as_ref().map(|value| value.clone()),
            name: name.clone(),
            arguments: arguments
                .iter()
                .map(|(key, value)| (key.clone(), value.into()))
                .collect(),
            directives: directives
                .iter()
                .map(|directive| directive.into())
                .collect(),
            selection_set: selection_set.into(),
        }
    }
}
impl From<Field> for graphql_parser::query::Field<'static, String> {
    fn from(value: Field) -> Self {
        let Field {
            position,
            alias,
            name,
            arguments,
            directives,
            selection_set,
        } = value;
        Self {
            position: position.into(),
            alias: alias.map(|value| value.into()),
            name: name.clone(),
            arguments: arguments
                .into_iter()
                .map(|(key, value)| (key.into(), value.into()))
                .collect(),
            directives: directives
                .into_iter()
                .map(|directive| directive.into())
                .collect(),
            selection_set: selection_set.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct FragmentSpread {
    pub position: Pos,
    pub fragment_name: String,
    pub directives: Vec<Directive>,
}
impl<'a> From<&'a graphql_parser::query::FragmentSpread<'static, String>> for FragmentSpread {
    fn from(value: &'a graphql_parser::query::FragmentSpread<'static, String>) -> Self {
        let graphql_parser::query::FragmentSpread {
            position,
            fragment_name,
            directives,
        } = value;
        Self {
            position: position.into(),
            fragment_name: fragment_name.clone(),
            directives: directives
                .iter()
                .map(|directive| directive.into())
                .collect(),
        }
    }
}
impl From<FragmentSpread> for graphql_parser::query::FragmentSpread<'static, String> {
    fn from(value: FragmentSpread) -> Self {
        let FragmentSpread {
            position,
            fragment_name,
            directives,
        } = value;
        Self {
            position: position.into(),
            fragment_name: fragment_name.clone(),
            directives: directives
                .into_iter()
                .map(|directive| directive.into())
                .collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TypeCondition {
    On(String),
}
impl<'a> From<&'a graphql_parser::query::TypeCondition<'static, String>> for TypeCondition {
    fn from(value: &'a graphql_parser::query::TypeCondition<'static, String>) -> Self {
        match value {
            graphql_parser::query::TypeCondition::On(inner) => Self::On(inner.into()),
        }
    }
}
impl From<TypeCondition> for graphql_parser::query::TypeCondition<'static, String> {
    fn from(value: TypeCondition) -> Self {
        match value {
            TypeCondition::On(inner) => Self::On(inner.into()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct InlineFragment {
    pub position: Pos,
    pub type_condition: Option<TypeCondition>,
    pub directives: Vec<Directive>,
    pub selection_set: SelectionSet,
}
impl<'a> From<&'a graphql_parser::query::InlineFragment<'static, String>> for InlineFragment {
    fn from(value: &'a graphql_parser::query::InlineFragment<'static, String>) -> Self {
        let graphql_parser::query::InlineFragment {
            position,
            type_condition,
            directives,
            selection_set,
        } = value;
        Self {
            position: position.into(),
            type_condition: type_condition.as_ref().map(|value| value.into()),
            directives: directives
                .iter()
                .map(|directive| directive.into())
                .collect(),
            selection_set: selection_set.into(),
        }
    }
}
impl From<InlineFragment> for graphql_parser::query::InlineFragment<'static, String> {
    fn from(value: InlineFragment) -> Self {
        let InlineFragment {
            position,
            type_condition,
            directives,
            selection_set,
        } = value;
        Self {
            position: position.into(),
            type_condition: type_condition.map(|value| value.into()),
            directives: directives
                .into_iter()
                .map(|directive| directive.into())
                .collect(),
            selection_set: selection_set.into(),
        }
    }
}
