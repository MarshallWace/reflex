// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{
    core::{
        hash_struct_field_offset, ApplicationTerm, Arity, Expression, LambdaTerm, MatcherTerm,
        NativeFunction, Signal, SignalTerm, StackOffset, StaticVariableTerm, StructFieldOffset,
        Term, VariableTerm,
    },
    hash::{combine_hashes, hash_seed, hash_sequence, prefix_hash, HashId, Hashable},
    stdlib::{
        signal::SignalType,
        value::{IntValue, ValueTerm},
    },
};

use super::BuiltinTerm;

pub struct Query {}
impl NativeFunction for Query {
    fn arity() -> Arity {
        Arity::from(1, 1, None)
    }
    fn apply(args: impl IntoIterator<Item = Expression> + ExactSizeIterator) -> Expression {
        if args.len() != 2 {
            return Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![ValueTerm::String(format!(
                    "Expected 2 arguments, received {}",
                    args.len(),
                ))],
            ))));
        }
        let mut args = args.into_iter();
        let shape = args.next().unwrap();
        let target = args.next().unwrap();
        match shape.value() {
            Term::Value(ValueTerm::QueryShape(shape)) => evaluate_query(target, shape),
            _ => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![ValueTerm::String(format!(
                    "Invalid query access: Expected (<shape>, <any>), received ({}, {})",
                    shape, target,
                ))],
            )))),
        }
    }
}

fn evaluate_query(target: Expression, shape: &QueryShape) -> Expression {
    match shape {
        QueryShape::Leaf => target,
        QueryShape::Branch(selectors) => evaluate_branch_query(target, selectors),
        QueryShape::List(shape) => evaluate_list_query(target, shape),
    }
}

fn evaluate_branch_query(target: Expression, selectors: &[FieldSelector]) -> Expression {
    Expression::new(Term::Application(ApplicationTerm::new(
        Expression::new(Term::Builtin(BuiltinTerm::Array)),
        selectors
            .iter()
            .map(|selector| evaluate_field(Expression::clone(&target), selector))
            .collect(),
    )))
}

fn evaluate_field(target: Expression, selector: &FieldSelector) -> Expression {
    match selector {
        FieldSelector::EnumField(variants) => evaluate_enum_field(target, variants),
        FieldSelector::StructField(field_offset, shape) => {
            evaluate_struct_field(target, *field_offset, shape)
        }
        FieldSelector::FunctionField(args, shape) => evaluate_function_field(target, args, shape),
    }
}

fn evaluate_struct_field(
    target: Expression,
    field_offset: StructFieldOffset,
    shape: &QueryShape,
) -> Expression {
    evaluate_query(
        Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Get)),
            vec![
                target,
                Expression::new(Term::Value(ValueTerm::Int(field_offset as IntValue))),
            ],
        ))),
        shape,
    )
}

fn evaluate_enum_field(target: Expression, variants: &[EnumFieldSelector]) -> Expression {
    Expression::new(Term::Application(ApplicationTerm::new(
        Expression::new(Term::Builtin(BuiltinTerm::Match)),
        vec![
            Expression::clone(&target),
            Expression::new(Term::Matcher(MatcherTerm::new(
                variants
                    .iter()
                    .enumerate()
                    .map(|(index, variant)| {
                        let enum_index = Expression::new(Term::Value(ValueTerm::Int(index as i32)));
                        let num_args = match variant {
                            EnumFieldSelector::Nullary(_) => 0,
                            EnumFieldSelector::Unary(_) => 1,
                            EnumFieldSelector::Multiple(num_args, _, _) => *num_args,
                        };
                        let result = match variant {
                            EnumFieldSelector::Nullary(value) => Expression::clone(value),
                            EnumFieldSelector::Unary(shape) => evaluate_query(
                                Expression::new(Term::Variable(VariableTerm::Static(
                                    StaticVariableTerm::new(0),
                                ))),
                                shape,
                            ),
                            EnumFieldSelector::Multiple(num_args, transform, shape) => {
                                let num_args = *num_args;
                                evaluate_query(
                                    Expression::new(Term::Application(ApplicationTerm::new(
                                        Expression::clone(transform),
                                        (0..num_args)
                                            .into_iter()
                                            .map(|index| {
                                                let offset = (num_args - index - 1) as StackOffset;
                                                Expression::new(Term::Variable(
                                                    VariableTerm::Static(StaticVariableTerm::new(
                                                        offset,
                                                    )),
                                                ))
                                            })
                                            .collect(),
                                    ))),
                                    shape,
                                )
                            }
                        };
                        Expression::new(Term::Lambda(LambdaTerm::new(
                            Arity::from(num_args, 0, None),
                            Expression::new(Term::Application(ApplicationTerm::new(
                                Expression::new(Term::Builtin(BuiltinTerm::Array)),
                                vec![enum_index, result],
                            ))),
                        )))
                    })
                    .collect(),
            ))),
        ],
    )))
}

fn evaluate_function_field(
    target: Expression,
    args: &[Expression],
    shape: &QueryShape,
) -> Expression {
    evaluate_query(
        Expression::new(Term::Application(ApplicationTerm::new(
            target,
            args.iter().map(|arg| Expression::clone(arg)).collect(),
        ))),
        shape,
    )
}

fn evaluate_list_query(target: Expression, shape: &QueryShape) -> Expression {
    Expression::new(Term::Application(ApplicationTerm::new(
        Expression::new(Term::Builtin(BuiltinTerm::Collect)),
        vec![Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Map)),
            vec![
                target,
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 1, None),
                    evaluate_query(
                        Expression::new(Term::Variable(VariableTerm::Static(
                            StaticVariableTerm::new(0),
                        ))),
                        shape,
                    ),
                ))),
            ],
        )))],
    )))
}

#[derive(PartialEq, Clone, Debug)]
pub enum QueryShape {
    Leaf,
    Branch(Vec<FieldSelector>),
    List(Box<QueryShape>),
}
impl Hashable for QueryShape {
    fn hash(&self) -> HashId {
        match self {
            QueryShape::Leaf => prefix_hash(0, hash_seed()),
            QueryShape::Branch(branches) => prefix_hash(
                1,
                hash_sequence(branches.iter().map(|selector| selector.hash())),
            ),
            QueryShape::List(shape) => prefix_hash(2, shape.hash()),
        }
    }
}
impl QueryShape {
    pub fn branch(fields: Vec<FieldSelector>) -> Self {
        Self::Branch(fields)
    }
    pub fn list(shape: QueryShape) -> Self {
        Self::List(Box::new(shape))
    }
}
impl fmt::Display for QueryShape {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            QueryShape::Leaf => write!(f, "<leaf>"),
            QueryShape::Branch(branches) => write!(
                f,
                "<branch:{{{}}}>",
                branches
                    .iter()
                    .map(|selector| format!("{}", selector))
                    .collect::<Vec<_>>()
                    .join(",")
            ),
            QueryShape::List(shape) => write!(f, "<list:{}>", shape),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum FieldSelector {
    StructField(StructFieldOffset, QueryShape),
    EnumField(Vec<EnumFieldSelector>),
    FunctionField(Vec<Expression>, QueryShape),
}
impl Hashable for FieldSelector {
    fn hash(&self) -> HashId {
        match self {
            FieldSelector::StructField(offset, shape) => prefix_hash(
                0,
                combine_hashes(hash_struct_field_offset(*offset), shape.hash()),
            ),
            FieldSelector::EnumField(variants) => prefix_hash(
                1,
                hash_sequence(variants.iter().map(|variant| variant.hash())),
            ),
            FieldSelector::FunctionField(args, shape) => prefix_hash(
                2,
                combine_hashes(
                    hash_sequence(args.iter().map(|arg| arg.hash())),
                    shape.hash(),
                ),
            ),
        }
    }
}
impl fmt::Display for FieldSelector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FieldSelector::StructField(_, shape) => fmt::Display::fmt(shape, f),
            FieldSelector::EnumField(variants) => write!(
                f,
                "{}",
                variants
                    .iter()
                    .map(|variant| format!("{}", variant))
                    .collect::<Vec<_>>()
                    .join(" | ")
            ),
            FieldSelector::FunctionField(_, shape) => fmt::Display::fmt(shape, f),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum EnumFieldSelector {
    Nullary(Expression),
    Unary(QueryShape),
    Multiple(u8, Expression, QueryShape),
}
impl Hashable for EnumFieldSelector {
    fn hash(&self) -> HashId {
        match self {
            Self::Nullary(value) => prefix_hash(0, value.hash()),
            Self::Unary(shape) => prefix_hash(1, shape.hash()),
            Self::Multiple(num_args, transform, shape) => prefix_hash(
                1,
                combine_hashes(prefix_hash(*num_args, transform.hash()), shape.hash()),
            ),
        }
    }
}
impl fmt::Display for EnumFieldSelector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Nullary(value) => fmt::Display::fmt(value, f),
            Self::Unary(shape) => fmt::Display::fmt(shape, f),
            Self::Multiple(_, _, shape) => fmt::Display::fmt(shape, f),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{EnumFieldSelector, FieldSelector, QueryShape};
    use crate::{
        core::{
            ApplicationTerm, DataStructureTerm, DependencyList, DynamicState, EnumTerm,
            EvaluationResult, Expression, Signal, SignalTerm, StructTerm, Term,
        },
        parser::sexpr::parse,
        stdlib::{
            builtin::BuiltinTerm,
            collection::{vector::VectorTerm, CollectionTerm},
            signal::SignalType,
            value::{ArrayValue, StringValue, ValueTerm},
        },
    };

    #[test]
    fn leaf_queries() {
        let state = DynamicState::new();
        let root = parse("(+ 3 4)").unwrap();
        let shape = QueryShape::Leaf;
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Query)),
            vec![
                Expression::new(Term::Value(ValueTerm::QueryShape(shape))),
                root,
            ],
        )));
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(3 + 4)))),
                DependencyList::empty(),
            )
        );
    }

    #[test]
    fn struct_fields() {
        let state = DynamicState::new();
        let root = Expression::new(Term::DataStructure(DataStructureTerm::Struct(
            StructTerm::new(vec![
                parse("(+ 0 1)").unwrap(),
                parse("(+ 1 1)").unwrap(),
                parse("(+ 2 1)").unwrap(),
                parse("(+ 3 1)").unwrap(),
                parse("(+ 4 1)").unwrap(),
                parse("(+ 5 1)").unwrap(),
                parse("(+ 6 1)").unwrap(),
            ]),
        )));
        let shape = QueryShape::branch(vec![
            FieldSelector::StructField(3, QueryShape::Leaf),
            FieldSelector::StructField(4, QueryShape::Leaf),
            FieldSelector::StructField(5, QueryShape::Leaf),
        ]);
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Query)),
            vec![
                Expression::new(Term::Value(ValueTerm::QueryShape(shape))),
                root,
            ],
        )));
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Array(
                    ArrayValue::new(vec![
                        ValueTerm::Int(3 + 1),
                        ValueTerm::Int(4 + 1),
                        ValueTerm::Int(5 + 1)
                    ]),
                ))),),
                DependencyList::empty(),
            )
        );
    }

    #[test]
    fn enum_fields_nullary() {
        let state = DynamicState::new();
        let root = Expression::new(Term::DataStructure(DataStructureTerm::Enum(EnumTerm::new(
            2,
            vec![],
        ))));
        let error = Expression::new(Term::Signal(SignalTerm::Single(Signal::new(
            SignalType::Error,
            vec![ValueTerm::String(StringValue::from("foo"))],
        ))));
        let shape = QueryShape::branch(vec![FieldSelector::EnumField(vec![
            EnumFieldSelector::Nullary(Expression::clone(&error)),
            EnumFieldSelector::Nullary(Expression::clone(&error)),
            EnumFieldSelector::Nullary(Expression::new(Term::Value(ValueTerm::Int(3)))),
            EnumFieldSelector::Nullary(Expression::clone(&error)),
            EnumFieldSelector::Nullary(Expression::clone(&error)),
            EnumFieldSelector::Nullary(Expression::clone(&error)),
        ])]);
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Query)),
            vec![
                Expression::new(Term::Value(ValueTerm::QueryShape(shape))),
                root,
            ],
        )));
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Array(
                    ArrayValue::new(vec![ValueTerm::Array(ArrayValue::new(vec![
                        ValueTerm::Int(2),
                        ValueTerm::Int(3),
                    ]))]),
                )))),
                DependencyList::empty(),
            )
        );
    }

    #[test]
    fn enum_fields_unary() {
        let state = DynamicState::new();
        let root = Expression::new(Term::DataStructure(DataStructureTerm::Enum(EnumTerm::new(
            2,
            vec![Expression::new(Term::Value(ValueTerm::Int(3)))],
        ))));
        let error = Expression::new(Term::Signal(SignalTerm::Single(Signal::new(
            SignalType::Error,
            vec![ValueTerm::String(StringValue::from("foo"))],
        ))));
        let shape = QueryShape::branch(vec![FieldSelector::EnumField(vec![
            EnumFieldSelector::Nullary(Expression::clone(&error)),
            EnumFieldSelector::Nullary(Expression::clone(&error)),
            EnumFieldSelector::Unary(QueryShape::Leaf),
            EnumFieldSelector::Nullary(Expression::clone(&error)),
            EnumFieldSelector::Nullary(Expression::clone(&error)),
            EnumFieldSelector::Nullary(Expression::clone(&error)),
        ])]);
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Query)),
            vec![
                Expression::new(Term::Value(ValueTerm::QueryShape(shape))),
                root,
            ],
        )));
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Array(
                    ArrayValue::new(vec![ValueTerm::Array(ArrayValue::new(vec![
                        ValueTerm::Int(2),
                        ValueTerm::Int(3),
                    ]))]),
                )))),
                DependencyList::empty(),
            )
        );
    }

    #[test]
    fn enum_fields_multiple() {
        let state = DynamicState::new();
        let root = Expression::new(Term::DataStructure(DataStructureTerm::Enum(EnumTerm::new(
            2,
            vec![
                Expression::new(Term::Value(ValueTerm::Int(3))),
                Expression::new(Term::Value(ValueTerm::Int(4))),
            ],
        ))));
        let transform = Expression::new(Term::Builtin(BuiltinTerm::Add));
        let error = Expression::new(Term::Signal(SignalTerm::Single(Signal::new(
            SignalType::Error,
            vec![ValueTerm::String(StringValue::from("foo"))],
        ))));
        let shape = QueryShape::branch(vec![FieldSelector::EnumField(vec![
            EnumFieldSelector::Nullary(Expression::clone(&error)),
            EnumFieldSelector::Nullary(Expression::clone(&error)),
            EnumFieldSelector::Multiple(2, Expression::clone(&transform), QueryShape::Leaf),
            EnumFieldSelector::Nullary(Expression::clone(&error)),
            EnumFieldSelector::Nullary(Expression::clone(&error)),
            EnumFieldSelector::Nullary(Expression::clone(&error)),
        ])]);
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Query)),
            vec![
                Expression::new(Term::Value(ValueTerm::QueryShape(shape))),
                root,
            ],
        )));
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Array(
                    ArrayValue::new(vec![ValueTerm::Array(ArrayValue::new(vec![
                        ValueTerm::Int(2),
                        ValueTerm::Int(3 + 4),
                    ]))]),
                )))),
                DependencyList::empty(),
            )
        );
    }

    #[test]
    fn function_fields() {
        let state = DynamicState::new();
        let root = Expression::new(Term::Builtin(BuiltinTerm::Add));
        let shape = QueryShape::branch(vec![
            FieldSelector::FunctionField(
                vec![
                    Expression::new(Term::Value(ValueTerm::Int(3))),
                    Expression::new(Term::Value(ValueTerm::Int(4))),
                ],
                QueryShape::Leaf,
            ),
            FieldSelector::FunctionField(
                vec![
                    Expression::new(Term::Value(ValueTerm::Int(5))),
                    Expression::new(Term::Value(ValueTerm::Int(6))),
                ],
                QueryShape::Leaf,
            ),
            FieldSelector::FunctionField(
                vec![
                    Expression::new(Term::Value(ValueTerm::Int(7))),
                    Expression::new(Term::Value(ValueTerm::Int(8))),
                ],
                QueryShape::Leaf,
            ),
        ]);
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Query)),
            vec![
                Expression::new(Term::Value(ValueTerm::QueryShape(shape))),
                root,
            ],
        )));
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Array(
                    ArrayValue::new(vec![
                        ValueTerm::Int(3 + 4),
                        ValueTerm::Int(5 + 6),
                        ValueTerm::Int(7 + 8),
                    ]),
                ))),),
                DependencyList::empty(),
            )
        );
    }

    #[test]
    fn deeply_nested_fields() {
        let state = DynamicState::new();
        let root = Expression::new(Term::DataStructure(DataStructureTerm::Struct(
            StructTerm::new(vec![
                Expression::new(Term::DataStructure(DataStructureTerm::Struct(
                    StructTerm::new(vec![
                        Expression::new(Term::DataStructure(DataStructureTerm::Struct(
                            StructTerm::new(vec![
                                Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                    "0:0:0",
                                )))),
                                Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                    "0:0:1",
                                )))),
                                Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                    "0:0:2",
                                )))),
                            ]),
                        ))),
                        Expression::new(Term::DataStructure(DataStructureTerm::Struct(
                            StructTerm::new(vec![
                                Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                    "0:1:0",
                                )))),
                                Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                    "0:1:1",
                                )))),
                                Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                    "0:1:2",
                                )))),
                            ]),
                        ))),
                        Expression::new(Term::DataStructure(DataStructureTerm::Struct(
                            StructTerm::new(vec![
                                Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                    "0:2:0",
                                )))),
                                Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                    "0:2:1",
                                )))),
                                Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                    "0:2:2",
                                )))),
                            ]),
                        ))),
                    ]),
                ))),
                Expression::new(Term::DataStructure(DataStructureTerm::Struct(
                    StructTerm::new(vec![
                        Expression::new(Term::DataStructure(DataStructureTerm::Struct(
                            StructTerm::new(vec![
                                Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                    "1:0:0",
                                )))),
                                Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                    "1:0:1",
                                )))),
                                Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                    "1:0:2",
                                )))),
                            ]),
                        ))),
                        Expression::new(Term::DataStructure(DataStructureTerm::Struct(
                            StructTerm::new(vec![
                                Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                    "1:1:0",
                                )))),
                                Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                    "1:1:1",
                                )))),
                                Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                    "1:1:2",
                                )))),
                            ]),
                        ))),
                        Expression::new(Term::DataStructure(DataStructureTerm::Struct(
                            StructTerm::new(vec![
                                Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                    "1:2:0",
                                )))),
                                Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                    "1:2:1",
                                )))),
                                Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                    "1:2:2",
                                )))),
                            ]),
                        ))),
                    ]),
                ))),
                Expression::new(Term::DataStructure(DataStructureTerm::Struct(
                    StructTerm::new(vec![
                        Expression::new(Term::DataStructure(DataStructureTerm::Struct(
                            StructTerm::new(vec![
                                Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                    "2:0:0",
                                )))),
                                Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                    "2:0:1",
                                )))),
                                Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                    "2:0:2",
                                )))),
                            ]),
                        ))),
                        Expression::new(Term::DataStructure(DataStructureTerm::Struct(
                            StructTerm::new(vec![
                                Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                    "2:1:0",
                                )))),
                                Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                    "2:1:1",
                                )))),
                                Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                    "2:1:2",
                                )))),
                            ]),
                        ))),
                        Expression::new(Term::DataStructure(DataStructureTerm::Struct(
                            StructTerm::new(vec![
                                Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                    "2:2:0",
                                )))),
                                Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                    "2:2:1",
                                )))),
                                Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                    "2:2:2",
                                )))),
                            ]),
                        ))),
                    ]),
                ))),
            ]),
        )));
        let shape = QueryShape::branch(vec![
            FieldSelector::StructField(
                0,
                QueryShape::branch(vec![
                    FieldSelector::StructField(
                        1,
                        QueryShape::branch(vec![
                            FieldSelector::StructField(0, QueryShape::Leaf),
                            FieldSelector::StructField(2, QueryShape::Leaf),
                        ]),
                    ),
                    FieldSelector::StructField(
                        2,
                        QueryShape::branch(vec![FieldSelector::StructField(1, QueryShape::Leaf)]),
                    ),
                ]),
            ),
            FieldSelector::StructField(
                2,
                QueryShape::branch(vec![
                    FieldSelector::StructField(
                        0,
                        QueryShape::branch(vec![FieldSelector::StructField(1, QueryShape::Leaf)]),
                    ),
                    FieldSelector::StructField(
                        2,
                        QueryShape::branch(vec![
                            FieldSelector::StructField(1, QueryShape::Leaf),
                            FieldSelector::StructField(2, QueryShape::Leaf),
                        ]),
                    ),
                ]),
            ),
        ]);
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Query)),
            vec![
                Expression::new(Term::Value(ValueTerm::QueryShape(shape))),
                root,
            ],
        )));
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Array(
                    ArrayValue::new(vec![
                        ValueTerm::Array(ArrayValue::new(vec![
                            ValueTerm::Array(ArrayValue::new(vec![
                                ValueTerm::String(StringValue::from("0:1:0")),
                                ValueTerm::String(StringValue::from("0:1:2")),
                            ])),
                            ValueTerm::Array(ArrayValue::new(vec![ValueTerm::String(
                                StringValue::from("0:2:1")
                            )])),
                        ])),
                        ValueTerm::Array(ArrayValue::new(vec![
                            ValueTerm::Array(ArrayValue::new(vec![ValueTerm::String(
                                StringValue::from("2:0:1"),
                            )])),
                            ValueTerm::Array(ArrayValue::new(vec![
                                ValueTerm::String(StringValue::from("2:2:1")),
                                ValueTerm::String(StringValue::from("2:2:2")),
                            ])),
                        ])),
                    ]),
                )))),
                DependencyList::empty(),
            )
        );
    }

    #[test]
    fn list_queries() {
        let state = DynamicState::new();
        let root = Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
            vec![
                parse("(+ 3 1)").unwrap(),
                parse("(+ 4 1)").unwrap(),
                parse("(+ 5 1)").unwrap(),
            ],
        ))));
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Query)),
            vec![
                Expression::new(Term::Value(ValueTerm::QueryShape(QueryShape::list(
                    QueryShape::Leaf,
                )))),
                root,
            ],
        )));
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Array(
                    ArrayValue::new(vec![
                        ValueTerm::Int(3 + 1),
                        ValueTerm::Int(4 + 1),
                        ValueTerm::Int(5 + 1),
                    ])
                )))),
                DependencyList::empty(),
            )
        );

        let root = Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
            vec![
                Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
                    vec![
                        parse("(+ 3 1)").unwrap(),
                        parse("(+ 4 1)").unwrap(),
                        parse("(+ 5 1)").unwrap(),
                    ],
                )))),
                Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
                    vec![
                        parse("(+ 3 2)").unwrap(),
                        parse("(+ 4 2)").unwrap(),
                        parse("(+ 5 2)").unwrap(),
                    ],
                )))),
                Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
                    vec![
                        parse("(+ 3 3)").unwrap(),
                        parse("(+ 4 3)").unwrap(),
                        parse("(+ 5 3)").unwrap(),
                    ],
                )))),
            ],
        ))));
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Query)),
            vec![
                Expression::new(Term::Value(ValueTerm::QueryShape(QueryShape::list(
                    QueryShape::list(QueryShape::Leaf),
                )))),
                root,
            ],
        )));
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Array(
                    ArrayValue::new(vec![
                        ValueTerm::Array(ArrayValue::new(vec![
                            ValueTerm::Int(3 + 1),
                            ValueTerm::Int(4 + 1),
                            ValueTerm::Int(5 + 1),
                        ])),
                        ValueTerm::Array(ArrayValue::new(vec![
                            ValueTerm::Int(3 + 2),
                            ValueTerm::Int(4 + 2),
                            ValueTerm::Int(5 + 2),
                        ])),
                        ValueTerm::Array(ArrayValue::new(vec![
                            ValueTerm::Int(3 + 3),
                            ValueTerm::Int(4 + 3),
                            ValueTerm::Int(5 + 3),
                        ])),
                    ])
                )))),
                DependencyList::empty(),
            )
        );
    }
}
