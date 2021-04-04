// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{fmt, iter::once, rc::Rc};

use crate::{
    env::Env,
    expression::{
        AstNode, CompoundNode, EvaluationResult, Expression, NodeFactoryResult, NodeType,
        RuntimeState,
    },
    hash::{hash_bytes, hash_string, prefix_hash},
    node::{core::CoreNode, evaluate::EvaluateResult, Evaluate1, Node},
};

#[derive(Debug, PartialEq, Clone)]
pub enum ValueNode {
    Nil,
    Boolean(bool),
    Int(i32),
    Float(f64),
    String(StringValue),
}
impl NodeType<Node> for ValueNode {
    fn hash(&self) -> u32 {
        match self {
            ValueNode::Nil => prefix_hash(0, 0),
            ValueNode::Boolean(value) => prefix_hash(1, {
                if !*value {
                    1
                } else {
                    2
                }
            }),
            ValueNode::Int(value) => prefix_hash(2, hash_bytes(&value.to_be_bytes())),
            ValueNode::Float(value) => prefix_hash(
                3,
                if *value == 0.0 {
                    0
                } else {
                    hash_bytes(&value.to_be_bytes())
                },
            ),
            ValueNode::String(value) => prefix_hash(4, hash_string(value.get())),
        }
    }
    fn capture_depth(&self) -> usize {
        0
    }
    fn evaluate(
        &self,
        _env: &Env<Node>,
        _state: &RuntimeState<Node>,
    ) -> Option<EvaluationResult<Node>> {
        None
    }
}
impl fmt::Display for ValueNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let formatted = match self {
            ValueNode::Nil => String::from("null"),
            ValueNode::Boolean(value) => String::from(if *value { "#t" } else { "#f" }),
            ValueNode::Int(value) => format!("{:?}", value),
            ValueNode::Float(value) => format!("{:?}", value),
            ValueNode::String(value) => format!("{}", value),
        };
        write!(f, "{}", formatted)
    }
}

#[derive(Clone)]
pub enum StringValue {
    Literal(&'static str),
    Runtime(Rc<String>),
}
impl StringValue {
    pub fn literal(value: &'static str) -> StringValue {
        StringValue::Literal(value)
    }
    pub fn new(value: String) -> Self {
        StringValue::Runtime(Rc::new(value))
    }
    pub fn get(&self) -> &str {
        match self {
            StringValue::Literal(value) => value,
            StringValue::Runtime(value) => value,
        }
    }
}
impl PartialEq for StringValue {
    fn eq(&self, other: &StringValue) -> bool {
        match (self, other) {
            (&StringValue::Literal(left), &StringValue::Literal(right)) => {
                std::ptr::eq(left, right)
            }
            (left, right) => left.get() == right.get(),
        }
    }
}
impl fmt::Display for StringValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.get())
    }
}
impl fmt::Debug for StringValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}({:?})",
            match self {
                StringValue::Literal(_) => "Literal",
                StringValue::Runtime(_) => "Runtime",
            },
            self.get()
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IsNullNode {
    target: Expression<Node>,
}
impl IsNullNode {
    pub fn new(target: Expression<Node>) -> Self {
        IsNullNode { target }
    }
}
impl AstNode<Node> for IsNullNode {
    fn factory(args: &[Expression<Node>]) -> NodeFactoryResult<Self> {
        if args.len() != 1 {
            return Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.iter().map(Expression::clone);
        let target = args.next().unwrap();
        Ok(Self::new(target))
    }
}
impl<'a> CompoundNode<'a, Node> for IsNullNode {
    type Expressions = std::iter::Once<&'a Expression<Node>>;
    fn expressions(&'a self) -> Self::Expressions {
        once(&self.target)
    }
}
impl NodeType<Node> for IsNullNode {
    fn hash(&self) -> u32 {
        CompoundNode::hash(self)
    }
    fn capture_depth(&self) -> usize {
        CompoundNode::capture_depth(self)
    }
    fn evaluate(
        &self,
        env: &Env<Node>,
        state: &RuntimeState<Node>,
    ) -> Option<EvaluationResult<Node>> {
        Evaluate1::evaluate(self, env, state)
    }
}
impl Evaluate1 for IsNullNode {
    fn dependencies(&self) -> &Expression<Node> {
        &self.target
    }
    fn run(&self, target: &Expression<Node>) -> EvaluateResult {
        Ok(match target.value() {
            Node::Core(CoreNode::Value(ValueNode::Nil)) => {
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
            }
            _ => Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false)))),
        })
    }
}
impl fmt::Display for IsNullNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IsBooleanNode {
    target: Expression<Node>,
}
impl IsBooleanNode {
    pub fn new(target: Expression<Node>) -> Self {
        IsBooleanNode { target }
    }
}
impl AstNode<Node> for IsBooleanNode {
    fn factory(args: &[Expression<Node>]) -> NodeFactoryResult<Self> {
        if args.len() != 1 {
            return Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.iter().map(Expression::clone);
        let target = args.next().unwrap();
        Ok(Self::new(target))
    }
}
impl<'a> CompoundNode<'a, Node> for IsBooleanNode {
    type Expressions = std::iter::Once<&'a Expression<Node>>;
    fn expressions(&'a self) -> Self::Expressions {
        once(&self.target)
    }
}
impl NodeType<Node> for IsBooleanNode {
    fn hash(&self) -> u32 {
        CompoundNode::hash(self)
    }
    fn capture_depth(&self) -> usize {
        CompoundNode::capture_depth(self)
    }
    fn evaluate(
        &self,
        env: &Env<Node>,
        state: &RuntimeState<Node>,
    ) -> Option<EvaluationResult<Node>> {
        Evaluate1::evaluate(self, env, state)
    }
}
impl Evaluate1 for IsBooleanNode {
    fn dependencies(&self) -> &Expression<Node> {
        &self.target
    }
    fn run(&self, target: &Expression<Node>) -> EvaluateResult {
        Ok(match target.value() {
            Node::Core(CoreNode::Value(ValueNode::Boolean(_))) => {
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
            }
            _ => Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false)))),
        })
    }
}
impl fmt::Display for IsBooleanNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IsIntegerNode {
    target: Expression<Node>,
}
impl IsIntegerNode {
    pub fn new(target: Expression<Node>) -> Self {
        IsIntegerNode { target }
    }
}
impl AstNode<Node> for IsIntegerNode {
    fn factory(args: &[Expression<Node>]) -> NodeFactoryResult<Self> {
        if args.len() != 1 {
            return Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.iter().map(Expression::clone);
        let target = args.next().unwrap();
        Ok(Self::new(target))
    }
}
impl<'a> CompoundNode<'a, Node> for IsIntegerNode {
    type Expressions = std::iter::Once<&'a Expression<Node>>;
    fn expressions(&'a self) -> Self::Expressions {
        once(&self.target)
    }
}
impl NodeType<Node> for IsIntegerNode {
    fn hash(&self) -> u32 {
        CompoundNode::hash(self)
    }
    fn capture_depth(&self) -> usize {
        CompoundNode::capture_depth(self)
    }
    fn evaluate(
        &self,
        env: &Env<Node>,
        state: &RuntimeState<Node>,
    ) -> Option<EvaluationResult<Node>> {
        Evaluate1::evaluate(self, env, state)
    }
}
impl Evaluate1 for IsIntegerNode {
    fn dependencies(&self) -> &Expression<Node> {
        &self.target
    }
    fn run(&self, target: &Expression<Node>) -> EvaluateResult {
        Ok(match target.value() {
            Node::Core(CoreNode::Value(ValueNode::Int(_))) => {
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
            }
            _ => Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false)))),
        })
    }
}
impl fmt::Display for IsIntegerNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IsFloatNode {
    target: Expression<Node>,
}
impl IsFloatNode {
    pub fn new(target: Expression<Node>) -> Self {
        IsFloatNode { target }
    }
}
impl AstNode<Node> for IsFloatNode {
    fn factory(args: &[Expression<Node>]) -> NodeFactoryResult<Self> {
        if args.len() != 1 {
            return Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.iter().map(Expression::clone);
        let target = args.next().unwrap();
        Ok(Self::new(target))
    }
}
impl<'a> CompoundNode<'a, Node> for IsFloatNode {
    type Expressions = std::iter::Once<&'a Expression<Node>>;
    fn expressions(&'a self) -> Self::Expressions {
        once(&self.target)
    }
}
impl NodeType<Node> for IsFloatNode {
    fn hash(&self) -> u32 {
        CompoundNode::hash(self)
    }
    fn capture_depth(&self) -> usize {
        CompoundNode::capture_depth(self)
    }
    fn evaluate(
        &self,
        env: &Env<Node>,
        state: &RuntimeState<Node>,
    ) -> Option<EvaluationResult<Node>> {
        Evaluate1::evaluate(self, env, state)
    }
}
impl Evaluate1 for IsFloatNode {
    fn dependencies(&self) -> &Expression<Node> {
        &self.target
    }
    fn run(&self, target: &Expression<Node>) -> EvaluateResult {
        Ok(match target.value() {
            Node::Core(CoreNode::Value(ValueNode::Float(_))) => {
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
            }
            _ => Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false)))),
        })
    }
}
impl fmt::Display for IsFloatNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IsStringNode {
    target: Expression<Node>,
}
impl IsStringNode {
    pub fn new(target: Expression<Node>) -> Self {
        IsStringNode { target }
    }
}
impl AstNode<Node> for IsStringNode {
    fn factory(args: &[Expression<Node>]) -> NodeFactoryResult<Self> {
        if args.len() != 1 {
            return Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.iter().map(Expression::clone);
        let target = args.next().unwrap();
        Ok(Self::new(target))
    }
}
impl<'a> CompoundNode<'a, Node> for IsStringNode {
    type Expressions = std::iter::Once<&'a Expression<Node>>;
    fn expressions(&'a self) -> Self::Expressions {
        once(&self.target)
    }
}
impl NodeType<Node> for IsStringNode {
    fn hash(&self) -> u32 {
        CompoundNode::hash(self)
    }
    fn capture_depth(&self) -> usize {
        CompoundNode::capture_depth(self)
    }
    fn evaluate(
        &self,
        env: &Env<Node>,
        state: &RuntimeState<Node>,
    ) -> Option<EvaluationResult<Node>> {
        Evaluate1::evaluate(self, env, state)
    }
}
impl Evaluate1 for IsStringNode {
    fn dependencies(&self) -> &Expression<Node> {
        &self.target
    }
    fn run(&self, target: &Expression<Node>) -> EvaluateResult {
        Ok(match target.value() {
            Node::Core(CoreNode::Value(ValueNode::String(_))) => {
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
            }
            _ => Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false)))),
        })
    }
}
impl fmt::Display for IsStringNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        env::Env,
        expression::{EvaluationResult, Expression, RuntimeState},
        node::{core::CoreNode, parser, Node},
    };

    use super::{StringValue, ValueNode};

    #[test]
    fn static_values_nil() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("null").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(ValueNode::Nil))))
        );
    }

    #[test]
    fn static_values_boolean() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("#t").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );
        let expression = parser::parse("#f").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
    }

    #[test]
    fn static_values_int() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("0").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(0)
            ))))
        );
        let expression = parser::parse("3").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(3)
            ))))
        );
        let expression = parser::parse("-3").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(-3)
            ))))
        );
    }

    #[test]
    fn static_values_float() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("0.0").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Float(0.0)
            ))))
        );
        let expression = parser::parse("3.142").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Float(3.142)
            ))))
        );
        let expression = parser::parse("-3.142").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Float(-3.142)
            ))))
        );
    }

    #[test]
    fn static_values_string() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("\"\"").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::String(StringValue::new(String::from("")))
            ))))
        );
        let expression = parser::parse("\"foo\"").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::String(StringValue::new(String::from("foo")))
            ))))
        );
        let expression = Expression::new(Node::Core(CoreNode::Value(ValueNode::String(
            StringValue::literal(""),
        ))));
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::String(StringValue::literal(""))
            ))))
        );
        let expression = Expression::new(Node::Core(CoreNode::Value(ValueNode::String(
            StringValue::literal("foo"),
        ))));
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::String(StringValue::literal("foo"))
            ))))
        );
    }

    #[test]
    fn string_equality() {
        let left = StringValue::literal("foo");
        let right = StringValue::literal("foo");
        assert_eq!(left, right);
        let left = StringValue::literal("foo");
        let right = StringValue::literal("bar");
        assert_ne!(left, right);
        let left = StringValue::new(String::from("foo"));
        let right = StringValue::new(String::from("foo"));
        assert_eq!(left, right);
        let left = StringValue::new(String::from("foo"));
        let right = StringValue::new(String::from("bar"));
        assert_ne!(left, right);
        let left = StringValue::literal("foo");
        let right = StringValue::new(String::from("foo"));
        assert_eq!(left, right);
        let left = StringValue::literal("foo");
        let right = StringValue::new(String::from("bar"));
        assert_ne!(left, right);
        let left = StringValue::new(String::from("foo"));
        let right = StringValue::literal("foo");
        assert_eq!(left, right);
        let left = StringValue::new(String::from("foo"));
        let right = StringValue::literal("bar");
        assert_ne!(left, right);
    }

    #[test]
    fn format_values() {
        assert_eq!(format!("{}", ValueNode::Nil), "null");
        assert_eq!(format!("{}", ValueNode::Boolean(true)), "#t");
        assert_eq!(format!("{}", ValueNode::Boolean(false)), "#f");
        assert_eq!(format!("{}", ValueNode::Int(0)), "0");
        assert_eq!(format!("{}", ValueNode::Int(3)), "3");
        assert_eq!(format!("{}", ValueNode::Int(-3)), "-3");
        assert_eq!(format!("{}", ValueNode::Float(0.0)), "0.0");
        assert_eq!(format!("{}", ValueNode::Float(3.142)), "3.142");
        assert_eq!(format!("{}", ValueNode::Float(-3.142)), "-3.142");
        assert_eq!(
            format!("{}", ValueNode::String(StringValue::literal(""))),
            "\"\""
        );
        assert_eq!(
            format!("{}", ValueNode::String(StringValue::literal("foo"))),
            "\"foo\""
        );
        assert_eq!(
            format!("{}", ValueNode::String(StringValue::new(String::from("")))),
            "\"\""
        );
        assert_eq!(
            format!(
                "{}",
                ValueNode::String(StringValue::new(String::from("foo")))
            ),
            "\"foo\""
        );
    }
    #[test]
    fn is_null_expressions() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("(null? null)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );

        let expression = parser::parse("(null? #t)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(null? #f)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(null? 0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(null? -0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(null? 3)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(null? -3)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(null? 0.0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(null? -0.0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(null? 3.142)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(null? -3.142)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(null? \"\")").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(null? \"foo\")").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
    }

    #[test]
    fn is_boolean_expressions() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("(boolean? #t)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );
        let expression = parser::parse("(boolean? #f)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );

        let expression = parser::parse("(boolean? null)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(boolean? 0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(boolean? -0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(boolean? 3)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(boolean? -3)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(boolean? 0.0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(boolean? -0.0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(boolean? 3.142)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(boolean? -3.142)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(boolean? \"\")").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(boolean? \"foo\")").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
    }

    #[test]
    fn is_integer_expressions() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("(integer? 0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );
        let expression = parser::parse("(integer? -0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );
        let expression = parser::parse("(integer? 3)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );
        let expression = parser::parse("(integer? -3)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );

        let expression = parser::parse("(integer? null)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(integer? #t)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(integer? #f)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(integer? 0.0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(integer? -0.0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(integer? 3.142)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(integer? -3.142)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(integer? \"\")").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(integer? \"foo\")").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
    }

    #[test]
    fn is_float_expressions() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("(float? 0.0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );
        let expression = parser::parse("(float? -0.0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );
        let expression = parser::parse("(float? 3.142)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );
        let expression = parser::parse("(float? -3.142)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );

        let expression = parser::parse("(float? null)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(float? #t)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(float? #f)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(float? 0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(float? -0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(float? 3)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(float? -3)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(float? \"\")").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(float? \"foo\")").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
    }

    #[test]
    fn is_string_expressions() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("(string? \"\")").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );
        let expression = parser::parse("(string? \"foo\")").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );

        let expression = parser::parse("(string? null)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(string? #t)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(string? #f)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(string? 0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(string? -0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(string? 3)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(string? -3)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(string? 0.0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(string? -0.0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(string? 3.142)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(string? -3.142)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
    }
}
