// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{fmt, rc::Rc};

pub mod operations;
pub use self::operations::bind::Bind;
pub use self::operations::evaluate::Evaluate;

pub mod function;
pub use self::function::Closure;
pub use self::function::Function;

pub mod node;
pub use self::node::Node;

pub mod object;
pub use self::object::Object;

pub mod value;
pub use self::value::StringValue;
pub use self::value::Value;

use crate::env::Env;

#[derive(PartialEq, Clone)]
pub enum Expression {
    Pending,
    Error(String),
    Value(Value),
    Reference(StackOffset),
    Bound(Env, Rc<Expression>),
    Function(Function),
    Closure(Closure),
    Object(Object),
    Node(Node),
}

pub type StackOffset = usize;

impl Bind for Rc<Expression> {
    fn bind(&self, env: &Env) -> Rc<Expression> {
        match &**self {
            Expression::Reference(_) | Expression::Node(_) => {
                Rc::new(Expression::Bound(env.clone(), Rc::clone(self)))
            }
            Expression::Function(Function {
                arity,
                captures: Some(_),
                body,
            }) => Rc::new(Expression::Closure(Closure {
                env: env.clone(),
                function: Function {
                    arity: *arity,
                    captures: None,
                    body: Rc::clone(body),
                },
            })),
            _ => Rc::clone(self),
        }
    }
}
impl Evaluate for Rc<Expression> {
    fn evaluate(&self, env: &Env) -> Rc<Expression> {
        match &**self {
            Expression::Reference(index) => env.get(*index).evaluate(env),
            Expression::Bound(env, value) => value.evaluate(env),
            Expression::Node(node) => node.evaluate(env),
            Expression::Function(_) => self.bind(env),
            _ => Rc::clone(self),
        }
    }
}
impl fmt::Debug for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Pending => write!(f, "{}", "Pending"),
            Expression::Error(message) => write!(f, "Error({:?})", message),
            Expression::Value(value) => write!(f, "{:?}", value),
            Expression::Reference(node) => write!(f, "Reference({})", node),
            Expression::Bound(_, value) => write!(f, "Bound({:?})", value),
            Expression::Function(value) => write!(f, "{:?}", value),
            Expression::Closure(value) => write!(f, "{:?}", value),
            Expression::Object(value) => write!(f, "{:?}", value),
            Expression::Node(value) => write!(f, "{:?}", value),
        }
    }
}
impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Pending => write!(f, "{}", "<pending>"),
            Expression::Error(message) => write!(f, "Error: {}", message),
            Expression::Value(value) => write!(f, "{}", value),
            Expression::Reference(value) => write!(f, "<env:{}>", value),
            Expression::Bound(_, value) => write!(f, "<bound:{}>", value),
            Expression::Function(value) => write!(f, "{}", value),
            Expression::Closure(value) => write!(f, "{}", value),
            Expression::Object(value) => write!(f, "{}", value),
            Expression::Node(value) => write!(f, "{:?}", value),
        }
    }
}
