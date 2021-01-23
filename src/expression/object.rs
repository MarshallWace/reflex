// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{collections::HashMap, fmt, iter::once, rc::Rc};

use crate::{expression::Expression, value::StringValue};

#[derive(PartialEq, Clone)]
pub struct Object {
    entries: HashMap<String, Rc<Expression>>,
}
impl Object {
    pub fn new(entries: impl IntoIterator<Item = (StringValue, Rc<Expression>)>) -> Object {
        Object {
            entries: entries
                .into_iter()
                .map(|(key, value)| (String::from(key.get()), value))
                .collect::<HashMap<_, _>>(),
        }
    }
    pub fn has(&self, key: &StringValue) -> bool {
        self.entries.contains_key(key.get())
    }
    pub fn get(&self, key: &StringValue) -> Option<Rc<Expression>> {
        self.entries.get(key.get()).map(Rc::clone)
    }
    pub fn set(&self, key: &StringValue, value: Rc<Expression>) -> Object {
        Object {
            entries: self
                .entries
                .iter()
                .map(|(key, value)| (key.clone(), Rc::clone(value)))
                .chain(once((String::from(key.get()), value)))
                .collect(),
        }
    }
}
impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{{ {} }}",
            self.entries
                .iter()
                .map(|(key, value)| format!("{}: {}", key, value))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}
impl fmt::Debug for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{{ {} }}",
            self.entries
                .iter()
                .map(|(key, value)| format!("{}: {:?}", key, value))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::{
        env::Env,
        expression::{Evaluate, Expression},
        node::{add::AddNode, Node},
        parser,
        value::{StringValue, Value},
    };

    use super::Object;

    #[test]
    fn object_literals() {
        let env = Env::new();
        let expression =
            parser::parse("{ first: 1, second: 2, third: 3 }", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result,
            Expression::Object(Object::new(vec![
                (
                    StringValue::new(String::from("first")),
                    Rc::new(Expression::Value(Value::Int(1)))
                ),
                (
                    StringValue::new(String::from("second")),
                    Rc::new(Expression::Value(Value::Int(2)))
                ),
                (
                    StringValue::new(String::from("third")),
                    Rc::new(Expression::Value(Value::Int(3)))
                )
            ]))
        );
        let expression = parser::parse(
            "{ first: (add 1 2), second: (add 3 4), third: (add 5 6) }",
            &Node::factory,
        )
        .unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result,
            Expression::Object(Object::new(vec![
                (
                    StringValue::new(String::from("first")),
                    Rc::new(Expression::Node(Node::Add(AddNode::new(
                        Rc::new(Expression::Value(Value::Int(1))),
                        Rc::new(Expression::Value(Value::Int(2))),
                    )))),
                ),
                (
                    StringValue::new(String::from("second")),
                    Rc::new(Expression::Node(Node::Add(AddNode::new(
                        Rc::new(Expression::Value(Value::Int(3))),
                        Rc::new(Expression::Value(Value::Int(4))),
                    )))),
                ),
                (
                    StringValue::new(String::from("third")),
                    Rc::new(Expression::Node(Node::Add(AddNode::new(
                        Rc::new(Expression::Value(Value::Int(5))),
                        Rc::new(Expression::Value(Value::Int(6))),
                    )))),
                ),
            ])),
        );
    }
}
