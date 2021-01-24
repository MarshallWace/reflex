// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::rc::Rc;

use crate::{
    env::Env,
    expression::{Evaluate, Expression},
};

pub type NodeFactoryResult = Result<Node, String>;

pub trait CompoundNode {
    fn factory(args: Vec<Rc<Expression>>) -> NodeFactoryResult;
    fn expressions(&self) -> Vec<&Rc<Expression>>;
}

pub mod abs;
pub mod add;
pub mod and;
pub mod apply;
pub mod ceil;
pub mod divide;
pub mod floor;
pub mod get;
pub mod _if;
pub mod multiply;
pub mod not;
pub mod or;
pub mod remainder;
pub mod subtract;

pub use abs::AbsNode;
pub use add::AddNode;
pub use and::AndNode;
pub use apply::ApplyNode;
pub use ceil::CeilNode;
pub use divide::DivideNode;
pub use floor::FloorNode;
pub use get::GetNode;
pub use _if::IfNode;
pub use multiply::MultiplyNode;
pub use not::NotNode;
pub use or::OrNode;
pub use remainder::RemainderNode;
pub use subtract::SubtractNode;

const ABS: &str = AbsNode::name();
const ADD: &str = AddNode::name();
const AND: &str = AndNode::name();
const APPLY: &str = ApplyNode::name();
const CEIL: &str = CeilNode::name();
const DIVIDE: &str = DivideNode::name();
const FLOOR: &str = FloorNode::name();
const GET: &str = GetNode::name();
const IF: &str = IfNode::name();
const MULTIPLY: &str = MultiplyNode::name();
const NOT: &str = NotNode::name();
const OR: &str = OrNode::name();
const REMAINDER: &str = RemainderNode::name();
const SUBTRACT: &str = SubtractNode::name();

#[derive(Debug, PartialEq, Clone)]
pub enum Node {
    Abs(AbsNode),
    Add(AddNode),
    And(AndNode),
    Apply(ApplyNode),
    Ceil(CeilNode),
    Divide(DivideNode),
    Floor(FloorNode),
    Get(GetNode),
    If(IfNode),
    Multiply(MultiplyNode),
    Not(NotNode),
    Or(OrNode),
    Remainder(RemainderNode),
    Subtract(SubtractNode),
}
impl Node {
    pub fn factory(type_name: &str, args: Vec<Rc<Expression>>) -> Result<Option<Node>, String> {
        match type_name {
            ABS => AbsNode::factory(args).map(Some),
            ADD => AddNode::factory(args).map(Some),
            AND => AndNode::factory(args).map(Some),
            APPLY => ApplyNode::factory(args).map(Some),
            CEIL => CeilNode::factory(args).map(Some),
            DIVIDE => DivideNode::factory(args).map(Some),
            FLOOR => FloorNode::factory(args).map(Some),
            GET => GetNode::factory(args).map(Some),
            IF => IfNode::factory(args).map(Some),
            MULTIPLY => MultiplyNode::factory(args).map(Some),
            NOT => NotNode::factory(args).map(Some),
            OR => OrNode::factory(args).map(Some),
            REMAINDER => RemainderNode::factory(args).map(Some),
            SUBTRACT => SubtractNode::factory(args).map(Some),
            _ => Ok(None),
        }
    }
    pub fn children(&self) -> Option<Vec<&Rc<Expression>>> {
        match self {
            Node::Abs(node) => Some(CompoundNode::expressions(node)),
            Node::Add(node) => Some(CompoundNode::expressions(node)),
            Node::And(node) => Some(CompoundNode::expressions(node)),
            Node::Apply(node) => Some(CompoundNode::expressions(node)),
            Node::Ceil(node) => Some(CompoundNode::expressions(node)),
            Node::Divide(node) => Some(CompoundNode::expressions(node)),
            Node::Floor(node) => Some(CompoundNode::expressions(node)),
            Node::Get(node) => Some(CompoundNode::expressions(node)),
            Node::If(node) => Some(CompoundNode::expressions(node)),
            Node::Multiply(node) => Some(CompoundNode::expressions(node)),
            Node::Not(node) => Some(CompoundNode::expressions(node)),
            Node::Or(node) => Some(CompoundNode::expressions(node)),
            Node::Remainder(node) => Some(CompoundNode::expressions(node)),
            Node::Subtract(node) => Some(CompoundNode::expressions(node)),
        }
    }
}
impl Evaluate for Node {
    fn evaluate(&self, env: &Env) -> Rc<Expression> {
        match self {
            Node::Abs(node) => Evaluate::evaluate(node, env),
            Node::Add(node) => Evaluate::evaluate(node, env),
            Node::And(node) => Evaluate::evaluate(node, env),
            Node::Apply(node) => Evaluate::evaluate(node, env),
            Node::Ceil(node) => Evaluate::evaluate(node, env),
            Node::Divide(node) => Evaluate::evaluate(node, env),
            Node::Floor(node) => Evaluate::evaluate(node, env),
            Node::Get(node) => Evaluate::evaluate(node, env),
            Node::If(node) => Evaluate::evaluate(node, env),
            Node::Multiply(node) => Evaluate::evaluate(node, env),
            Node::Not(node) => Evaluate::evaluate(node, env),
            Node::Or(node) => Evaluate::evaluate(node, env),
            Node::Remainder(node) => Evaluate::evaluate(node, env),
            Node::Subtract(node) => Evaluate::evaluate(node, env),
        }
    }
}
