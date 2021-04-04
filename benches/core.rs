// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
#![feature(test)]
extern crate test;

use test::Bencher;

use reflex::{
    env::Env,
    expression::{Expression, RuntimeState},
    node::{
        arithmetic::{AddNode, ArithmeticNode},
        core::{CoreNode, FunctionApplicationNode, FunctionNode, ReferenceNode, ValueNode},
        parser, Node,
    },
};

#[bench]
fn nested_expressions(b: &mut Bencher) {
    let env = Env::new();
    let state = RuntimeState::new();
    let expression = parser::parse("(+ (+ (abs -3) 4) 5)").unwrap();
    b.iter(|| expression.evaluate(&env, &state));
}

#[bench]
fn function_application_nullary(b: &mut Bencher) {
    let env = Env::new();
    let state = RuntimeState::new();
    let expression = parser::parse("((lambda () 3))").unwrap();
    b.iter(|| expression.evaluate(&env, &state));
}

#[bench]
fn function_application_unary(b: &mut Bencher) {
    let env = Env::new();
    let state = RuntimeState::new();
    let expression = parser::parse("((lambda (foo) foo) 3)").unwrap();
    b.iter(|| expression.evaluate(&env, &state));
}

#[bench]
fn function_application_binary(b: &mut Bencher) {
    let env = Env::new();
    let state = RuntimeState::new();
    let expression = parser::parse("((lambda (foo bar) foo) 3 4)").unwrap();
    b.iter(|| expression.evaluate(&env, &state));
}

#[bench]
fn function_application_ternary(b: &mut Bencher) {
    let env = Env::new();
    let state = RuntimeState::new();
    let expression = parser::parse("((lambda (foo bar baz) foo) 3 4 5)").unwrap();
    b.iter(|| expression.evaluate(&env, &state));
}

#[bench]
fn function_application_unused_args(b: &mut Bencher) {
    let env = Env::new();
    let state = RuntimeState::new();
    let expression = parser::parse("((lambda (foo bar baz) 2) 3 4 5)").unwrap();
    b.iter(|| expression.evaluate(&env, &state));
}

#[bench]
fn function_application_argument_scope(b: &mut Bencher) {
    let env = Env::new();
    let state = RuntimeState::new();
    let expression = parser::parse(
            "((lambda (first second third) ((lambda (one two) ((lambda (foo bar) (+ foo bar)) one two)) first third)) 3 4 5)",
        )
        .unwrap();
    b.iter(|| expression.evaluate(&env, &state));
}

#[bench]
fn deeply_nested_function_application(b: &mut Bencher) {
    let env = Env::new();
    let state = RuntimeState::new();
    let expression = (1..=100).fold(
        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(0)))),
        |acc, i| {
            Expression::new(Node::Core(CoreNode::FunctionApplication(
                FunctionApplicationNode::new(
                    Expression::new(Node::Core(CoreNode::Function(FunctionNode::new(
                        1,
                        Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                            Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                            acc,
                        )))),
                    )))),
                    vec![Expression::new(Node::Core(CoreNode::Value(
                        ValueNode::Int(i),
                    )))],
                ),
            )))
        },
    );
    b.iter(|| expression.evaluate(&env, &state))
}

#[bench]
fn function_application_closure(b: &mut Bencher) {
    let env = Env::new();
    let state = RuntimeState::new();
    let expression = parser::parse("(((lambda (foo) (lambda () foo)) 3))").unwrap();
    b.iter(|| expression.evaluate(&env, &state));
}

#[bench]
fn conditional_expressions(b: &mut Bencher) {
    let env = Env::new();
    let state = RuntimeState::new();
    let expression = parser::parse("(if #t 3 4)").unwrap();
    b.iter(|| expression.evaluate(&env, &state));
}
