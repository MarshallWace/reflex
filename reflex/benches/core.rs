// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
#![feature(test)]
extern crate test;

use test::Bencher;

use reflex::{
    core::{
        ApplicationTerm, Arity, DynamicState, Expression, LambdaTerm, StaticVariableTerm, Term,
        VariableTerm,
    },
    parser::sexpr::parse,
    stdlib::{
        builtin::BuiltinTerm,
        collection::{vector::VectorTerm, CollectionTerm},
        value::ValueTerm,
    },
};

#[bench]
fn nested_expressions(b: &mut Bencher) {
    let state = DynamicState::new();
    let expression = parse("(+ (+ (abs -3) 4) 5)").unwrap();
    b.iter(|| expression.evaluate(&state));
}

#[bench]
fn function_application_nullary(b: &mut Bencher) {
    let state = DynamicState::new();
    let expression = parse("((lambda () 3))").unwrap();
    b.iter(|| expression.evaluate(&state));
}

#[bench]
fn function_application_unary(b: &mut Bencher) {
    let state = DynamicState::new();
    let expression = parse("((lambda (foo) foo) 3)").unwrap();
    b.iter(|| expression.evaluate(&state));
}

#[bench]
fn function_application_binary(b: &mut Bencher) {
    let state = DynamicState::new();
    let expression = parse("((lambda (foo bar) foo) 3 4)").unwrap();
    b.iter(|| expression.evaluate(&state));
}

#[bench]
fn function_application_ternary(b: &mut Bencher) {
    let state = DynamicState::new();
    let expression = parse("((lambda (foo bar baz) foo) 3 4 5)").unwrap();
    b.iter(|| expression.evaluate(&state));
}

#[bench]
fn function_application_unused_args(b: &mut Bencher) {
    let state = DynamicState::new();
    let expression = parse("((lambda (foo bar baz) 2) 3 4 5)").unwrap();
    b.iter(|| expression.evaluate(&state));
}

#[bench]
fn function_application_argument_scope(b: &mut Bencher) {
    let state = DynamicState::new();
    let expression = parse(
            "((lambda (first second third) ((lambda (one two) ((lambda (foo bar) (+ foo bar)) one two)) first third)) 3 4 5)",
        )
        .unwrap();
    b.iter(|| expression.evaluate(&state));
}

#[bench]
fn deeply_nested_function_application(b: &mut Bencher) {
    let state = DynamicState::new();
    let expression = (1..=100).fold(Expression::new(Term::Value(ValueTerm::Int(0))), |acc, i| {
        Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(1, 0, None),
                Expression::new(Term::Application(ApplicationTerm::new(
                    Expression::new(Term::Builtin(BuiltinTerm::Add)),
                    vec![
                        Expression::new(Term::Variable(VariableTerm::Static(
                            StaticVariableTerm::new(0),
                        ))),
                        acc,
                    ],
                ))),
            ))),
            vec![Expression::new(Term::Value(ValueTerm::Int(i)))],
        )))
    });
    b.iter(|| expression.evaluate(&state))
}

#[bench]
fn function_application_closure(b: &mut Bencher) {
    let state = DynamicState::new();
    let expression = parse("(((lambda (foo) (lambda () foo)) 3))").unwrap();
    b.iter(|| expression.evaluate(&state));
}

#[bench]
fn conditional_expressions(b: &mut Bencher) {
    let state = DynamicState::new();
    let expression = parse("(if #t 3 4)").unwrap();
    b.iter(|| expression.evaluate(&state));
}

#[bench]
fn list_transforms(b: &mut Bencher) {
    let state = DynamicState::new();
    let collection = Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
        (0..1000).map(|index| {
            Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Builtin(BuiltinTerm::Add)),
                vec![
                    Expression::new(Term::Value(ValueTerm::Int(index))),
                    Expression::new(Term::Value(ValueTerm::Int(1))),
                ],
            )))
        }),
    ))));
    let transform = parse("(lambda (value) (+ value 2))").unwrap();
    let expression = Expression::new(Term::Application(ApplicationTerm::new(
        Expression::new(Term::Builtin(BuiltinTerm::Collect)),
        vec![Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Map)),
            vec![collection, transform],
        )))],
    )));
    b.iter(|| expression.evaluate(&state));
}
