// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex_dispatcher::Named;
use reflex_macros::Named;

#[test]
fn basic_usage() {
    #[derive(Named)]
    struct Foo;
    assert_eq!(Foo.name(), "Foo")
}

#[test]
fn generics() {
    #[derive(Named)]
    struct Foo<T>(T);
    assert_eq!(Foo(true).name(), "Foo")
}

#[test]
fn bounded_generics() {
    #[derive(Named)]
    struct Foo<T: From<bool>>(T);
    assert_eq!(Foo(true).name(), "Foo")
}

#[test]
fn where_clauses() {
    #[derive(Named)]
    struct Foo<T>(T)
    where
        T: From<bool>;
    assert_eq!(Foo(true).name(), "Foo")
}
