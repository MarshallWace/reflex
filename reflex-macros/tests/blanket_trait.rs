// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use reflex_macros::blanket_trait;
use std::marker::PhantomData;

mod fixtures {
    pub(crate) trait Bar {}

    pub(crate) trait Baz {}

    pub(crate) trait Gen<T> {}
}

#[test]
fn implements_bar_baz_suffices() {
    use fixtures::{Bar, Baz};

    blanket_trait!(
        trait Foo: Bar + Baz {}
    );
    fn assert_foo<T: Foo>() {}

    struct Stuff {}
    impl Bar for Stuff {}
    impl Baz for Stuff {}

    assert_foo::<Stuff>()
}

#[test]
fn implements_bar_gen() {
    use fixtures::{Bar, Gen};

    blanket_trait!(
        trait Foo<T>: Bar + Gen<T> {}
    );
    // trait Foo<T>: Bar + Gen<T> {}
    // impl<_Self, T> Foo<T> for _Self where _Self: Bar + Gen<T> {}
    fn assert_foo<S, T: Foo<S>>() {}

    struct Stuff<T> {
        _phantom: PhantomData<T>,
    }
    impl<T> Bar for Stuff<T> {}
    impl<T> Gen<T> for Stuff<T> {}

    assert_foo::<i32, Stuff<i32>>()
}

#[test]
fn implements_bar_gen_with_where_claus_and_bounds() {
    use fixtures::{Bar, Baz, Gen};

    blanket_trait!(
        trait Foo<T: Baz>: Bar + Gen<T>
        where
            T: Bar,
        {
        }
    );
    // trait Foo<T: Baz>: Bar + Gen<T> where T: Bar {}
    // impl<_Self, T: Baz> Foo<T> for _Self where _Self: Bar + Gen<T>, T: Bar {}
    fn assert_foo<S: Bar + Baz, T: Foo<S>>() {}

    struct Stuff<T> {
        _phantom: PhantomData<T>,
    }
    impl<T> Bar for Stuff<T> {}
    impl<T> Gen<T> for Stuff<T> {}
    impl Baz for i32 {}
    impl Bar for i32 {}

    assert_foo::<i32, Stuff<i32>>()
}
