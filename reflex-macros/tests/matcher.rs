// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex_macros::Matcher;

mod fixtures {
    #[derive(PartialEq, Eq, Default, Debug)]
    pub(crate) struct FooMessage {
        foo: bool,
    }

    #[derive(PartialEq, Eq, Default, Debug)]
    pub(crate) struct BarMessage {
        bar: bool,
    }

    #[derive(PartialEq, Eq, Default, Debug)]
    pub(crate) struct BazMessage {
        bar: bool,
    }

    #[derive(PartialEq, Eq, Default, Debug)]
    pub(crate) struct QuuxMessage {
        quux: bool,
    }
}

#[test]
fn matcher() {
    use crate::fixtures::*;

    #[derive(Matcher, PartialEq, Eq, Debug)]
    enum Outer {
        Foo(FooMessage),
        Bar(BarMessage),
        Baz(BazMessage),
        Quux(QuuxMessage),
    }

    let outer: Outer = FooMessage::default().into();
    assert_eq!(outer, Outer::Foo(FooMessage::default()));
    let inner: Option<&FooMessage> = (&outer).into();
    assert_eq!(inner, Some(&FooMessage::default()));
    let inner: Option<FooMessage> = outer.into();
    assert_eq!(inner, Some(FooMessage::default()));
}

#[test]
fn ref_matcher() {
    use crate::fixtures::*;

    #[derive(Matcher, PartialEq, Eq, Debug)]
    enum Outer {
        Foo(FooMessage),
        Bar(BarMessage),
        Baz(BazMessage),
        Quux(QuuxMessage),
    }

    #[derive(Matcher, PartialEq, Eq, Debug)]
    #[matcher(as_ref(FilteredRef, PartialEq, Eq, Debug))]
    enum Filtered {
        Bar(BarMessage),
        RenamedQuux(QuuxMessage),
    }

    {
        let outer = Outer::Foo(FooMessage::default());
        let matched = FilteredRef::match_from(&outer);
        assert_eq!(matched, None);
    }
    {
        let outer = Outer::Bar(BarMessage::default());
        let matched = FilteredRef::match_from(&outer);
        assert_eq!(matched, Some(FilteredRef::Bar(&BarMessage::default())));
    }
}
