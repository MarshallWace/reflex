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
    {
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

    {
        #[derive(PartialEq, Eq, Debug)]
        pub struct GenericMessage<'b, T: From<u32>>(&'b T)
        where
            T: From<u64> + Default + 'b;

        #[derive(Matcher, PartialEq, Eq, Debug)]
        pub enum MyMatcher<'a, T: From<u64>>
        where
            T: From<u32> + Default + 'a,
        {
            Foo(GenericMessage<'a, T>),
        }
        let value: &u64 = &0;
        let outer: MyMatcher<_> = GenericMessage(value).into();
        assert_eq!(outer, MyMatcher::Foo(GenericMessage(value)));
        let inner: Option<&GenericMessage<_>> = (&outer).into();
        assert_eq!(inner, Some(&GenericMessage(value)));
        let inner: Option<GenericMessage<_>> = outer.into();
        assert_eq!(inner, Some(GenericMessage(value)));
    }
}

#[test]
fn ref_matcher() {
    {
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

    {
        #[derive(PartialEq, Eq, Debug)]
        pub struct GenericMessage<'b, T: From<u32>>(&'b T)
        where
            T: From<u64> + Default + 'b;

        #[derive(Matcher, PartialEq, Eq, Debug)]
        enum Outer<'a, T: From<u64>>
        where
            T: From<u32> + Default + 'a,
        {
            Foo(GenericMessage<'a, T>),
            Bar(bool),
        }

        #[derive(Matcher, PartialEq, Eq, Debug)]
        #[matcher(as_ref(FilteredRef, PartialEq, Eq, Debug))]
        pub enum Filtered<'a, T: From<u64>>
        where
            T: From<u32> + Default + 'a,
        {
            Foo(GenericMessage<'a, T>),
        }
        {
            let outer = Outer::<u64>::Bar(true);
            let matched = FilteredRef::match_from(&outer);
            assert_eq!(matched, None);
        }
        {
            let value: &u64 = &0;
            let outer = Outer::<u64>::Foo(GenericMessage(value));
            let matched = FilteredRef::<u64>::match_from(&outer);
            assert_eq!(matched, Some(FilteredRef::Foo(&GenericMessage(value))));
        }
    }
}
