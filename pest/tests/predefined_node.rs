// pest-typed. A statically typed version of pest.
// Copyright (c) 2023 黄博奕
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

extern crate alloc;

#[cfg(test)]
mod tests {
    use pest_typed::{
        atomic_rule,
        choices::Choice2,
        compound_atomic_rule,
        iterators::{Pair, PairTree, Pairs},
        non_atomic_rule, normal_rule,
        predefined_node::*,
        rule_eoi,
        sequence::{Seq2, Seq3},
        silent_rule, BoundWrapper, ParsableTypedNode, RuleStruct, RuleWrapper, Storage,
        StringArrayWrapper, StringWrapper, TypeWrapper,
    };
    use std::{fmt::Write, ops::Deref, string::String};

    #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    enum Rule {
        Foo,
        RepFoo,
        NotFooBar,
        Life,
        WHITESPACE,
        COMMENT,
        EOI,
        String,
        Quote,
    }

    #[derive(Clone, Hash, PartialEq, Eq)]
    pub struct Foo;
    impl StringWrapper for Foo {
        const CONTENT: &'static str = "foo";
    }
    impl RuleWrapper<Rule> for Foo {
        const RULE: Rule = Rule::Foo;
        type Rule = Rule;
    }

    atomic_rule!(
        WHITESPACE,
        "White space.",
        Rule,
        Rule::WHITESPACE,
        CharRange::<' ', ' '>
    );
    compound_atomic_rule!(
        COMMENT,
        "Comment.",
        Rule,
        Rule::COMMENT,
        CharRange::<'\t', '\t'>,
        false
    );
    normal_rule!(
        StrFoo,
        "String \"Foo\".",
        Rule,
        Rule::Foo,
        Str::<Foo>,
        AtomicRep<Choice2<WHITESPACE<'i>, COMMENT<'i>>>,
        false
    );
    rule_eoi!(EOI, Rule);

    #[test]
    fn string() {
        assert_eq!(<StrFoo<'_, 0> as TypeWrapper>::Inner::CONTENT, Foo::CONTENT);
        let s = StrFoo::try_parse("foo").unwrap();
        assert_eq!(s.content.get_content(), "foo");
        assert_eq!(
            format!("{:?}", s),
            "StrFoo { content: Str, span: Span { str: \"foo\", start: 0, end: 3 } }"
        )
    }

    #[test]
    fn range() {
        let whitespace = WHITESPACE::try_parse(" ").unwrap();
        assert_eq!(
            format!("{:?}", whitespace),
            "WHITESPACE { span: Span { str: \" \", start: 0, end: 1 } }"
        );
        let comment = COMMENT::try_parse("\t").unwrap();
        assert_eq!(
            format!("{:?}", comment),
            "COMMENT { content: CharRange { content: '\\t' }, span: Span { str: \"\\t\", start: 0, end: 1 } }"
        );
    }
    type Ignore<'i> = AtomicRep<Choice2<WHITESPACE<'i>, COMMENT<'i>>>;

    #[test]
    fn ignore() {
        silent_rule!(
            tmp,
            "Temporary rule.",
            Rule,
            Rule::RepFoo,
            Ignore<'i>,
            Empty<'i>,
            false
        );
        tmp::try_parse(" \t  ").unwrap();
    }

    #[test]
    fn repetition() {
        type REP<'i> = Rep<StrFoo<'i, 0>, Ignore<'i>, 1>;
        non_atomic_rule!(
            R,
            "Repetion of [StrFoo].",
            Rule,
            Rule::RepFoo,
            REP<'i>,
            Ignore<'i>,
            false
        );

        let rep1 = R::try_parse("foofoofoo").unwrap();
        let rep2 = R::try_parse("foo foo foo").unwrap();
        let rep3 = R::try_parse("foo foo\tfoo").unwrap();
        let _ = R::try_parse("").unwrap();
        assert_ne!(rep1, rep2);
        assert_ne!(rep1, rep3);

        let format = |rep: &R<'_, 1>| -> String {
            rep.iter_matched()
                .map(|e| e.get_content())
                .collect::<Vec<_>>()
                .concat()
        };
        assert_eq!(format(&rep1), format(&rep2));
        assert_eq!(format(&rep1), format(&rep3));

        for i in rep1.clone().into_inner() {
            assert_eq!(i.rule(), Rule::Foo);
        }

        // White spaces and comments aren't filtered out.
        assert_ne!(
            rep1.iter_matched().collect::<Vec<_>>(),
            rep2.iter_matched().collect::<Vec<_>>()
        );
        assert_ne!(
            rep1.iter_matched().collect::<Vec<_>>(),
            rep3.iter_matched().collect::<Vec<_>>()
        );

        assert_eq!(REP::MIN, 0);
        assert_eq!(rep1.deref().get_min_len(), 0);
        assert_eq!(rep1.deref().get_max_len(), usize::MAX);
        assert_eq!(<R<'_, 0> as TypeWrapper>::Inner::MIN, 0);
    }

    #[test]
    fn repetition_at_least_once() {
        type REP<'i> = RepOnce<Insens<'i, Foo>, Ignore<'i>, 1>;
        non_atomic_rule!(
            R,
            "Repetion of [StrFoo].",
            Rule,
            Rule::RepFoo,
            REP<'i>,
            Ignore<'i>,
            false
        );

        let rep1 = R::try_parse("fooFoofoo").unwrap();
        let rep2 = R::try_parse("foo Foo foo").unwrap();
        let rep3 = R::try_parse("Foo foo\tfoo").unwrap();
        let rep4 = R::try_parse("Foofoofoo").unwrap();
        assert_ne!(rep1, rep2);
        assert_ne!(rep1, rep3);
        assert_ne!(rep1, rep4);

        let collect = |r: &R<'_, 1>| {
            r.iter_matched()
                .map(|r| r.get_content())
                .collect::<Vec<_>>()
                .concat()
        };
        assert_eq!(collect(&rep1), collect(&rep2));
        assert_eq!(collect(&rep1), collect(&rep3));
        assert_eq!(collect(&rep1), collect(&rep4));

        assert_eq!(rep1.clone().into_iter_pairs().count(), 1);
        assert_eq!(
            rep1.clone().into_iter_pairs().next().unwrap().rule(),
            Rule::RepFoo
        );

        {
            let mut buf = String::new();
            rep3.iterate_level_order(|p, _depth, _queue| writeln!(buf, "{}", p.span().as_str()))
                .unwrap();
            assert_eq!(buf, "Foo foo\tfoo\n \n\t\n");
            assert_eq!(
                rep3.format_as_tree().unwrap(),
                "RepFoo
    WHITESPACE \" \"
    COMMENT \"\\t\"
"
            );
        }

        assert_eq!(REP::MIN, 1);
        assert_eq!(rep1.deref().get_min_len(), 1);
        assert_eq!(rep1.deref().get_max_len(), usize::MAX);
        assert_eq!(<R<'_, 0> as TypeWrapper>::Inner::MIN, 1);
    }

    #[test]
    fn skip() {
        #[derive(Clone, Debug, Hash, PartialEq, Eq)]
        pub struct NewLine;
        impl StringArrayWrapper for NewLine {
            const CONTENT: &'static [&'static str] = &["\r\n", "\n", "\r"];
        }
        compound_atomic_rule!(
            QuotedString,
            "Quoted string.",
            Rule,
            Rule::String,
            Seq2<Skipped<Skip<'i, NewLine>, Ignore<'i>, 0>, Skipped<NEWLINE, Ignore<'i>, 0>>,
            false
        );

        let s1 = QuotedString::<1>::try_parse("2\r\n").unwrap();
        let s2 = QuotedString::<1>::try_parse("\r\n").unwrap();
        assert_ne!(s1, s2);

        let new_line = NewLine;
        assert_eq!(new_line.get_content(), NewLine::CONTENT);
    }

    #[test]
    fn skip_char() {
        compound_atomic_rule!(
            three,
            "Skip 3 characters.",
            Rule,
            Rule::Foo,
            SkipChar<'i, 3>,
            false
        );
        three::<1>::try_parse("foo").unwrap();
        three::<1>::try_parse("foobar").unwrap_err();
    }

    #[test]
    fn positive_predicate() {
        atomic_rule!(Quote,"A single `'`.", Rule,Rule::Quote,CharRange<'\'', '\''>);
        compound_atomic_rule!(
            Lifetime,
            "A simplified example of rust lifetime specifier.",
            Rule,
            Rule::Life,
            Seq3<
                Skipped<Quote<'i, 0>, Ignore<'i>, 0>,
                Skipped<
                    Positive<
                        Seq2<
                            Skipped<ANY, Ignore<'i>, 0>,
                            Skipped<Negative<Quote<'i, 0>>, Ignore<'i>, 0>,
                        >,
                    >,
                    Ignore<'i>,
                    0,
                >,
                Skipped<AtomicRep<CharRange<'a', 'z'>>, Ignore<'i>, 0>,
            >,
            false
        );

        let l = Lifetime::<1>::try_parse("'i").unwrap();
        let (quote, peeked, name) = l.as_ref();
        assert_eq!(quote.span.as_str(), "'");
        let (any, _) = peeked.as_ref();
        assert_eq!(any.content, 'i');
        assert_eq!(name.iter().map(|c| c.content).collect::<String>(), "i");
        assert_eq!(
            name.deref()
                .into_iter()
                .map(|c| c.content)
                .collect::<String>(),
            "i"
        );

        let l = Lifetime::<1>::try_parse("'input").unwrap();
        let (_, peeked, name) = l.as_ref();
        let (any, _) = peeked.as_ref();
        assert_eq!(any.content, 'i');
        assert_eq!(name.iter().map(|c| c.content).collect::<String>(), "input");
        assert_eq!(
            name.deref()
                .clone()
                .into_iter()
                .map(|c| c.content)
                .collect::<String>(),
            "input"
        );

        Lifetime::<1>::try_parse("'i'").unwrap_err();
    }

    #[test]
    fn negative_predicate() {
        #[derive(Clone, Debug, Hash, PartialEq, Eq)]
        pub struct StrFoo;
        impl StringWrapper for StrFoo {
            const CONTENT: &'static str = "foo";
        }
        #[derive(Clone, Debug, Hash, PartialEq, Eq)]
        pub struct StrBar;
        impl StringWrapper for StrBar {
            const CONTENT: &'static str = "bar";
        }
        compound_atomic_rule!(
            not_foo_bar,
            "Any character except \"foo\" or \"bar\".",
            Rule,
            Rule::NotFooBar,
            AtomicRep<(Negative<Choice2<Str<StrFoo>, Str<StrBar>>>, ANY)>,
            false
        );
        let _ = not_foo_bar::<1>::try_parse("").unwrap();
        let baz = not_foo_bar::<1>::try_parse("baz").unwrap();
        for i in baz.iter() {
            let (neg, any) = i;
            assert_eq!(
                <Negative<_> as Pairs<'_, '_, Rule>>::into_iter_pairs(neg.clone()).count(),
                0
            );
            assert_eq!(
                <ANY as Pairs<'_, '_, Rule>>::into_iter_pairs(any.clone()).count(),
                0
            );
        }
        let _ = not_foo_bar::<1>::try_parse("Foofoo").unwrap_err();
        let _ = not_foo_bar::<1>::try_parse("bazfoo").unwrap_err();
    }

    #[test]
    fn peek() {
        compound_atomic_rule!(
            Rep_1_3,
            "Repeat previously matched expression 1 to 3 times",
            Rule,
            Rule::RepFoo,
            Seq2<
                Skipped<Push<Insens<'i, Foo>>, Ignore<'i>, 0>,
                Skipped<RepMinMax<Skipped<PEEK<'i>, Ignore<'i>, 0>, 1, 3>, Ignore<'i>, 0>,
            >,
            false
        );
        let r = Rep_1_3::<1>::try_parse("foOfoO").unwrap();
        assert_eq!(
            format!("{:#?}", r),
            "Rep_1_3 {
    content: Seq2(
        Push {
            content: Insens {
                content: \"foO\",
            },
        },
        RepMinMax {
            content: [
                PEEK {
                    span: Span {
                        str: \"foO\",
                        start: 3,
                        end: 6,
                    },
                },
            ],
        },
    ),
    span: Span {
        str: \"foOfoO\",
        start: 0,
        end: 6,
    },
}"
        );
        let (head, following) = r.as_ref();
        assert_eq!(head.deref().content, "foO");
        assert_eq!(head.get_content(), Foo::CONTENT);
        for i in following.iter_matched() {
            assert_eq!(i.span.as_str(), head.deref().content);
        }

        Rep_1_3::try_parse("fooFoo").unwrap_err();
        Rep_1_3::try_parse("Foo").unwrap_err();
        Rep_1_3::try_parse("FooFooFooFooFoo").unwrap_err();
        let (pos, _) = Rep_1_3::try_parse_partial("FooFooFooFooFoo").unwrap();
        assert_eq!(pos.pos(), 12);

        {
            use std::collections::HashSet;
            let mut set = HashSet::new();
            for input in [
                "foofoo",
                "FooFoo",
                "fOofOo",
                "foOfoO",
                "foofoo",
                "foofoofoo",
            ] {
                set.insert(Rep_1_3::try_parse(input).unwrap());
            }
            assert_eq!(set.len(), 5);
        }
    }

    #[test]
    fn rep() {
        compound_atomic_rule!(
            Rep_0_3,
            "Repeat previously matched expression 0 to 3 times",
            Rule,
            Rule::RepFoo,
            RepMin<Skipped<Str<Foo>, Ignore<'i>, 0>, 0>,
            false
        );

        let x = Rep_0_3::try_parse("").unwrap();
        assert_eq!(x.ref_inner(), &RepMin::default());

        use std::collections::HashSet;

        let mut set = HashSet::new();
        let foos = ["foofoo", "foo", "foofoofoo"];
        for input in foos {
            let r = Rep_0_3::try_parse(input).unwrap();
            for s in r.iter_matched() {
                set.insert(s.clone());
            }
            for s in r.deref().clone().into_iter_matched() {
                set.insert(s);
            }
        }
        assert_eq!(set.len(), 1);

        let mut set = HashSet::new();
        for input in ["foo", "foofoofoo", "foofoo", "foofoofoo"] {
            set.insert(Rep_0_3::try_parse(input).unwrap());
        }
        assert_eq!(set.len(), 3);

        compound_atomic_rule!(
            Rep_2,
            "Repeat \"foo\" 2 times",
            Rule,
            Rule::RepFoo,
            [Insens<'i, Foo>; 2],
            false
        );
        Rep_2::try_parse("").unwrap_err();
        Rep_2::try_parse("foo").unwrap_err();
        Rep_2::try_parse("foofoofoo").unwrap_err();
        let arr = Rep_2::try_parse("foofoo").unwrap();
        assert_eq!(
            format!("{:?}", arr),
            "Rep_2 { content: [Insens { content: \"foo\" }, Insens { content: \"foo\" }], span: Span { str: \"foofoo\", start: 0, end: 6 } }"
        );
        let mut set = HashSet::new();
        for input in ["foofoo", "fooFOO", "FooFoo", "foofoo"] {
            set.insert(Rep_2::try_parse(input).unwrap());
        }
        assert_eq!(set.len(), 3);
    }
}
