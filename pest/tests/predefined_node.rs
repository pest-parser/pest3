//! Copied from [pest-typed/tests/predefined_node.rs] and modified.
//!
//! [pest-typed/tests/predefined_node.rs]: https://github.com/TheVeryDarkness/pest-typed/blob/0.12.1/main/tests/predefined_node.rs

extern crate alloc;

#[cfg(test)]
mod tests {
    use pest::{
        choice::Choice2,
        typed::{
            template::*,
            wrapper::{Rule as RuleWrapper, String as StringWrapper},
            RuleType, Tracker, TypedNode, TypedParser,
        },
        unicode, Position, Span, Stack,
    };

    #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    pub enum Rule {
        Foo,
        EOI,
    }

    impl RuleType for Rule {
        const EOI: Self = Self::EOI;
        type OptionalTrivia<'i> = Rep<Choice2<Char<' '>, Char<'\t'>>, 0>;
        type MandatoryTrivia<'i> = Empty;
    }

    unicode!(MATH);

    struct Parser;
    impl TypedParser<Rule> for Parser {}

    #[derive(Clone, Debug, Hash, PartialEq, Eq)]
    pub struct Foo;
    impl StringWrapper for Foo {
        const CONTENT: &'static str = "foo";
    }

    #[derive(Debug, PartialEq, Eq)]
    pub struct StrFoo {
        content: Str<Foo>,
    }
    impl RuleWrapper for StrFoo {
        type Rule = Rule;
        const RULE: Rule = Rule::Foo;
    }
    impl<'i> TypedNode<'i, Rule> for StrFoo {
        fn try_parse_with_partial(
            input: Position<'i>,
            stack: &mut Stack<Span<'i>>,
            tracker: &mut Tracker<'i, Rule>,
        ) -> Option<(Position<'i>, Self)> {
            let (pos, content) = Str::<Foo>::try_parse_with_partial(input, stack, tracker)?;
            Some((pos, Self { content }))
        }
        fn check_with_partial(
            input: Position<'i>,
            stack: &mut Stack<Span<'i>>,
            tracker: &mut Tracker<'i, Rule>,
        ) -> Option<Position<'i>> {
            let pos = Str::<Foo>::check_with_partial(input, stack, tracker)?;
            Some(pos)
        }
    }

    #[test]
    fn string() {
        let s = StrFoo::try_parse("foo").unwrap();
        assert_eq!(format!("{:?}", s), r#"StrFoo { content: Str("foo") }"#)
    }

    #[test]
    fn ignore() {
        type Trivia<'i> = <Rule as RuleType>::OptionalTrivia<'i>;
        let trivia = <Trivia as TypedNode<'static, Rule>>::try_parse(" \t  \t\t").unwrap();
        assert_eq!(
            format!("{:?}", trivia),
            "RepMinMax([Choice0(Char(' ')), Choice1(Char('\\t')), Choice0(Char(' ')), Choice0(Char(' ')), Choice1(Char('\\t')), Choice1(Char('\\t'))])"
        );
    }

    #[test]
    fn repetition() {
        type R<'i> = Rep<StrFoo, 1>;

        let rep1 = Parser::try_parse::<R>("foofoofoo").unwrap();
        let rep2 = Parser::try_parse::<R>("foo foo foo").unwrap();
        let rep3 = Parser::try_parse::<R>("foo foo\tfoo").unwrap();
        let _ = R::try_parse("").unwrap();
        assert_eq!(rep1, rep2);
        assert_eq!(rep1, rep3);
    }

    #[test]
    fn repetition_at_least_once() {
        type R<'i> = RepOnce<Insens<'i, Foo>, 1>;

        let rep1 = Parser::try_parse::<R>("fooFoofoo").unwrap();
        let rep2 = Parser::try_parse::<R>("foo Foo foo").unwrap();
        let rep3 = Parser::try_parse::<R>("Foo foo\tfoo").unwrap();
        let rep4 = Parser::try_parse::<R>("Foofoofoo").unwrap();
        assert_eq!(rep1, rep2);
        assert_ne!(rep1, rep3);
        assert_eq!(rep3, rep4);
    }
}
