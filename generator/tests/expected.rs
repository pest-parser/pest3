#[allow(dead_code, non_camel_case_types, clippy::upper_case_acronyms)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Rule {
    EOI,
    r#Regular,
    r#Atomic,
    r#NonAtomic,
    r#ExactString,
    r#CharRange,
    r#Any,
    r#Seq,
    r#Choice,
    r#Rep,
    r#RepAtLeastOnce,
    r#Opt,
    r#RepExact,
    r#RepLeft,
    r#RepRight,
    r#RepLeftRight,
    r#Pos,
    r#Neg,
    r#Push,
    r#Pop,
    r#PopAll,
    r#Peek,
    r#PeekLeft,
    r#PeekRight,
    r#PeekLeftRight,
    r#Drop,
    r#PeekAll,
}
impl ::pest::typed::RuleType for Rule {
    const EOI: Self = Rule::EOI;
    type Trivia<'i> = trivia::Trivia<'i>;
}
mod trivia {
    pub type Trivia<'i> = super::generics::Rep<super::generics::Str<super::wrapper::W0>>;
}
mod wrapper {
    #[doc = "A wrapper for `\" \"`."]
    #[derive(Clone, Debug, Hash, PartialEq, Eq)]
    pub struct W0;
    impl ::pest::typed::wrapper::String for W0 {
        const CONTENT: &'static ::core::primitive::str = " ";
    }
    #[doc = "A wrapper for `\"+\"`."]
    #[derive(Clone, Debug, Hash, PartialEq, Eq)]
    pub struct W1;
    impl ::pest::typed::wrapper::String for W1 {
        const CONTENT: &'static ::core::primitive::str = "+";
    }
    #[doc = "A wrapper for `\"(\"`."]
    #[derive(Clone, Debug, Hash, PartialEq, Eq)]
    pub struct W2;
    impl ::pest::typed::wrapper::String for W2 {
        const CONTENT: &'static ::core::primitive::str = "(";
    }
    #[doc = "A wrapper for `\")\"`."]
    #[derive(Clone, Debug, Hash, PartialEq, Eq)]
    pub struct W3;
    impl ::pest::typed::wrapper::String for W3 {
        const CONTENT: &'static ::core::primitive::str = ")";
    }
    #[doc = "A wrapper for `\"r#\"`."]
    #[derive(Clone, Debug, Hash, PartialEq, Eq)]
    pub struct W4;
    impl ::pest::typed::wrapper::String for W4 {
        const CONTENT: &'static ::core::primitive::str = "r#";
    }
    #[doc = "A wrapper for `\"1\"`."]
    #[derive(Clone, Debug, Hash, PartialEq, Eq)]
    pub struct W5;
    impl ::pest::typed::wrapper::String for W5 {
        const CONTENT: &'static ::core::primitive::str = "1";
    }
    #[doc = "A wrapper for `\".\"`."]
    #[derive(Clone, Debug, Hash, PartialEq, Eq)]
    pub struct W6;
    impl ::pest::typed::wrapper::String for W6 {
        const CONTENT: &'static ::core::primitive::str = ".";
    }
    #[doc = "A wrapper for `\"a\"`."]
    #[derive(Clone, Debug, Hash, PartialEq, Eq)]
    pub struct W7;
    impl ::pest::typed::wrapper::String for W7 {
        const CONTENT: &'static ::core::primitive::str = "a";
    }
    #[doc = "A wrapper for `\"b\"`."]
    #[derive(Clone, Debug, Hash, PartialEq, Eq)]
    pub struct W8;
    impl ::pest::typed::wrapper::String for W8 {
        const CONTENT: &'static ::core::primitive::str = "b";
    }
    #[doc = "A wrapper for `\"c\"`."]
    #[derive(Clone, Debug, Hash, PartialEq, Eq)]
    pub struct W9;
    impl ::pest::typed::wrapper::String for W9 {
        const CONTENT: &'static ::core::primitive::str = "c";
    }
    #[doc = "A wrapper for `\"?\"`."]
    #[derive(Clone, Debug, Hash, PartialEq, Eq)]
    pub struct W10;
    impl ::pest::typed::wrapper::String for W10 {
        const CONTENT: &'static ::core::primitive::str = "?";
    }
}
#[doc = "Definitions of statically typed nodes generated by pest-generator."]
pub mod rules {
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct r#Regular<'i> {
        content: ::pest::std::Box<super::generics::RepOnce<super::generics::CharRange<'0', '9'>>>,
        span: ::pest::Span<'i>,
    }
    impl<'i> ::pest::typed::TypedNode<'i, super::Rule> for r#Regular<'i> {
        fn try_parse_with_partial(input: ::pest::Position<'i>, stack: &mut ::pest::Stack<::pest::Span<'i>>, tracker: &mut ::pest::typed::Tracker<'i, super::Rule>) -> ::pest::std::Option<(::pest::Position<'i>, Self)> {
            let (pos, content) = super::generics::RepOnce::<super::generics::CharRange<'0', '9'>>::try_parse_with_partial(input, stack, tracker)?;
            let content = content.into();
            let span = input.span(&pos);
            let res = Self { content, span };
            ::pest::std::Some((pos, res))
        }
    }
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct r#Atomic<'i> {
        content: ::pest::std::Box<super::generics::Sequence3<super::generics::RepOnce<super::rules::CharRange<'i>>, false, super::generics::Str<super::wrapper::W1>, true, super::generics::RepOnce<super::rules::CharRange<'i>>, true>>,
        span: ::pest::Span<'i>,
    }
    impl<'i> ::pest::typed::TypedNode<'i, super::Rule> for r#Atomic<'i> {
        fn try_parse_with_partial(input: ::pest::Position<'i>, stack: &mut ::pest::Stack<::pest::Span<'i>>, tracker: &mut ::pest::typed::Tracker<'i, super::Rule>) -> ::pest::std::Option<(::pest::Position<'i>, Self)> {
            let (pos, content) = super::generics::Sequence3::<super::generics::RepOnce<super::rules::CharRange<'i>>, false, super::generics::Str<super::wrapper::W1>, true, super::generics::RepOnce<super::rules::CharRange<'i>>, true>::try_parse_with_partial(input, stack, tracker)?;
            let content = content.into();
            let span = input.span(&pos);
            let res = Self { content, span };
            ::pest::std::Some((pos, res))
        }
    }
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct r#NonAtomic<'i> {
        content: ::pest::std::Box<super::generics::Sequence2<super::generics::Str<super::wrapper::W2>, false, super::generics::Str<super::wrapper::W3>, true>>,
        span: ::pest::Span<'i>,
    }
    impl<'i> ::pest::typed::TypedNode<'i, super::Rule> for r#NonAtomic<'i> {
        fn try_parse_with_partial(input: ::pest::Position<'i>, stack: &mut ::pest::Stack<::pest::Span<'i>>, tracker: &mut ::pest::typed::Tracker<'i, super::Rule>) -> ::pest::std::Option<(::pest::Position<'i>, Self)> {
            let (pos, content) = super::generics::Sequence2::<super::generics::Str<super::wrapper::W2>, false, super::generics::Str<super::wrapper::W3>, true>::try_parse_with_partial(input, stack, tracker)?;
            let content = content.into();
            let span = input.span(&pos);
            let res = Self { content, span };
            ::pest::std::Some((pos, res))
        }
    }
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct r#ExactString<'i> {
        content: ::pest::std::Box<super::generics::Str<super::wrapper::W4>>,
        span: ::pest::Span<'i>,
    }
    impl<'i> ::pest::typed::TypedNode<'i, super::Rule> for r#ExactString<'i> {
        fn try_parse_with_partial(input: ::pest::Position<'i>, stack: &mut ::pest::Stack<::pest::Span<'i>>, tracker: &mut ::pest::typed::Tracker<'i, super::Rule>) -> ::pest::std::Option<(::pest::Position<'i>, Self)> {
            let (pos, content) = super::generics::Str::<super::wrapper::W4>::try_parse_with_partial(input, stack, tracker)?;
            let content = content.into();
            let span = input.span(&pos);
            let res = Self { content, span };
            ::pest::std::Some((pos, res))
        }
    }
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct r#CharRange<'i> {
        content: ::pest::std::Box<super::generics::CharRange<'0', '9'>>,
        span: ::pest::Span<'i>,
    }
    impl<'i> ::pest::typed::TypedNode<'i, super::Rule> for r#CharRange<'i> {
        fn try_parse_with_partial(input: ::pest::Position<'i>, stack: &mut ::pest::Stack<::pest::Span<'i>>, tracker: &mut ::pest::typed::Tracker<'i, super::Rule>) -> ::pest::std::Option<(::pest::Position<'i>, Self)> {
            let (pos, content) = super::generics::CharRange::<'0', '9'>::try_parse_with_partial(input, stack, tracker)?;
            let content = content.into();
            let span = input.span(&pos);
            let res = Self { content, span };
            ::pest::std::Some((pos, res))
        }
    }
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct r#Any<'i> {
        content: ::pest::std::Box<super::generics::r#any>,
        span: ::pest::Span<'i>,
    }
    impl<'i> ::pest::typed::TypedNode<'i, super::Rule> for r#Any<'i> {
        fn try_parse_with_partial(input: ::pest::Position<'i>, stack: &mut ::pest::Stack<::pest::Span<'i>>, tracker: &mut ::pest::typed::Tracker<'i, super::Rule>) -> ::pest::std::Option<(::pest::Position<'i>, Self)> {
            let (pos, content) = super::generics::r#any::try_parse_with_partial(input, stack, tracker)?;
            let content = content.into();
            let span = input.span(&pos);
            let res = Self { content, span };
            ::pest::std::Some((pos, res))
        }
    }
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct r#Seq<'i> {
        content: ::pest::std::Box<super::generics::Sequence3<super::generics::Str<super::wrapper::W5>, false, super::generics::CharRange<'2', '9'>, true, super::generics::Str<super::wrapper::W6>, true>>,
        span: ::pest::Span<'i>,
    }
    impl<'i> ::pest::typed::TypedNode<'i, super::Rule> for r#Seq<'i> {
        fn try_parse_with_partial(input: ::pest::Position<'i>, stack: &mut ::pest::Stack<::pest::Span<'i>>, tracker: &mut ::pest::typed::Tracker<'i, super::Rule>) -> ::pest::std::Option<(::pest::Position<'i>, Self)> {
            let (pos, content) = super::generics::Sequence3::<super::generics::Str<super::wrapper::W5>, false, super::generics::CharRange<'2', '9'>, true, super::generics::Str<super::wrapper::W6>, true>::try_parse_with_partial(input, stack, tracker)?;
            let content = content.into();
            let span = input.span(&pos);
            let res = Self { content, span };
            ::pest::std::Some((pos, res))
        }
    }
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct r#Choice<'i> {
        content: ::pest::std::Box<super::generics::Choice9<super::generics::Str<super::wrapper::W7>, super::generics::Sequence2<super::generics::RepOnce<super::generics::Str<super::wrapper::W8>>, false, super::rules::RepAtLeastOnce<'i>, true>, super::generics::Sequence4<super::generics::Positive<super::generics::Str<super::wrapper::W9>>, false, super::rules::Choice<'i>, true, super::rules::Rep<'i>, true, super::rules::Opt<'i>, true>, super::rules::Peek<'i>, super::rules::PeekLeft<'i>, super::rules::PeekRight<'i>, super::rules::PeekLeftRight<'i>, super::rules::Drop<'i>, super::rules::PeekAll<'i>>>,
        span: ::pest::Span<'i>,
    }
    impl<'i> ::pest::typed::TypedNode<'i, super::Rule> for r#Choice<'i> {
        fn try_parse_with_partial(input: ::pest::Position<'i>, stack: &mut ::pest::Stack<::pest::Span<'i>>, tracker: &mut ::pest::typed::Tracker<'i, super::Rule>) -> ::pest::std::Option<(::pest::Position<'i>, Self)> {
            let (pos, content) = super::generics::Choice9::<super::generics::Str<super::wrapper::W7>, super::generics::Sequence2<super::generics::RepOnce<super::generics::Str<super::wrapper::W8>>, false, super::rules::RepAtLeastOnce<'i>, true>, super::generics::Sequence4<super::generics::Positive<super::generics::Str<super::wrapper::W9>>, false, super::rules::Choice<'i>, true, super::rules::Rep<'i>, true, super::rules::Opt<'i>, true>, super::rules::Peek<'i>, super::rules::PeekLeft<'i>, super::rules::PeekRight<'i>, super::rules::PeekLeftRight<'i>, super::rules::Drop<'i>, super::rules::PeekAll<'i>>::try_parse_with_partial(input, stack, tracker)?;
            let content = content.into();
            let span = input.span(&pos);
            let res = Self { content, span };
            ::pest::std::Some((pos, res))
        }
    }
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct r#Rep<'i> {
        content: ::pest::std::Box<super::generics::Rep<super::generics::Str<super::wrapper::W8>>>,
        span: ::pest::Span<'i>,
    }
    impl<'i> ::pest::typed::TypedNode<'i, super::Rule> for r#Rep<'i> {
        fn try_parse_with_partial(input: ::pest::Position<'i>, stack: &mut ::pest::Stack<::pest::Span<'i>>, tracker: &mut ::pest::typed::Tracker<'i, super::Rule>) -> ::pest::std::Option<(::pest::Position<'i>, Self)> {
            let (pos, content) = super::generics::Rep::<super::generics::Str<super::wrapper::W8>>::try_parse_with_partial(input, stack, tracker)?;
            let content = content.into();
            let span = input.span(&pos);
            let res = Self { content, span };
            ::pest::std::Some((pos, res))
        }
    }
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct r#RepAtLeastOnce<'i> {
        content: ::pest::std::Box<super::generics::RepOnce<super::generics::CharRange<'0', '9'>>>,
        span: ::pest::Span<'i>,
    }
    impl<'i> ::pest::typed::TypedNode<'i, super::Rule> for r#RepAtLeastOnce<'i> {
        fn try_parse_with_partial(input: ::pest::Position<'i>, stack: &mut ::pest::Stack<::pest::Span<'i>>, tracker: &mut ::pest::typed::Tracker<'i, super::Rule>) -> ::pest::std::Option<(::pest::Position<'i>, Self)> {
            let (pos, content) = super::generics::RepOnce::<super::generics::CharRange<'0', '9'>>::try_parse_with_partial(input, stack, tracker)?;
            let content = content.into();
            let span = input.span(&pos);
            let res = Self { content, span };
            ::pest::std::Some((pos, res))
        }
    }
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct r#Opt<'i> {
        content: ::pest::std::Box<::pest::std::Option<super::generics::Str<super::wrapper::W10>>>,
        span: ::pest::Span<'i>,
    }
    impl<'i> ::pest::typed::TypedNode<'i, super::Rule> for r#Opt<'i> {
        fn try_parse_with_partial(input: ::pest::Position<'i>, stack: &mut ::pest::Stack<::pest::Span<'i>>, tracker: &mut ::pest::typed::Tracker<'i, super::Rule>) -> ::pest::std::Option<(::pest::Position<'i>, Self)> {
            let (pos, content) = ::pest::std::Option::<super::generics::Str<super::wrapper::W10>>::try_parse_with_partial(input, stack, tracker)?;
            let content = content.into();
            let span = input.span(&pos);
            let res = Self { content, span };
            ::pest::std::Some((pos, res))
        }
    }
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct r#RepExact<'i> {
        content: ::pest::std::Box<super::generics::RepMinMax<super::rules::RepAtLeastOnce<'i>, 3usize, 3usize>>,
        span: ::pest::Span<'i>,
    }
    impl<'i> ::pest::typed::TypedNode<'i, super::Rule> for r#RepExact<'i> {
        fn try_parse_with_partial(input: ::pest::Position<'i>, stack: &mut ::pest::Stack<::pest::Span<'i>>, tracker: &mut ::pest::typed::Tracker<'i, super::Rule>) -> ::pest::std::Option<(::pest::Position<'i>, Self)> {
            let (pos, content) = super::generics::RepMinMax::<super::rules::RepAtLeastOnce<'i>, 3usize, 3usize>::try_parse_with_partial(input, stack, tracker)?;
            let content = content.into();
            let span = input.span(&pos);
            let res = Self { content, span };
            ::pest::std::Some((pos, res))
        }
    }
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct r#RepLeft<'i> {
        content: ::pest::std::Box<super::generics::RepMin<super::rules::RepExact<'i>, 1usize>>,
        span: ::pest::Span<'i>,
    }
    impl<'i> ::pest::typed::TypedNode<'i, super::Rule> for r#RepLeft<'i> {
        fn try_parse_with_partial(input: ::pest::Position<'i>, stack: &mut ::pest::Stack<::pest::Span<'i>>, tracker: &mut ::pest::typed::Tracker<'i, super::Rule>) -> ::pest::std::Option<(::pest::Position<'i>, Self)> {
            let (pos, content) = super::generics::RepMin::<super::rules::RepExact<'i>, 1usize>::try_parse_with_partial(input, stack, tracker)?;
            let content = content.into();
            let span = input.span(&pos);
            let res = Self { content, span };
            ::pest::std::Some((pos, res))
        }
    }
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct r#RepRight<'i> {
        content: ::pest::std::Box<super::generics::RepMax<super::rules::RepLeft<'i>, 2usize>>,
        span: ::pest::Span<'i>,
    }
    impl<'i> ::pest::typed::TypedNode<'i, super::Rule> for r#RepRight<'i> {
        fn try_parse_with_partial(input: ::pest::Position<'i>, stack: &mut ::pest::Stack<::pest::Span<'i>>, tracker: &mut ::pest::typed::Tracker<'i, super::Rule>) -> ::pest::std::Option<(::pest::Position<'i>, Self)> {
            let (pos, content) = super::generics::RepMax::<super::rules::RepLeft<'i>, 2usize>::try_parse_with_partial(input, stack, tracker)?;
            let content = content.into();
            let span = input.span(&pos);
            let res = Self { content, span };
            ::pest::std::Some((pos, res))
        }
    }
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct r#RepLeftRight<'i> {
        content: ::pest::std::Box<super::generics::RepMinMax<super::rules::RepRight<'i>, 1usize, 2usize>>,
        span: ::pest::Span<'i>,
    }
    impl<'i> ::pest::typed::TypedNode<'i, super::Rule> for r#RepLeftRight<'i> {
        fn try_parse_with_partial(input: ::pest::Position<'i>, stack: &mut ::pest::Stack<::pest::Span<'i>>, tracker: &mut ::pest::typed::Tracker<'i, super::Rule>) -> ::pest::std::Option<(::pest::Position<'i>, Self)> {
            let (pos, content) = super::generics::RepMinMax::<super::rules::RepRight<'i>, 1usize, 2usize>::try_parse_with_partial(input, stack, tracker)?;
            let content = content.into();
            let span = input.span(&pos);
            let res = Self { content, span };
            ::pest::std::Some((pos, res))
        }
    }
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct r#Pos<'i> {
        content: ::pest::std::Box<super::generics::Positive<super::generics::Sequence2<super::generics::r#SOI, false, super::generics::RepMinMax<super::rules::RepLeftRight<'i>, 2usize, 4usize>, true>>>,
        span: ::pest::Span<'i>,
    }
    impl<'i> ::pest::typed::TypedNode<'i, super::Rule> for r#Pos<'i> {
        fn try_parse_with_partial(input: ::pest::Position<'i>, stack: &mut ::pest::Stack<::pest::Span<'i>>, tracker: &mut ::pest::typed::Tracker<'i, super::Rule>) -> ::pest::std::Option<(::pest::Position<'i>, Self)> {
            let (pos, content) = super::generics::Positive::<super::generics::Sequence2<super::generics::r#SOI, false, super::generics::RepMinMax<super::rules::RepLeftRight<'i>, 2usize, 4usize>, true>>::try_parse_with_partial(input, stack, tracker)?;
            let content = content.into();
            let span = input.span(&pos);
            let res = Self { content, span };
            ::pest::std::Some((pos, res))
        }
    }
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct r#Neg<'i> {
        content: ::pest::std::Box<super::generics::Negative<super::generics::Sequence2<super::generics::r#EOI, false, super::rules::Pos<'i>, true>>>,
        span: ::pest::Span<'i>,
    }
    impl<'i> ::pest::typed::TypedNode<'i, super::Rule> for r#Neg<'i> {
        fn try_parse_with_partial(input: ::pest::Position<'i>, stack: &mut ::pest::Stack<::pest::Span<'i>>, tracker: &mut ::pest::typed::Tracker<'i, super::Rule>) -> ::pest::std::Option<(::pest::Position<'i>, Self)> {
            let (pos, content) = super::generics::Negative::<super::generics::Sequence2<super::generics::r#EOI, false, super::rules::Pos<'i>, true>>::try_parse_with_partial(input, stack, tracker)?;
            let content = content.into();
            let span = input.span(&pos);
            let res = Self { content, span };
            ::pest::std::Some((pos, res))
        }
    }
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct r#Push<'i> {
        content: ::pest::std::Box<super::generics::r#push<super::generics::Sequence7<super::generics::Rep<super::rules::RepLeft<'i>>, false, super::rules::Neg<'i>, true, super::generics::RepOnce<super::rules::ExactString<'i>>, true, super::rules::Push<'i>, true, super::rules::Pop<'i>, true, super::rules::Push<'i>, true, super::rules::PopAll<'i>, true>>>,
        span: ::pest::Span<'i>,
    }
    impl<'i> ::pest::typed::TypedNode<'i, super::Rule> for r#Push<'i> {
        fn try_parse_with_partial(input: ::pest::Position<'i>, stack: &mut ::pest::Stack<::pest::Span<'i>>, tracker: &mut ::pest::typed::Tracker<'i, super::Rule>) -> ::pest::std::Option<(::pest::Position<'i>, Self)> {
            let (pos, content) = super::generics::r#push::<super::generics::Sequence7<super::generics::Rep<super::rules::RepLeft<'i>>, false, super::rules::Neg<'i>, true, super::generics::RepOnce<super::rules::ExactString<'i>>, true, super::rules::Push<'i>, true, super::rules::Pop<'i>, true, super::rules::Push<'i>, true, super::rules::PopAll<'i>, true>>::try_parse_with_partial(input, stack, tracker)?;
            let content = content.into();
            let span = input.span(&pos);
            let res = Self { content, span };
            ::pest::std::Some((pos, res))
        }
    }
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct r#Pop<'i> {
        content: ::pest::std::Box<super::generics::r#pop<'i>>,
        span: ::pest::Span<'i>,
    }
    impl<'i> ::pest::typed::TypedNode<'i, super::Rule> for r#Pop<'i> {
        fn try_parse_with_partial(input: ::pest::Position<'i>, stack: &mut ::pest::Stack<::pest::Span<'i>>, tracker: &mut ::pest::typed::Tracker<'i, super::Rule>) -> ::pest::std::Option<(::pest::Position<'i>, Self)> {
            let (pos, content) = super::generics::r#pop::<'i>::try_parse_with_partial(input, stack, tracker)?;
            let content = content.into();
            let span = input.span(&pos);
            let res = Self { content, span };
            ::pest::std::Some((pos, res))
        }
    }
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct r#PopAll<'i> {
        content: ::pest::std::Box<super::generics::r#pop_all<'i>>,
        span: ::pest::Span<'i>,
    }
    impl<'i> ::pest::typed::TypedNode<'i, super::Rule> for r#PopAll<'i> {
        fn try_parse_with_partial(input: ::pest::Position<'i>, stack: &mut ::pest::Stack<::pest::Span<'i>>, tracker: &mut ::pest::typed::Tracker<'i, super::Rule>) -> ::pest::std::Option<(::pest::Position<'i>, Self)> {
            let (pos, content) = super::generics::r#pop_all::<'i>::try_parse_with_partial(input, stack, tracker)?;
            let content = content.into();
            let span = input.span(&pos);
            let res = Self { content, span };
            ::pest::std::Some((pos, res))
        }
    }
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct r#Peek<'i> {
        content: ::pest::std::Box<super::generics::r#peek<'i>>,
        span: ::pest::Span<'i>,
    }
    impl<'i> ::pest::typed::TypedNode<'i, super::Rule> for r#Peek<'i> {
        fn try_parse_with_partial(input: ::pest::Position<'i>, stack: &mut ::pest::Stack<::pest::Span<'i>>, tracker: &mut ::pest::typed::Tracker<'i, super::Rule>) -> ::pest::std::Option<(::pest::Position<'i>, Self)> {
            let (pos, content) = super::generics::r#peek::<'i>::try_parse_with_partial(input, stack, tracker)?;
            let content = content.into();
            let span = input.span(&pos);
            let res = Self { content, span };
            ::pest::std::Some((pos, res))
        }
    }
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct r#PeekLeft<'i> {
        content: ::pest::std::Box<super::generics::r#peek<'i>>,
        span: ::pest::Span<'i>,
    }
    impl<'i> ::pest::typed::TypedNode<'i, super::Rule> for r#PeekLeft<'i> {
        fn try_parse_with_partial(input: ::pest::Position<'i>, stack: &mut ::pest::Stack<::pest::Span<'i>>, tracker: &mut ::pest::typed::Tracker<'i, super::Rule>) -> ::pest::std::Option<(::pest::Position<'i>, Self)> {
            let (pos, content) = super::generics::r#peek::<'i>::try_parse_with_partial(input, stack, tracker)?;
            let content = content.into();
            let span = input.span(&pos);
            let res = Self { content, span };
            ::pest::std::Some((pos, res))
        }
    }
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct r#PeekRight<'i> {
        content: ::pest::std::Box<super::generics::r#peek<'i>>,
        span: ::pest::Span<'i>,
    }
    impl<'i> ::pest::typed::TypedNode<'i, super::Rule> for r#PeekRight<'i> {
        fn try_parse_with_partial(input: ::pest::Position<'i>, stack: &mut ::pest::Stack<::pest::Span<'i>>, tracker: &mut ::pest::typed::Tracker<'i, super::Rule>) -> ::pest::std::Option<(::pest::Position<'i>, Self)> {
            let (pos, content) = super::generics::r#peek::<'i>::try_parse_with_partial(input, stack, tracker)?;
            let content = content.into();
            let span = input.span(&pos);
            let res = Self { content, span };
            ::pest::std::Some((pos, res))
        }
    }
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct r#PeekLeftRight<'i> {
        content: ::pest::std::Box<super::generics::r#peek<'i>>,
        span: ::pest::Span<'i>,
    }
    impl<'i> ::pest::typed::TypedNode<'i, super::Rule> for r#PeekLeftRight<'i> {
        fn try_parse_with_partial(input: ::pest::Position<'i>, stack: &mut ::pest::Stack<::pest::Span<'i>>, tracker: &mut ::pest::typed::Tracker<'i, super::Rule>) -> ::pest::std::Option<(::pest::Position<'i>, Self)> {
            let (pos, content) = super::generics::r#peek::<'i>::try_parse_with_partial(input, stack, tracker)?;
            let content = content.into();
            let span = input.span(&pos);
            let res = Self { content, span };
            ::pest::std::Some((pos, res))
        }
    }
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct r#Drop<'i> {
        content: ::pest::std::Box<super::generics::r#drop>,
        span: ::pest::Span<'i>,
    }
    impl<'i> ::pest::typed::TypedNode<'i, super::Rule> for r#Drop<'i> {
        fn try_parse_with_partial(input: ::pest::Position<'i>, stack: &mut ::pest::Stack<::pest::Span<'i>>, tracker: &mut ::pest::typed::Tracker<'i, super::Rule>) -> ::pest::std::Option<(::pest::Position<'i>, Self)> {
            let (pos, content) = super::generics::r#drop::try_parse_with_partial(input, stack, tracker)?;
            let content = content.into();
            let span = input.span(&pos);
            let res = Self { content, span };
            ::pest::std::Some((pos, res))
        }
    }
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct r#PeekAll<'i> {
        content: ::pest::std::Box<super::generics::r#peek_all<'i>>,
        span: ::pest::Span<'i>,
    }
    impl<'i> ::pest::typed::TypedNode<'i, super::Rule> for r#PeekAll<'i> {
        fn try_parse_with_partial(input: ::pest::Position<'i>, stack: &mut ::pest::Stack<::pest::Span<'i>>, tracker: &mut ::pest::typed::Tracker<'i, super::Rule>) -> ::pest::std::Option<(::pest::Position<'i>, Self)> {
            let (pos, content) = super::generics::r#peek_all::<'i>::try_parse_with_partial(input, stack, tracker)?;
            let content = content.into();
            let span = input.span(&pos);
            let res = Self { content, span };
            ::pest::std::Some((pos, res))
        }
    }
}
#[doc = "Used generics."]
pub mod generics {
    pub use pest::choice::Choice9;
    pub use pest::sequence::Sequence2;
    pub use pest::sequence::Sequence3;
    pub use pest::sequence::Sequence4;
    pub use pest::sequence::Sequence7;
    pub use pest::typed::template::{CharRange, Negative, Positive, Rep, RepMax, RepMin, RepMinMax, RepOnce, Str, ANY as any, DROP as drop, EOI, PEEK as peek, PEEK_ALL as peek_all, POP as pop, POP_ALL as pop_all, PUSH as push, SOI};
}