//! Core part of pest3.

// #![no_std]
#![warn(rust_2018_idioms, rust_2021_compatibility, missing_docs)]

pub mod choice;
pub mod formatter;
pub mod position;
pub mod sequence;
pub mod span;
pub mod token;
pub mod typed;

extern crate alloc;

pub use pest2::{error, unicode, RuleType, Stack};
pub use position::Position;
pub use span::Span;

/// Re-exported items from [std].
pub mod std {
    pub use std::boxed::Box;
    pub use std::ops::FnMut;
    pub use std::option::{Option, Option::None, Option::Some};
    pub use std::primitive::{bool, char, usize};
    pub use std::result::{Result, Result::Err, Result::Ok};
    pub use std::vec::Vec;
}

/// Generate unicode property type.
#[macro_export]
macro_rules! unicode {
    ($property:ident) => {
        #[doc = ::core::stringify!(Auto generated. Unicode property $property.)]
        #[derive(Clone, Debug, Hash, PartialEq, Eq)]
        #[allow(non_camel_case_types)]
        pub struct $property {
            pub content: $crate::std::char,
        }
        impl<'i, R: $crate::typed::RuleType> $crate::typed::TypedNode<'i, R> for $property {
            #[inline]
            fn try_parse_with_partial(
                mut input: $crate::Position<'i>,
                _stack: &mut $crate::Stack<$crate::Span<'i>>,
                tracker: &mut $crate::typed::Tracker<'i, R>,
            ) -> $crate::std::Option<($crate::Position<'i>, Self)> {
                match $crate::typed::template::match_char_by(&mut input, $crate::unicode::$property)
                {
                    $crate::std::Some(content) => $crate::std::Some((input, Self { content })),
                    $crate::std::None => $crate::std::None,
                }
            }
            #[inline]
            fn check_with_partial(
                mut input: $crate::Position<'i>,
                _stack: &mut $crate::Stack<$crate::Span<'i>>,
                tracker: &mut $crate::typed::Tracker<'i, R>,
            ) -> $crate::std::Option<$crate::Position<'i>> {
                match $crate::typed::template::match_char_by(&mut input, $crate::unicode::$property)
                {
                    $crate::std::Some(_) => $crate::std::Some(input),
                    $crate::std::None => $crate::std::None,
                }
            }
        }
        impl $crate::typed::EmptyPairContainer for $property {}
    };
}
