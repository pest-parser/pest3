//! Core part of pest3.

// #![no_std]
#![warn(rust_2018_idioms, rust_2021_compatibility, missing_docs)]

pub mod choice {
    pub use pest3_core::choice::*;
}
pub mod formatter {
    pub use pest3_core::formatter::*;
}
pub mod position {
    pub use pest3_core::position::*;
}
pub mod sequence {
    pub use pest3_core::sequence::*;
}
pub mod span {
    pub use pest3_core::span::*;
}
pub mod token {
    pub use pest3_core::token::*;
}
pub mod typed {
    pub use pest3_core::typed::*;
}

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

#[cfg(feature = "derive")]
pub use pest3_derive::Parser;