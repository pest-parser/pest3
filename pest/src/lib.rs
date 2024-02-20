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

pub use pest2::{error, RuleType, Stack};
pub use position::Position;
pub use span::Span;

/// Re-exported items from [std].
pub mod std {
    pub use std::boxed::Box;
    pub use std::ops::FnMut;
    pub use std::option::{Option, Option::None, Option::Some};
    pub use std::primitive::usize;
    pub use std::vec::Vec;
}
