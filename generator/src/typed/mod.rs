//! Generator for typed structures.
//!
//! Generated modules.
//!
//! - Root module, in which the derived structure is.
//!

mod attr;
mod config;
mod generator;
mod getter;
mod module;
mod output;

pub use generator::derive_typed_parser;
