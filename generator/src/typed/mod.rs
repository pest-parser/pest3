//! Generator for typed structures.
//!
//! Generated modules.
//!
//! - Root module, in which the derived structure is.
//!

mod accesser;
mod attr;
mod config;
mod generator;
mod module;
mod output;

pub use generator::derive_typed_parser;
