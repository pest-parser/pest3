//! Derive macros implemented with [pest3_generator].
//!
//! ## Code Generation
//!
//! ### Meta Rules
//!
//! Rules can have several arguments.
//!
//! Definitions of these rules can use these arguments,
//! and these rules will be mapped to a struct in Rust using generics.
//!
//! ```rust
//! use pest3_derive::Parser;
//! use pest3_core::typed::TypedNode;
//!
//! #[derive(Parser)]
//! #[grammar_inline = r#"
//!   pair(a, b) = a - " "* - ":" - " "* - b
//!   string     = "\"" - ('a'..'z' | 'A'..'Z' | '0'..'9')* - "\""
//!   main       = pair(string, string)
//! "#]
//! struct Parser;
//!
//! fn main() -> anyhow::Result<()> {
//!   use rules::*;
//!   pair::<string, string>::check(r#""a": "b""#)?;
//!   Ok(())
//! }
//! ```
//!
//! Once called with some exact parameters,
//! these arguments are replaced directly as a whole.
//!
//! In the example above,
//! `pair(string, string)` has the same structure with
//! `string - " "* - ":" - " "* - string`.
//!
//! But there are some restrictions on meta rules:
//!
//! - All arguments must be used.
//!   I think there is no reason in pest for one to define an argument
//!   that won't be used, at least for now.
//!
//! And later we may support constant arguments
//! just like the built-in rule `pest::stack::peek`.

#![warn(rust_2018_idioms, rust_2021_compatibility, missing_docs)]

use proc_macro::TokenStream;

#[proc_macro_derive(
    Parser,
    attributes(
        grammar,
        grammar_inline,
        no_pair,
        no_span,
        no_warnings,
        box_rules_only_if_needed,
        no_getter,
        rules_mod,
    )
)]
/// Derive typed parser from given grammar.
pub fn derive_typed_parser(input: TokenStream) -> TokenStream {
    pest3_generator::typed::derive_typed_parser(input.into(), true, true).into()
}
