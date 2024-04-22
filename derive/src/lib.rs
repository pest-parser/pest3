//! Derive macros implemented with [pest3_generator].
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
    )
)]
/// Derive typed parser from given grammar.
pub fn derive_typed_parser(input: TokenStream) -> TokenStream {
    pest3_generator::typed::derive_typed_parser(input.into(), true, true).into()
}
