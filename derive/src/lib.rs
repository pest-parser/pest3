#![warn(rust_2018_idioms, rust_2021_compatibility, missing_docs)]

use proc_macro::TokenStream;

#[proc_macro_derive(
    Parser,
    attributes(
        grammar,
        grammar_inline,
        emit_rule_reference,
        do_not_emit_span,
        no_warnings,
    )
)]
pub fn derive_typed_parser(input: TokenStream) -> TokenStream {
    pest_generator::typed::derive_typed_parser(input.into(), true, true).into()
}
