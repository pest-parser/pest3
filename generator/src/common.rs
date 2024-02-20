//! Some common codes that most backends will use.

use pest_meta::{doc::DocComment, parser::ParseRule};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use std::path::PathBuf;

/// Generate Rust `include_str!` for grammar files, then Cargo will watch changes in grammars.
pub(crate) fn generate_include(name: &Ident, paths: Vec<PathBuf>) -> TokenStream {
    let const_name = format_ident!("_PEST_GRAMMAR_{}", name);
    // Need to make this relative to the current directory since the path to the file
    // is derived from the CARGO_MANIFEST_DIR environment variable
    let current_dir = std::env::current_dir().expect("Unable to get current directory");

    let include_tokens = paths.iter().map(|path| {
        let path = path.to_str().expect("non-Unicode path");

        let relative_path = current_dir
            .join(path)
            .to_str()
            .expect("path contains invalid unicode")
            .to_string();

        quote! {
            include_str!(#relative_path)
        }
    });

    let len = include_tokens.len();
    quote! {
        #[allow(non_upper_case_globals)]
        const #const_name: [&'static ::core::primitive::str; #len] = [
            #(#include_tokens),*
        ];
    }
}

pub(crate) fn generate_rule_enum(rules: &[ParseRule], doc_comment: &DocComment) -> TokenStream {
    let rules = rules
        .iter()
        .filter(|rule| rule.name != "~" && rule.name != "^")
        .map(|rule| {
            let rule_name = format_ident!("r#{}", rule.name);

            let doc = &rule.doc;
            quote! {
                #(
                    #[doc = #doc]
                )*
                #rule_name,
            }
        });

    let grammar_doc = &doc_comment.grammar_doc;
    let grammar_doc = quote! {#(#[doc = #grammar_doc])*};
    quote! {
        #grammar_doc
        #[allow(dead_code, missing_docs, non_camel_case_types, clippy::upper_case_acronyms)]
        #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub enum Rule {
            EOI,
            #( #rules )*
        }
    }
}
