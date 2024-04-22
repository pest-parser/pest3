//! Some common codes that most backends will use.

use pest3_meta::{
    doc::DocComment,
    parser::{GrammarModule, Import, ParseRule},
};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use std::path::PathBuf;

use crate::types::pest;

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

pub(crate) fn generate_rule_enum(module: &GrammarModule) -> TokenStream {
    let this = pest();
    let GrammarModule {
        rules,
        doc,
        imports,
        ..
    } = module;
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

    let subrules = imports.iter().map(|import| match import {
        Import::Builtin(_, _) => quote! {},
        Import::File(name, module) => {
            let ident = format_ident!("r#{}", name);
            quote! {
                #ident(rules::#ident::Rule),
            }
        }
    });
    let impl_sup_rules = imports.iter().map(|import| match import {
        Import::Builtin(_, _) => quote! {},
        Import::File(name, module) => {
            let ident = format_ident!("r#{}", name);
            quote! {
                impl #this::typed::SuperRule<rules::#ident::Rule> for Rule {
                    fn cvt_from(rule: rules::#ident::Rule) -> Self {
                        Self::#ident(rule)
                    }
                }
            }
        }
    });

    let grammar_doc = &doc.grammar_doc;
    let grammar_doc = quote! {#(#[doc = #grammar_doc])*};
    quote! {
        #grammar_doc
        #[allow(dead_code, missing_docs, non_camel_case_types, clippy::upper_case_acronyms)]
        #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub enum Rule {
            EOI,
            #( #rules )*
            #( #subrules )*
        }
        #( #impl_sup_rules )*
    }
}
