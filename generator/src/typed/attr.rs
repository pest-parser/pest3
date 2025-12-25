use super::config::Config;
use crate::config::{get_bool, get_string, GrammarSource};
use proc_macro2::Ident;
use syn::{DeriveInput, Generics};

pub(crate) fn parse_derive(ast: DeriveInput) -> (Ident, Generics, Vec<GrammarSource>, Config) {
    let name = ast.ident;
    let generics = ast.generics;

    let mut grammar_sources = vec![];
    let mut config = Config::default();
    for attr in ast.attrs.iter() {
        let path = attr.meta.path();
        if path.is_ident("grammar") {
            grammar_sources.push(GrammarSource::File(get_string(attr, "grammar")));
        } else if path.is_ident("grammar_inline") {
            grammar_sources.push(GrammarSource::Inline(get_string(attr, "grammar_inline")));
        } else if path.is_ident("no_pair") {
            config.no_pair = get_bool(attr, "no_pair");
        } else if path.is_ident("no_span") {
            config.no_span = get_bool(attr, "no_span");
        } else if path.is_ident("no_warnings") {
            config.no_warnings = get_bool(attr, "no_warnings");
        } else if path.is_ident("box_all_rules") {
            config.box_rules_only_if_needed = get_bool(attr, "box_all_rules");
        } else if path.is_ident("rules_mod") {
            config.rules_mod = Some(get_string(attr, "rules_mod"));
        }
    }

    if grammar_sources.is_empty() {
        panic!("A grammar file needs to be provided with the #[grammar = \"PATH\"] or #[grammar_inline = \"GRAMMAR CONTENTS\"] attribute.");
    }

    (name, generics, grammar_sources, config)
}
