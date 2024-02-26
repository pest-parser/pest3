use super::config::Config;
use crate::config::{get_bool, get_string, GrammarSource};
use proc_macro2::Ident;
use quote::ToTokens;
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
        } else if path.is_ident("emit_rule_reference") {
            config.emit_rule_reference = get_bool(attr, "emit_rule_reference");
        } else if path.is_ident("do_not_emit_span") {
            config.do_not_emit_span = get_bool(attr, "do_not_emit_span");
        } else if path.is_ident("no_warnings") {
            config.no_warnings = get_bool(attr, "no_warnings");
        } else if path.is_ident("box_all_rules") {
            config.box_all_rules = get_bool(attr, "box_all_rules");
        }
    }

    if grammar_sources.is_empty() {
        panic!("A grammar file needs to be provided with the #[grammar = \"PATH\"] or #[grammar_inline = \"GRAMMAR CONTENTS\"] attribute.");
    }

    (name, generics, grammar_sources, config)
}
