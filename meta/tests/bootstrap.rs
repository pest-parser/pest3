use pest3::typed::TypedParser;
use pest3_meta::grammar::{rules, PestParser};

#[test]
fn generated_bootstrap_parser_accepts_basic_grammar() {
    PestParser::try_parse::<rules::r#grammar_rules>("main = \"a\"").unwrap();
}
