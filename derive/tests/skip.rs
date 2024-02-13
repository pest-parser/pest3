#![allow(unused_variables)]
use anyhow::Result;
use pest::typed::TypedNode as _;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar_inline = r#"
~          = " " | "/*" - (!"*/" - pest::any)* - "*/"
main       = "x"~*
program    = pest::SOI ~ main ~ pest::EOI
"#]
struct Parser;

#[test]
fn comment() -> Result<()> {
    let vec = rules::main::try_parse_partial("x x x /*x*/")?;
    Ok(())
}

#[test]
fn skip_on_two_end() {
    rules::main::try_parse(" x x").unwrap_err();
    rules::main::try_parse_partial("x x ").unwrap();
}

#[test]
fn post_skip() -> Result<()> {
    let program = rules::program::try_parse("x x /*x*/")?;
    Ok(())
}

#[test]
fn pre_skip() -> Result<()> {
    let program = rules::program::try_parse("/* x x */ x x")?;
    Ok(())
}
