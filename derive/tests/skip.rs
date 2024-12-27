#![allow(unused_variables)]
use anyhow::Result;
use pest3::typed::TypedNode as _;
use pest3_derive::Parser;

#[derive(Parser)]
#[grammar_inline = r#"
~          = (" " | "/*" - (!"*/" - pest::any)* - "*/")*
main       = "x"~*
program    = pest::soi ~ main ~ pest::EOI
"#]
struct Parser;

#[test]
fn comment() -> Result<()> {
    rules::main::try_parse_partial("x x x /*x*/")?;
    rules::main::try_parse("x x x /*x*/").unwrap_err();
    Ok(())
}

#[test]
fn skip_on_two_end() -> Result<()> {
    rules::main::try_parse_partial("x x ")?;
    rules::main::try_parse(" x x").unwrap_err();
    rules::main::try_parse("x x ").unwrap_err();
    Ok(())
}

#[test]
fn post_skip() -> Result<()> {
    let program = rules::program::try_parse("x x /*x*/")?;
    Ok(())
}

#[test]
fn pre_skip() -> Result<()> {
    rules::program::try_parse("/* x x */ ")?;
    rules::program::try_parse("/* x x */ x x")?;
    Ok(())
}
