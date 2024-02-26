#![allow(unused_variables)]
use anyhow::Result;
use pest::typed::TypedNode as _;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "../meta/tests/pest3sample.pest"]
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
