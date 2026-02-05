use pest3_derive::Parser;

/// JSON parser.
#[derive(Parser)]
#[grammar = "tests/issue27.pest"]
pub struct Parser;
