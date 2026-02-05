use pest3_derive::Parser;

/// Issue `#27` regression grammar parser.
#[derive(Parser)]
#[grammar = "tests/issue27.pest"]
pub struct Parser;
