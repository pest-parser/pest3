use anyhow::Result;
use pest3::typed::TypedParser;
use pest3_derive::Parser;
use serde_json::{json, Value};
use std::iter::once;

#[derive(Parser)]
#[grammar_inline = r#"
separated(i, sep) = i - (sep - i)*
cell              = ('a'..'z' | 'A'..'Z' | '0'..'9' | ".")+
main              = separated(separated(cell, " "* - "," - " "*), "\n")
"#]
struct Parser;

fn to_json(input: &str) -> Result<Value> {
    let file = Parser::try_parse::<rules::main>(input)?;
    let (first, following) = file.separated().i();
    let lines = once(first).chain(following);
    let lines = lines.map(|line| {
        let (first, following) = line.i();
        let cells = once(first).chain(following);
        let cells = cells.map(|cell| Value::String(cell.span.as_str().to_owned()));
        Value::Array(cells.collect())
    });
    let res = Value::Array(lines.collect());
    Ok(res)
}

#[test]
fn main() -> Result<()> {
    assert_eq!(to_json("123,456")?, json!([["123", "456"]]));
    assert_eq!(
        to_json("123,456\n789,abc")?,
        json!([["123", "456"], ["789", "abc"]])
    );
    Ok(())
}
