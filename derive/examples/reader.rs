use pest::typed::{PairTree, TypedNode as _};
use pest_derive::Parser;
use std::io::{stdin, BufRead};

#[derive(Parser)]
#[grammar_inline = r#"
~       = " " | "/*" - (!"*/" - pest::any)* - "*/"
x       = "x"
main    = x~+
"#]
struct Parser;

fn main() -> anyhow::Result<()> {
    let mut stdin = stdin().lock();
    let mut buf = String::new();
    loop {
        buf.clear();
        stdin.read_line(&mut buf)?;
        buf.pop();
        if buf.is_empty() {
            return Ok(());
        }
        match rules::main::try_parse(&buf) {
            Ok(res) => println!("{}", res.as_pair_tree()),
            Err(err) => println!("{err}"),
        }
    }
}
