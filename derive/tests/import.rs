#![allow(unused_variables)]
use pest3_derive::Parser;

#[derive(Parser)]
#[grammar_inline = "
use tests::grammar
main = string
"]
struct Parser;
