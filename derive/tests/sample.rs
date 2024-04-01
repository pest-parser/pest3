#![allow(unused_variables)]
use pest3_derive::Parser;

#[derive(Parser)]
#[grammar = "../meta/tests/pest3sample.pest"]
struct Parser;
