#![allow(dead_code)]

use pest3_core::token::Pair;
use pest3_vm::{Error, ParseError, Vm};

pub fn test_path(relative: &str) -> String {
    format!("{}/tests/{relative}", env!("CARGO_MANIFEST_DIR"))
}

pub fn vm_from_source(grammar: &str, relative_path: &str) -> Vm {
    Vm::from_source(grammar, test_path(relative_path)).unwrap()
}

pub fn parse_error(error: Error) -> ParseError {
    match error {
        Error::Parse(error) => error,
        other => panic!("expected parse error, got {other:?}"),
    }
}

pub fn pair(rule: &str, start: usize, end: usize, children: Vec<Pair<String>>) -> Pair<String> {
    Pair {
        rule: rule.to_owned(),
        start,
        end,
        children,
    }
}

#[macro_export]
macro_rules! token {
    ($rule:ident ( $start:literal, $end:literal )) => {{
        $crate::common::pair(stringify!($rule), $start, $end, Vec::new())
    }};
    ($rule:ident ( $start:literal, $end:literal, [ $( $names:ident $tokens:tt ),* $(,)* ] )) => {{
        $crate::common::pair(stringify!($rule), $start, $end, vec![$( token!($names $tokens) ),*])
    }};
}
