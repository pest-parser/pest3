mod common;

use common::vm_from_source;

fn vm() -> pest3_vm::Vm {
    vm_from_source(include_str!("surround.pest"), "surround.pest")
}

#[test]
fn quote() {
    let output = vm().parse("Quote", "(abc)").unwrap();
    assert_eq!(output.pairs, vec![token!(QuoteChars(1, 4))]);
}
