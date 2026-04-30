mod common;

use common::vm_from_source;

fn vm() -> pest3_vm::Vm {
    vm_from_source(include_str!("lists.pest"), "lists.pest")
}

#[test]
fn item() {
    let output = vm().parse("lists", "- a").unwrap();
    assert_eq!(output.pairs, vec![token!(item(2, 3))]);
}

#[test]
fn items() {
    let output = vm().parse("lists", "- a\n- b").unwrap();
    assert_eq!(output.pairs, vec![token!(item(2, 3)), token!(item(6, 7))]);
}

#[test]
fn children() {
    let output = vm().parse("children", "  - b").unwrap();
    assert_eq!(
        output.pairs,
        vec![token!(children(0, 5, [item(4, 5)]))]
    );
}

#[test]
fn nested_item() {
    let output = vm().parse("lists", "- a\n  - b").unwrap();
    assert_eq!(
        output.pairs,
        vec![token!(item(2, 3)), token!(children(4, 9, [item(8, 9)]))]
    );
}

#[test]
fn nested_items() {
    let output = vm().parse("lists", "- a\n  - b\n  - c").unwrap();
    assert_eq!(
        output.pairs,
        vec![
            token!(item(2, 3)),
            token!(children(4, 15, [item(8, 9), item(14, 15)]))
        ]
    );
}

#[test]
fn nested_two_levels() {
    let output = vm().parse("lists", "- a\n  - b\n    - c").unwrap();
    assert_eq!(
        output.pairs,
        vec![
            token!(item(2, 3)),
            token!(children(4, 17, [
                item(8, 9),
                children(10, 17, [item(16, 17)])
            ]))
        ]
    );
}

#[test]
fn nested_then_not() {
    let output = vm().parse("lists", "- a\n  - b\n- c").unwrap();
    assert_eq!(
        output.pairs,
        vec![
            token!(item(2, 3)),
            token!(children(4, 9, [item(8, 9)])),
            token!(item(12, 13))
        ]
    );
}

