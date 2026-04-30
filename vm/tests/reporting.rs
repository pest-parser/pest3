mod common;

use common::{parse_error, vm_from_source};

fn vm() -> pest3_vm::Vm {
    vm_from_source(include_str!("reporting.pest"), "reporting.pest")
}

fn assert_failure(rule: &str, input: &str, position: usize, expected: &[&str]) {
    let error = parse_error(vm().parse(rule, input).unwrap_err());
    assert_eq!(error.position, position);
    for item in expected {
        assert!(
            error.expected.iter().any(|actual| actual == item),
            "missing expected item {item:?} in {:?}",
            error.expected
        );
    }
}

#[test]
fn choices() {
    assert_failure("choices", "x", 0, &["\"a\"", "\"b\"", "\"c\""]);
}

#[test]
fn choices_no_progress() {
    assert_failure("choices_no_progress", "x", 0, &["\"a\"", "\"b\"", "\"c\""]);
}

#[test]
fn choices_a_progress() {
    assert_failure("choices_a_progress", "a", 1, &["\"a\""]);
}

#[test]
fn choices_b_progress() {
    assert_failure("choices_b_progress", "b", 1, &["\"b\""]);
}

#[test]
fn nested() {
    assert_failure("level1", "x", 0, &["\"a\"", "\"b\"", "\"c\""]);
}

#[test]
fn negative() {
    assert_failure("negative", "x", 0, &["negative"]);
}

#[test]
fn negative_match() {
    assert_failure("negative_match", "x", 0, &["\"b\""]);
}

#[test]
fn mixed() {
    assert_failure("mixed", "x", 0, &["\"a\""]);
}

#[test]
fn mixed_progress() {
    assert_failure("mixed_progress", "b", 1, &["\"a\""]);
}

