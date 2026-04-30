mod common;

use common::{parse_error, vm_from_source};

fn vm() -> pest3_vm::Vm {
    let grammar = include_str!("../../derive/tests/grammar.pest");
    vm_from_source(grammar, "../../derive/tests/grammar.pest")
}

#[test]
fn string() {
    let output = vm().parse("string", "abc").unwrap();
    assert_eq!(output.pairs, vec![token!(string(0, 3))]);
}

#[test]
fn sequence_optional_trivia() {
    let output = vm().parse("sequence_optional_trivia", "abcabc").unwrap();
    assert_eq!(
        output.pairs,
        vec![token!(sequence_optional_trivia(0, 6, [
            string(0, 3),
            string(3, 6)
        ]))]
    );
}

#[test]
fn sequence_mandatory_trivia() {
    let output = vm().parse("sequence_mandatory_trivia", "abc   abc").unwrap();
    assert_eq!(
        output.pairs,
        vec![token!(sequence_mandatory_trivia(0, 9, [
            string(0, 3),
            string(6, 9)
        ]))]
    );
}

#[test]
fn choice_prefix() {
    let output = vm().parse("choice_prefix", "abc").unwrap();
    assert_eq!(
        output.pairs,
        vec![token!(choice_prefix(0, 3, [string(0, 3)]))]
    );
}

#[test]
fn repeat_min_max_atomic_thrice() {
    let output = vm().parse("repeat_min_max_atomic", "abcabcabc").unwrap();
    assert_eq!(
        output.pairs,
        vec![token!(repeat_min_max_atomic(0, 9, [
            string(0, 3),
            string(3, 6),
            string(6, 9)
        ]))]
    );
}

#[test]
fn repeat_max_atomic_twice() {
    let output = vm().parse("repeat_max_atomic", "abcabc").unwrap();
    assert_eq!(
        output.pairs,
        vec![token!(repeat_max_atomic(0, 6, [
            string(0, 3),
            string(3, 6)
        ]))]
    );
}

#[test]
fn repeat_comment() {
    let output = vm().parse("repeat_once", "abc$$$ $$$abc").unwrap();
    assert_eq!(
        output.pairs,
        vec![token!(repeat_once(0, 13, [
            string(0, 3),
            string(10, 13)
        ]))]
    );
}

#[test]
fn stack_resume_after_fail() {
    let output = vm().parse("stack_resume_after_fail", "a,b,c,cba").unwrap();
    assert_eq!(
        output.pairs,
        vec![token!(stack_resume_after_fail(0, 9, [repeat_mutate_stack_pop_all(0, 9)]))]
    );
}

#[test]
fn peek_slice_23() {
    let output = vm().parse("peek_slice_23", "0123412").unwrap();
    assert_eq!(
        output.pairs,
        vec![token!(peek_slice_23(0, 7, [
            range(0, 1),
            range(1, 2),
            range(2, 3),
            range(3, 4),
            range(4, 5)
        ]))]
    );
}

#[test]
fn pop_fail() {
    let output = vm().parse("pop_fail", "010").unwrap();
    assert_eq!(
        output.pairs,
        vec![token!(pop_fail(0, 3, [range(0, 1), range(1, 2)]))]
    );
}

#[test]
fn sequence_atomic_space_fails() {
    let error = parse_error(vm().parse("sequence_atomic", "abc abc").unwrap_err());
    assert_eq!(error.position, 3);
    assert!(error.expected.iter().any(|item| item == "\"abc\""));
}

#[test]
fn repeat_once_atomic_empty_fails() {
    let error = parse_error(vm().parse("repeat_once_atomic", "").unwrap_err());
    assert_eq!(error.position, 0);
    assert!(error.expected.iter().any(|item| item == "\"abc\""));
}

