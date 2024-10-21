#![allow(non_camel_case_types)]
pub type ascii_digit = CharRange<'0', '9'>;

/// Non-zero ASCII Digit. `'1'..'9'`
#[allow(non_camel_case_types)]
pub type ascii_nonzero_digit = CharRange<'1', '9'>;

/// Binary ASCII Digit. `'0'..'1'`
#[allow(non_camel_case_types)]
pub type ascii_bin_digit = CharRange<'0', '1'>;

/// Octal ASCII Digit. `'0'..'7'`
#[allow(non_camel_case_types)]
pub type ascii_oct_digit = CharRange<'0', '7'>;

use crate::choice::{Choice2, Choice3};
/// Hexadecimal ASCII Digit. `'0'..'9' | 'a'..'f' | 'A'..'F'`
#[allow(non_camel_case_types)]
pub type ascii_hex_digit = Choice3<ascii_digit, CharRange<'a', 'f'>, CharRange<'A', 'F'>>;

/// Lower case ASCII alphabet.
#[allow(non_camel_case_types)]
pub type ascii_alpha_lower = CharRange<'a', 'z'>;

/// Upper case ASCII alphabet.
#[allow(non_camel_case_types)]
pub type ascii_alpha_upper = CharRange<'A', 'Z'>;

/// ASCII alphabet.
#[allow(non_camel_case_types)]
pub type ascii_alpha = Choice2<ascii_alpha_lower, ascii_alpha_upper>;

/// ASCII alphabet or digit.
#[allow(non_camel_case_types)]
pub type ascii_alphanumeric = Choice2<ascii_alpha, ascii_digit>;

/// ASCII alphabet.
#[allow(non_camel_case_types)]
pub type ascii = CharRange<'\x00', '\x7f'>;

/// Match char by a predicate.
///
/// Return Some(char) if matched.
pub fn match_char_by(position: &mut Position<'_>, pred: impl FnOnce(char) -> bool) -> Option<char> {
    let mut res = None;
    position.match_char_by(|c| {
        let matched = pred(c);
        if matched {
            res = Some(c);
        }
        matched
    });
    res
}

/// Restore on error.
pub fn restore_on_none<'i, T>(
    stack: &mut Stack<Span<'i>>,
    f: impl FnOnce(&mut Stack<Span<'i>>) -> Option<T>,
) -> Option<T> {
    stack.snapshot();
    let res = f(stack);
    match res {
        Some(_) => stack.clear_snapshot(),
        None => stack.restore(),
    }
    res
}
