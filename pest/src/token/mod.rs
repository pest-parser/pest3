//! Definition of tokens.

/// Matched token. Not optimized.
///
/// See [Token](pest2::Token) and [Pair](pest2::iterators::Pair).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Pair<R> {
    pub rule: R,
    pub start: usize,
    pub end: usize,
    pub children: Vec<Self>,
}
