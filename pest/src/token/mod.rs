//! Definition of tokens.

use std::fmt::Display;

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
impl<R: crate::RuleType> Display for Pair<R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}({}, {}, [", self.rule, self.start, self.end,)?;
        let mut iter = self.children.iter();
        if let Some(first) = iter.next() {
            write!(f, "{}", first)?;
        }
        for item in iter {
            write!(f, ", {}", item)?;
        }
        write!(f, "])")?;
        Ok(())
    }
}
