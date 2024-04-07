//! Definition of tokens.

use std::fmt::Display;

/// Matched token. Not optimized.
///
/// See [Token](pest2::Token) and [Pair](pest2::iterators::Pair).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Pair<R> {
    /// Rule.
    pub rule: R,
    /// Start position in code points.
    pub start: usize,
    /// End position in code points.
    pub end: usize,
    /// Children pairs.
    pub children: Vec<Self>,
}
impl<R: Clone> Pair<R> {
    /// Convert to a [Pair] with another rule type.
    pub fn convert<S>(&self, converter: &impl Fn(R) -> S) -> Pair<S> {
        let Self {
            rule,
            start,
            end,
            children,
        } = self;
        let rule = converter(rule.clone());
        let start = *start;
        let end = *end;
        let children = children
            .iter()
            .map(|pair| pair.convert(converter))
            .collect();
        Pair {
            rule,
            start,
            end,
            children,
        }
    }
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
