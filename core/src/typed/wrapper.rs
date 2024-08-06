//! Wrapper.
use super::RuleType;

/// A wrapper for string constant.
pub trait String {
    /// String content.
    const CONTENT: &'static str;
    /// Get content.
    fn get_const(&self) -> &'static str {
        Self::CONTENT
    }
}

/// A wrapper for string constant.
pub trait Rule {
    /// Should be the type of wrapped rule.
    type Rule: RuleType;
    /// Wrapped rule.
    const RULE: Self::Rule;
}

/// Bound for the length of vector.
pub trait Bound {
    /// Min length of a vector.
    const MIN: usize;
    /// Max length of a vector.
    const MAX: usize;
}
