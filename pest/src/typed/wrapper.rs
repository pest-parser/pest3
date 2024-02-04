use pest2::RuleType;

/// A wrapper for string constant.
pub trait String {
    const CONTENT: &'static str;
}

/// A wrapper for string constant.
pub trait Rule<R: RuleType> {
    type Rule: RuleType;
    const RULE: R;
}

/// Bound for the length of vector.
pub trait Bound {
    /// Min length of a vector.
    const MIN: usize;
    /// Max length of a vector.
    const MAX: usize;
}
