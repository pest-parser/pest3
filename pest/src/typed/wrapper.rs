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
