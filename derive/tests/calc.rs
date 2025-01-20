/// Grammar rules of a sample calculator parser
#[allow(missing_docs)]
pub mod calc {
    use pest3_derive::Parser;

    /// Calculator parser.
    #[derive(Parser)]
    #[grammar = "tests/calc.pest"]
    pub struct CalcParser;
}

#[cfg(test)]
mod tests {
    use pest3_core::typed::TypedParser;

    use crate::calc;

    #[test]
    fn calc_invalid_expression() {
        let parse_result: Result<calc::rules::program, _> = calc::CalcParser::try_parse("3 + * 4");
        assert!(parse_result.is_err());
    }

    #[test]
    fn calc_basic_test() {
        let sample1 = "3 + 4 * 5";
        let s1: calc::rules::program = calc::CalcParser::try_parse(sample1).unwrap();
        let expr = s1.expr();
        let (primary, rest) = expr.primary();
        dbg!(primary);
        assert_eq!(primary.span.as_str(), "3");
        let second: &calc::rules::primary<'_> = rest[0];
        let third: &calc::rules::primary<'_> = rest[1];
        dbg!(rest);
        assert_eq!(second.span.as_str(), "4");
        assert_eq!(third.span.as_str(), "5");
        let op = expr.infix();
        assert_eq!(op.len(), 2);
        assert_eq!(op[0].span.as_str(), "+");
        assert_eq!(op[1].span.as_str(), "*");
    }
}
