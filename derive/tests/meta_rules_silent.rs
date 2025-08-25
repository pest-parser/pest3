pub mod inline {
    use pest3_derive::Parser;

    #[derive(Parser)]
    #[grammar_inline = r#"
^ = "\r\n" | "\n"
field = (pest::ascii_digit | "." | "-")+
separated(e, s) = _{ e - (s - e)* }
comma_separated(e) = _{ separated(e, ",") }
record = comma_separated(field)
file = pest::soi - (record ^ "")* - pest::eoi
"#]
    pub struct ParserInline;
}

#[cfg(test)]
mod tests {
    use pest3_core::typed::TypedParser;

    use crate::inline;

    #[test]
    fn test_record_parsing() {
        let input = "123,456\n789,012\n";
        let parsed = inline::ParserInline::try_parse::<inline::rules::file>(input).expect("parse");
        let records = parsed.record();
        assert_eq!(records.len(), 2);
        assert_eq!(records[0].field().0.span.as_str(), "123");
        assert_eq!(records[0].field().1[0].span.as_str(), "456");
        assert_eq!(records[1].field().0.span.as_str(), "789");
        assert_eq!(records[1].field().1[0].span.as_str(), "012");
    }
}
