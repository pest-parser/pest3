//! Created by [Tomas Tauber](https://github.com/tomtau).
//! Modified by [Boyi Huang](https://github.com/TheVeryDarkness).

/// Grammar rules of a sample JSON parser
#[allow(missing_docs)]
pub mod json {
    use pest_derive::Parser;

    /// JSON parser.
    #[derive(Parser)]
    #[grammar = "tests/json.pest"]
    pub struct JsonParser;
}

#[cfg(test)]
mod tests {
    use pest::typed::{PairContainer, TypedParser};

    use crate::{json, json::generics::*};
    #[test]
    fn json_basic_test() {
        let sample1 = "{\"key\": \"value\"}";
        let s1: json::rules::json = json::JsonParser::try_parse(sample1).unwrap();
        let values = s1.vec_children_pairs();
        assert_eq!(&values[0].rule, &json::Rule::value);
        let obj = &values[0].children;
        assert_eq!(obj[0].rule, json::Rule::object);
        let pair = &obj[0].children;
        assert_eq!(pair[0].rule, json::Rule::pair);
        let key = &pair[0].children;
        assert_eq!(key[0].rule, json::Rule::string);
        assert_eq!(&sample1[key[0].start..key[0].end], "\"key\"");
        let value = &key[1].children;
        assert_eq!(value[0].rule, json::Rule::string);
        assert_eq!(&sample1[value[0].start..value[0].end], "\"value\"");
    }

    /// Test in structural style.
    #[test]
    fn json_structural_test() {
        let sample1 = "{\"key\": \"value\"}";
        let s1: json::rules::json = json::JsonParser::try_parse(sample1).unwrap();
        let value: json::rules::value<'_> = s1.content.field_1;
        match *value.content {
            Choice6::Choice2(object) => {
                let (_, mut pairs, _) = object.content.into_tuple();
                assert_eq!(pairs.content.len(), 1);
                let pair = pairs.content.pop().unwrap();
                let (key, _, value) = pair.content.into_tuple();
                let (_, key, _) = key.content.into_tuple();
                assert_eq!(key.span.as_str(), "key");
                assert_eq!(value.span.as_str(), "\"value\"");
            }
            _ => {}
        }
    }
}
