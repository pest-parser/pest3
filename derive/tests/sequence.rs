use pest3::typed::TypedNode;
use pest3_derive::Parser;

#[derive(Parser)]
#[grammar_inline = r#"
s1  = { "a" }
s2  = { "a" - "b" }
s3  = { "a" - "b" - "c" }
s4  = { "a" - "b" - "c" - "d" }
s5  = { "a" - "b" - "c" - "d" - "e" }
s6  = { "a" - "b" - "c" - "d" - "e" - "f" }
s7  = { "a" - "b" - "c" - "d" - "e" - "f" - "g" }
s8  = { "a" - "b" - "c" - "d" - "e" - "f" - "g" - "h" }
s9  = { "a" - "b" - "c" - "d" - "e" - "f" - "g" - "h" - "i" }
s10 = { "a" - "b" - "c" - "d" - "e" - "f" - "g" - "h" - "i" - "j" }
s11 = { "a" - "b" - "c" - "d" - "e" - "f" - "g" - "h" - "i" - "j" - "k" }
s12 = { "a" - "b" - "c" - "d" - "e" - "f" - "g" - "h" - "i" - "j" - "k" - "l" }
"#]
struct Parser;

macro_rules! test {
    ($name:ident, $input:literal, $($fields:tt)*) => {
        mod $name {
            use super::rules;
            use pest3::typed::TypedNode;

            #[test]
            fn matched() -> anyhow::Result<()> {
                let res = rules::$name::try_parse($input)?;
                assert_eq!(res, res.clone());
                Ok(())
            }
            #[test]
            fn unmatched() {
                rules::$name::try_parse(concat!("_", $input)).unwrap_err();
            }
            #[test]
            fn incomplete() {
                rules::$name::try_parse(concat!($input, "_")).unwrap_err();
            }
        }
    };
}

test!(s2, "ab", e0 e1);
test!(s3, "abc", e0 e1 e2);
test!(s4, "abcd", e0 e1 e2 e3);
test!(s5, "abcde", e0 e1 e2 e3 e4);
test!(s6, "abcdef", e0 e1 e2 e3 e4 e5);
test!(s7, "abcdefg", e0 e1 e2 e3 e4 e5 e6);
test!(s8, "abcdefgh", e0 e1 e2 e3 e4 e5 e6 e7);
test!(s9, "abcdefghi", e0 e1 e2 e3 e4 e5 e6 e7 e8);
test!(s10, "abcdefghij", e0 e1 e2 e3 e4 e5 e6 e7 e8 e9);
test!(s11, "abcdefghijk", e0 e1 e2 e3 e4 e5 e6 e7 e8 e9 e10);
test!(s12, "abcdefghijkl", e0 e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 e11);

#[test]
fn single() -> anyhow::Result<()> {
    rules::s1::try_parse("a")?;
    Ok(())
}

#[test]
fn as_ref() -> anyhow::Result<()> {
    let s4 = rules::s4::try_parse("abcd")?;
    Ok(())
}
