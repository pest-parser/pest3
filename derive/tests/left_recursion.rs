use pest3_core::typed::TypedNode;
use pest3_derive::Parser;

mod direct {
    use super::*;

    #[derive(Parser)]
    #[rules_mod = "direct_rules"]
    #[grammar_inline = r#"
expr = @ { expr - "+" - num | num }
num = ('0'..'9')+
"#]
    pub struct Parser;
}

mod mutual {
    use super::*;

    #[derive(Parser)]
    #[rules_mod = "mutual_rules"]
    #[grammar_inline = r#"
primary = @ { call | ident }
call = @ { primary - args }
ident = ('a'..'z')+
args = "(" - ")"
"#]
    pub struct Parser;
}

#[test]
fn direct_left_recursion() -> anyhow::Result<()> {
    let expr = direct::direct_rules::expr::try_parse("1+2+3")?;
    assert_eq!(expr.span.as_str(), "1+2+3");
    Ok(())
}

#[test]
fn mutual_left_recursion() -> anyhow::Result<()> {
    let primary = mutual::mutual_rules::primary::try_parse("f()()")?;
    assert_eq!(primary.span.as_str(), "f()()");
    Ok(())
}
