#![allow(unused_variables)]
use anyhow::Result;
use pest3_core::typed::TypedNode as _;
use pest3_derive::Parser;

/// Test that the `rules_mod` attribute allows customizing the generated module name.
mod custom_mod_test {
    use super::*;

    #[derive(Parser)]
    #[grammar_inline = r#"
    main = "hello"
    "#]
    #[rules_mod = "custom"]
    struct ParserWithCustomMod;

    #[test]
    fn custom_module_name() -> Result<()> {
        // Access through the custom module name
        custom::main::try_parse("hello")?;
        Ok(())
    }
}

/// Test default behavior (rules module name should be "rules")
mod default_mod_test {
    use super::*;

    #[derive(Parser)]
    #[grammar_inline = r#"
    greeting = "world"
    "#]
    struct ParserWithDefaultMod;

    #[test]
    fn default_module_name() -> Result<()> {
        // Access through the default "rules" module name
        rules::greeting::try_parse("world")?;
        Ok(())
    }
}
