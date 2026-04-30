#![allow(unused_variables)]

use anyhow::Result;
use pest3_derive::Parser;

mod basic_events {
    use super::*;

    #[derive(Parser)]
    #[grammar_inline = r#"
    main = lhs - rhs
    lhs = "a"
    rhs = "b"
    "#]
    struct EventParser;

    #[derive(Default)]
    struct Recorder(Vec<&'static str>);

    impl<'i> EventParserEventObserver<'i> for Recorder {
        fn on_main_start(&mut self, pair: &pest3_core::token::Pair<Rule>, input: &'i str) {
            self.0.push("main:start");
        }

        fn on_main_end(&mut self, pair: &pest3_core::token::Pair<Rule>, input: &'i str) {
            self.0.push("main:end");
        }

        fn on_lhs_start(&mut self, pair: &pest3_core::token::Pair<Rule>, input: &'i str) {
            assert_eq!(&input[pair.start..pair.end], "a");
            self.0.push("lhs:start");
        }

        fn on_lhs_end(&mut self, pair: &pest3_core::token::Pair<Rule>, input: &'i str) {
            self.0.push("lhs:end");
        }

        fn on_rhs_start(&mut self, pair: &pest3_core::token::Pair<Rule>, input: &'i str) {
            assert_eq!(&input[pair.start..pair.end], "b");
            self.0.push("rhs:start");
        }

        fn on_rhs_end(&mut self, pair: &pest3_core::token::Pair<Rule>, input: &'i str) {
            self.0.push("rhs:end");
        }
    }

    impl<'i> EventParserEventProcessor<'i> for Recorder {
        type Output = Vec<&'static str>;

        fn finalize(self) -> Self::Output {
            self.0
        }
    }

    #[test]
    fn parse_into_emits_start_and_end_events() -> Result<()> {
        let mut recorder = Recorder::default();
        EventParser::parse_into(Rule::r#main, &mut recorder, "ab")?;
        assert_eq!(
            recorder.0,
            vec![
                "main:start",
                "lhs:start",
                "lhs:end",
                "rhs:start",
                "rhs:end",
                "main:end",
            ]
        );
        Ok(())
    }

    #[test]
    fn parse_finalizes_owned_processor() -> Result<()> {
        let events = EventParser::parse(Rule::r#main, Recorder::default(), "ab")?;
        assert_eq!(
            events,
            vec![
                "main:start",
                "lhs:start",
                "lhs:end",
                "rhs:start",
                "rhs:end",
                "main:end",
            ]
        );
        Ok(())
    }
}

mod custom_rules_module_events {
    use super::*;
    use pest3_core::typed::TypedNode as _;

    #[derive(Parser)]
    #[grammar_inline = r#"
    main = child
    child = "x"
    "#]
    #[rules_mod = "custom"]
    struct CustomEventParser;

    #[derive(Default)]
    struct Recorder(Vec<&'static str>);

    impl<'i> CustomEventParserEventObserver<'i> for Recorder {
        fn on_main_start(&mut self, pair: &pest3_core::token::Pair<Rule>, input: &'i str) {
            self.0.push("main:start");
        }

        fn on_child_end(&mut self, pair: &pest3_core::token::Pair<Rule>, input: &'i str) {
            self.0.push("child:end");
        }
    }

    #[test]
    fn event_api_respects_custom_rules_module_generation() -> Result<()> {
        custom::main::try_parse("x")?;

        let mut recorder = Recorder::default();
        CustomEventParser::parse_into(Rule::r#main, &mut recorder, "x")?;
        assert_eq!(recorder.0, vec!["main:start", "child:end"]);
        Ok(())
    }
}
