#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct Config {
    pub emit_rule_reference: bool,
    pub do_not_emit_span: bool,
    pub no_warnings: bool,
    pub box_all_rules: bool,
}
impl Default for Config {
    fn default() -> Self {
        Self {
            emit_rule_reference: false,
            do_not_emit_span: false,
            no_warnings: false,
            box_all_rules: false,
        }
    }
}
