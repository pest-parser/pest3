#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct Config {
    pub emit_rule_reference: bool,
    pub do_not_emit_span: bool,
    pub no_warnings: bool,
}
impl Default for Config {
    fn default() -> Self {
        Self {
            emit_rule_reference: false,
            do_not_emit_span: false,
            no_warnings: false,
        }
    }
}
