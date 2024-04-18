#[derive(Clone, Copy, Default, Debug, Eq, PartialEq)]
pub(crate) struct Config {
    pub no_pair: bool,
    pub no_span: bool,
    pub no_warnings: bool,
    pub box_rules_only_if_needed: bool,
}
