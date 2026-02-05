#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub(crate) struct Config {
    pub no_pair: bool,
    pub no_span: bool,
    pub no_warnings: bool,
    pub box_rules_only_if_needed: bool,
    pub no_getter: bool,
    /// Name of the generated rules module. Defaults to "rules".
    pub rules_mod: Option<String>,
}
