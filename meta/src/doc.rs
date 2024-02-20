use std::collections::HashMap;

#[derive(Debug, Default)]
pub struct DocComment {
    /// Document comments for the whole grammar file.
    pub grammar_doc: Vec<String>,
}
