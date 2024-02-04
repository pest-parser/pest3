use std::collections::HashMap;

#[derive(Debug, Default)]
pub struct DocComment {
    pub grammar_doc: Option<String>,

    /// HashMap for store all doc_comments for rules.
    /// key is rule name, value is doc_comment.
    pub line_docs: HashMap<String, String>,
}
