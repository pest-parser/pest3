//! Generator configuration.
//!
//! From attributes.

use std::{
    env,
    fs::read_to_string,
    path::{Path, PathBuf},
};

use syn::{Attribute, Expr, ExprLit, Lit, Meta};

#[derive(Debug, PartialEq)]
pub(crate) enum GrammarSource {
    File(String),
    Inline(String),
}
pub(crate) fn get_string(attr: &Attribute, name: &str) -> String {
    match &attr.meta {
        Meta::NameValue(name_value) => match &name_value.value {
            Expr::Lit(ExprLit {
                lit: Lit::Str(string),
                ..
            }) => string.value(),
            _ => panic!("Attribute {name} must be a string."),
        },
        _ => panic!("Attribute {name} must be of the form `{name} = \"...\"`."),
    }
}

pub(crate) fn get_bool(attr: &Attribute, flag: &str) -> bool {
    match &attr.meta {
        Meta::Path(_) => true,
        Meta::NameValue(name_value) => match &name_value.value {
            Expr::Lit(ExprLit {
                lit: Lit::Bool(val),
                ..
            }) => val.value(),
            _ => panic!("Attribute {flag} must be a boolean value."),
        },
        _ => {
            panic!("Attribute {flag} must be of the form `flag`, `flag = true` or `flag = false`.")
        }
    }
}

pub(crate) fn collect_data(contents: Vec<GrammarSource>) -> Vec<(String, Option<PathBuf>)> {
    let mut res = vec![];

    for content in contents {
        let (_data, _path) = match content {
            GrammarSource::File(ref path) => {
                let root = env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".into());

                // Check whether we can find a file at the path relative to the CARGO_MANIFEST_DIR
                // first.
                //
                // If we cannot find the expected file over there, fallback to the
                // `CARGO_MANIFEST_DIR/src`, which is the old default and kept for convenience
                // reasons.
                // TODO: This could be refactored once `std::path::absolute()` get's stabilized.
                // https://doc.rust-lang.org/std/path/fn.absolute.html
                let path = if Path::new(&root).join(path).exists() {
                    Path::new(&root).join(path)
                } else {
                    Path::new(&root).join("src/").join(path)
                };

                let file_name = match path.file_name() {
                    Some(file_name) => file_name,
                    None => panic!("grammar attribute should point to a file"),
                };

                let data = match read_to_string(&path) {
                    Ok(data) => data,
                    Err(error) => panic!("error opening {:?}: {}", file_name, error),
                };
                (data, Some(path.clone()))
            }
            GrammarSource::Inline(content) => (content, None),
        };

        res.push((_data, _path));
    }

    res
}
