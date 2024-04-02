use pest3::unicode::unicode_property_names;
use std::{fs::File, io::Write, path::PathBuf};

fn main() {
    let mut output = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    output.push("src/typed/unicode.rs");
    let mut out = File::create(output).unwrap();
    writeln!(
        out,
        "//! Wrapped types for unicode property. See [pest2::unicode] for details."
    )
    .unwrap();
    for unicode in unicode_property_names() {
        writeln!(out, "crate::unicode!({});", unicode).unwrap()
    }
}
