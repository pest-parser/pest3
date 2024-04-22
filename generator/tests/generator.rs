//! Use a script to format generated codes if changed.
//!
//! ```shell
//! cargo fmt
//! ```
use pest3_generator::typed::derive_typed_parser;
use proc_macro2::TokenStream;
use quote::quote;

fn template(path_generated: &'static str, path_expected: &'static str, input: TokenStream) {
    let actual = derive_typed_parser(input, false, false);
    let actual = actual.to_string();
    std::fs::write(path_generated, &actual).unwrap_or_else(|e| panic!("Error writing file: {}", e));
    let output = std::process::Command::new("rustfmt")
        .arg(path_generated)
        .output()
        .unwrap_or_else(|e| panic!("Error executing rustfmt: {}", e));
    assert!(
        output.status.success(),
        "STDOUT:\n{}\nSTDERR:\n{}",
        String::from_utf8(output.stdout).unwrap(),
        String::from_utf8(output.stderr).unwrap(),
    );

    if std::fs::read(path_generated).unwrap() != std::fs::read(path_expected).unwrap() {
        panic!("Generated codes have changed.")
    }
}

#[test]
fn generated() {
    template(
        "tests/generated.rs",
        "tests/expected.rs",
        quote! {
            #[grammar = "tests/syntax.pest"]
            struct Parser;
        },
    );
}

#[test]
fn generated_sample() {
    template(
        "tests/generated_sample.rs",
        "tests/expected_sample.rs",
        quote! {
            #[grammar = "../meta/tests/pest3sample.pest"]
            struct Parser;
        },
    );
}

#[test]
fn generated_import_inline() {
    template(
        "tests/generated_import_inline.rs",
        "tests/expected_import_inline.rs",
        quote! {
            #[grammar_inline = "
use tests::minimal
w  = \"w\"
x0 = minimal::x
x1 = minimal::x - w
x2 = minimal::x - minimal::x
x3 = minimal::x - \"y\"
x4 = minimal::x | \"z\"
"]
            struct Parser;
        },
    );
}

#[test]
fn generated_import_dag() {
    template(
        "tests/generated_import_dag.rs",
        "tests/expected_import_dag.rs",
        quote! {
            #[grammar_inline = "
use tests::dag::f as dag
main = dag::f
"]
            struct Parser;
        },
    );
}

#[test]
fn generated_json() {
    template(
        "tests/generated_json.rs",
        "tests/expected_json.rs",
        quote! {
            #[grammar = "../derive/tests/json.pest"]
            struct Parser;
        },
    );
}
