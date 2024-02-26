use pest_generator::typed::derive_typed_parser;
use quote::quote;

/// Use a script to format generated codes if changed.
///
/// ```shell
/// rustfmt generator/tests/generated.txt
/// ```
#[test]
fn generated() {
    let path_generated = "tests/generated.rs";
    let path_expected = "tests/expected.rs";
    let actual = derive_typed_parser(
        quote! {
            #[grammar = "tests/syntax.pest"]
            struct Parser;
        },
        false,
        false,
    );
    let actual = actual.to_string();
    std::fs::write(path_generated, &actual).unwrap();
    let output = std::process::Command::new("rustfmt")
        .arg(path_generated)
        .output()
        .unwrap();
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
fn generated_sample() {
    let path_generated = "tests/generated_sample.rs";
    let path_expected = "tests/expected_sample.rs";
    let actual = derive_typed_parser(
        quote! {
            #[grammar = "../meta/tests/pest3sample.pest"]
            struct Parser;
        },
        false,
        false,
    );
    let actual = actual.to_string();
    std::fs::write(path_generated, &actual).unwrap();
    let output = std::process::Command::new("rustfmt")
        .arg(path_generated)
        .output()
        .unwrap();
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
