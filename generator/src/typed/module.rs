use super::{
    generator::{ProcessedPathArgs, RuleRef},
    output::generics,
};
use pest::unicode::unicode_property_names;
use pest_meta::parser::ParseRule;
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use std::collections::{btree_map::Keys, BTreeMap};

#[derive(Clone, Debug)]
pub enum RuleGenerics {
    /// Defined rule in current module.
    Rule,
    Unicode(Ident),
    BuiltIn {
        /// Built-in rule that accepts nothing as argument.
        ///
        /// - Whether it requires lifetime specifier.
        direct: Option<(bool,)>,
        /// Built-in rule that accepts a range as argument.
        ///
        /// - Slice 1.
        /// - Slice 2.
        slice: Option<(Ident, Ident)>,
        /// Built-in rule that accepts parsing expressions as arguments.
        ///
        /// - Argument count (optional).
        callable: Option<(Option<usize>,)>,
    },
}
impl RuleGenerics {
    pub fn call(&self, rule_ref: &RuleRef<'_>, root: &TokenStream) -> TokenStream {
        let getter = || {
            let mut path = rule_ref.path.iter().map(|s| format_ident!("{}", s));
            let name = path.next_back().unwrap();
            (path, name)
        };
        let builtin_getter = || {
            assert_eq!(rule_ref.path.len(), 2);
            let name = rule_ref.path.last().unwrap();
            let name = format_ident!("r#{}", name);
            name
        };
        let generics = generics();
        match self {
            Self::Rule => {
                assert!(
                    rule_ref.args.is_none(),
                    "Unexpected arguments in `{}`.",
                    rule_ref,
                );
                let (path, name) = getter();
                quote! { #root::rules::#( #path:: )* #name :: <'i> }
            }
            Self::Unicode(ident) => {
                quote! { #root::rules::unicode::#ident }
            }
            Self::BuiltIn {
                direct,
                slice,
                callable,
            } => match &rule_ref.args {
                None => {
                    let name = builtin_getter();
                    let (lifetime,) = direct.expect(&format!(
                        "Built-in rule {rule_ref} can't be called without arguments."
                    ));
                    match lifetime {
                        true => quote! { #root::#generics::#name::<'i> },
                        false => quote! { #root::#generics::#name },
                    }
                }
                Some(ProcessedPathArgs::Slice(range)) => {
                    let (slice1, slice2) = slice.as_ref().expect(&format!(
                        "Built-in rule {rule_ref} can't be called without arguments."
                    ));
                    let start = range.start.unwrap_or(0);
                    let end = range.end;
                    match end {
                        Some(end) => quote! { #root::#generics::#slice2::<#start, #end> },
                        None => quote! { #root::#generics::#slice1::<#start> },
                    }
                }
                Some(ProcessedPathArgs::Call(args)) => {
                    let (argc,) = callable.expect(&format!(
                        "Built-in rule {rule_ref} can't be called without arguments."
                    ));
                    if let Some(argc) = argc {
                        assert_eq!(args.len(), argc, "Argument count not matched.");
                    }
                    let name = builtin_getter();
                    quote! { #root::#generics::#name::< #( #args, )* > }
                }
            },
        }
    }
}

/// Module system for pest3.
///
/// `'g` stands for the lifetime of grammar file content.
#[derive(Clone, Debug)]
pub struct ModuleSystem<'g> {
    /// Path -> Rule.
    tree: BTreeMap<Vec<&'g str>, RuleGenerics>,
}

impl<'g> ModuleSystem<'g> {
    pub fn new() -> Self {
        macro_rules! pest_direct {
            () => {
                (false,)
            };
            ('i) => {
                (true,)
            };
        }
        macro_rules! pest_slice {
            ($($T:ident),*) => {
                ($( format_ident!("{}", core::stringify!($T)) ),*)
            };
        }
        macro_rules! pest_callable {
            ($($T:ident),*) => {
                (Some([$(core::stringify!($T)),*].len()), )
            };
        }
        macro_rules! pest_builtin {
            ( $name:ident $( {$($direct:tt)*} )? $( [$($slice:tt)*] )? $( ($($callable:tt)*) )? ) => {
                (
                    vec!["pest", core::stringify!($name)],
                    RuleGenerics::BuiltIn {
                        direct: None $( .or(Some(pest_direct! { $($direct)* })) )?,
                        slice: None $( .or(Some(pest_slice! { $( $slice )* })) )?,
                        callable: None $( .or(Some(pest_callable! { $( $callable )* })) )?,
                    },
                )
            };
        }
        let mut tree = BTreeMap::from([
            pest_builtin!(SOI {}),
            pest_builtin!(EOI {}),
            pest_builtin!(any {}),
            pest_builtin!(peek {'i} [PeekSlice1, PeekSlice2]),
            pest_builtin!(peek_all {'i}),
            pest_builtin!(drop {}),
            pest_builtin!(push(T)),
            pest_builtin!(pop {'i}),
            pest_builtin!(pop_all {'i}),
        ]);
        for unicode in unicode_property_names() {
            assert!(unicode.is_ascii());
            let ident = format_ident!("{}", unicode.to_ascii_lowercase());
            tree.insert(
                vec!["pest", "unicode", unicode],
                RuleGenerics::Unicode(ident),
            );
        }
        Self { tree }
    }
    pub fn insert_rule(&mut self, rule: &'g ParseRule) {
        let key = &rule.name;
        let value = RuleGenerics::Rule;
        self.tree.insert(vec![key], value);
    }
    pub fn resolve(&self, rule_ref: &RuleRef<'g>, root: &TokenStream) -> Option<TokenStream> {
        self.tree
            .get(&rule_ref.path)
            .map(|rg| rg.call(rule_ref, root))
    }
    pub fn keys(&self) -> Keys<'_, Vec<&'g str>, RuleGenerics> {
        self.tree.keys()
    }
}
