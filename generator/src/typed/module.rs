use super::{
    generator::{
        ProcessedPathArgs::{Call, Slice},
        RuleRef,
    },
    output::generics,
};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use std::collections::{btree_map::Keys, BTreeMap};

#[derive(Clone, Debug)]
pub enum RuleGenerics {
    #[deprecated]
    Absolute(TokenStream),
    /// Defined rule in current module.
    Rule,
    /// Built-in rule that accepts nothing as argument.
    ///
    /// - Whether it requires lifetime specifier.
    Direct(bool),
    /// Built-in rule that accepts a range as argument.
    Slice,
    /// Built-in rule that accepts parsing expressions as arguments.
    ///
    /// - Argument count (optional).
    Callable(Option<usize>),
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
            Self::Absolute(tokens) => {
                assert!(
                    rule_ref.args.is_none(),
                    "Unexpected arguments in `{}`.",
                    rule_ref,
                );
                quote! {#root::#tokens}
            }
            Self::Rule => {
                assert!(
                    rule_ref.args.is_none(),
                    "Unexpected arguments in `{}`.",
                    rule_ref,
                );
                let (path, name) = getter();
                quote! { #root::rules::#( #path:: )* #name :: <'i> }
            }
            Self::Direct(lifetime) => {
                if false {
                    assert!(
                        rule_ref.args.is_none(),
                        "Unexpected arguments in `{}`.",
                        rule_ref,
                    );
                }
                let name = builtin_getter();
                match lifetime {
                    true => quote! { #root::#generics::#name::<'i> },
                    false => quote! { #root::#generics::#name },
                }
            }
            Self::Slice => match &rule_ref.args {
                Some(Call(_args)) => panic!("Unexpcted arguments in `{}`.", rule_ref,),
                Some(Slice(_slice)) => {
                    let name = builtin_getter();
                    quote! { #root::#generics::#name }
                }
                None => panic!("Missing arguments in `{}`.", rule_ref,),
            },
            Self::Callable(argc) => match &rule_ref.args {
                Some(Call(args)) => {
                    if let Some(argc) = argc {
                        assert_eq!(args.len(), *argc, "Argument count not matched.");
                    }
                    let name = builtin_getter();
                    quote! { #root::#generics::#name::< #( #args, )* > }
                }
                Some(Slice(_slice)) => panic!("Unexpcted slice in `{}`.", rule_ref,),
                None => panic!("Missing arguments in `{}`.", rule_ref,),
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
            ($name:ident) => {
                (
                    vec!["pest", core::stringify!($name)],
                    RuleGenerics::Direct(false),
                )
            };
            ($name:ident<'i>) => {
                (
                    vec!["pest", core::stringify!($name)],
                    RuleGenerics::Direct(true),
                )
            };
        }
        macro_rules! pest_callable {
            ($name:ident) => {
                (vec!["pest", core::stringify!($name)], RuleGenerics::Callable(None))
            };
            ($name:ident<$($T:ident),*>) => {
                (vec!["pest", core::stringify!($name)], RuleGenerics::Callable(Some([$(core::stringify!($T)),*].len())))
            };
        }
        let tree = BTreeMap::from([
            pest_direct!(SOI),
            pest_direct!(EOI),
            pest_direct!(any),
            pest_direct!(peek<'i>),
            pest_direct!(peek_all<'i>),
            pest_direct!(drop),
            pest_callable!(push<T>),
            pest_direct!(pop<'i>),
            pest_direct!(pop_all<'i>),
        ]);
        Self { tree }
    }
    pub fn insert_rule(&mut self, key: &'g str) {
        let value = RuleGenerics::Rule;
        self.tree.insert(vec![key], value);
    }
    pub fn get(&self, key: &[&'g str]) -> Option<RuleGenerics> {
        self.tree.get(key).cloned()
    }
    pub fn resolve(&self, rule_ref: &RuleRef<'g>, root: &TokenStream) -> Option<TokenStream> {
        self.tree
            .get(&rule_ref.path)
            .map(|rg| rg.call(rule_ref, root))
    }
    pub fn keys(&self) -> Keys<'_, Vec<&'g str>, RuleGenerics> {
        self.tree.keys()
    }
    pub fn contains_key(&self, key: &[&'g str]) -> bool {
        self.tree.contains_key(key)
    }
    pub fn matches_ref(&self, rule_ref: &RuleRef<'g>) -> bool {
        let mut vec = rule_ref.path.to_owned();
        self.contains_key(&vec)
    }
}
