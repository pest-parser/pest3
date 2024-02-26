use super::{
    generator::{ProcessedPathArgs, RuleRef},
    output::generics,
};
use pest::unicode::unicode_property_names;
use pest_meta::parser::ParseRule;
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use std::{
    cell::RefCell,
    collections::{btree_map::Keys, BTreeMap, HashMap, HashSet},
    rc::Rc,
};

#[derive(Clone, Debug)]
pub enum RuleGenerics {
    /// Defined rule in current module.
    Rule {
        argc: usize,
    },
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
    pub fn call(
        &self,
        name: &str,
        rule_ref: &RuleRef<'_>,
        args: Option<&ProcessedPathArgs>,
        root: &TokenStream,
    ) -> TokenStream {
        let generics = generics();
        let name = format_ident!("r#{}", name);
        match self {
            Self::Rule { argc } => match (argc, args) {
                (0, None) => {
                    quote! { #root::rules::#name::<'i> }
                }
                (0, Some(_args)) => {
                    panic!("Unexpected arguments in `{}`.", rule_ref,);
                }
                (argc, None) => {
                    panic!("Expect {argc} arguments in `{rule_ref}`.");
                }
                (_argc, Some(ProcessedPathArgs::Slice(slice))) => {
                    panic!("Unexpected slice {slice} in `{rule_ref}`.");
                }
                (argc, Some(ProcessedPathArgs::Call(args))) => {
                    assert_eq!(
                        *argc,
                        args.len(),
                        "Argument count not matched in `{}`.",
                        rule_ref
                    );
                    quote! { #root::rules::#name::<'i, #(#args, )*> }
                }
            },
            Self::Unicode(ident) => {
                quote! { #root::rules::unicode::#ident }
            }
            Self::BuiltIn {
                direct,
                slice,
                callable,
            } => match args {
                None => {
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
                    quote! { #root::#generics::#name::< #( #args, )* > }
                }
            },
        }
    }
}

#[derive(Clone, Debug)]
enum ModuleNode<'g> {
    // MutableCollection(RefCell<HashMap<&'g str, Rc<ModuleNode<'g>>>>),
    Collection(HashMap<&'g str, Rc<ModuleNode<'g>>>),
    Map {
        bound: HashSet<String>,
        mapper: fn(&'g str) -> TokenStream,
    },
    Generics(RuleGenerics),
    Imported(Rc<Self>),
}

impl<'g> ModuleNode<'g> {
    /// [Err] here is actually not an error.
    fn child(&self, node: &'g str) -> &Self {
        match self {
            Self::Collection(map) => map.get(node).unwrap_or_else(|| {
                panic!(
                    "Node {} can't be resolved. Available nodes are {:?}.",
                    node,
                    map.keys()
                )
            }),
            Self::Map { bound, mapper } => {
                assert!(bound.contains(node), "{node} is out of bound.");
                panic!("No child found.")
            }
            Self::Generics(_generics) => panic!("No child found."),
            Self::Imported(rc) => rc.child(node),
        }
    }
    fn get(
        &self,
        root: &TokenStream,
        node: &'g str,
        rule_ref: &RuleRef<'g>,
        args: &Option<ProcessedPathArgs>,
    ) -> TokenStream {
        match self {
            Self::Collection(map) => panic!("Collection can't be called."),
            Self::Map { bound, mapper } => {
                assert!(args.is_none(), "Unexpected arguments.");
                mapper(node)
            }
            Self::Generics(generics) => generics.call(node, rule_ref, args.as_ref(), root),
            Self::Imported(rc) => rc.get(root, node, rule_ref, args),
        }
    }
}

/// Module system for pest3.
///
/// `'g` stands for the lifetime of grammar file content.
#[derive(Clone, Debug)]
pub struct ModuleSystem<'g> {
    root: ModuleNode<'g>,
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
                    core::stringify!($name),
                    Rc::new(
                        ModuleNode::Generics(
                            RuleGenerics::BuiltIn {
                                direct: None $( .or(Some(pest_direct! { $($direct)* })) )?,
                                slice: None $( .or(Some(pest_slice! { $( $slice )* })) )?,
                                callable: None $( .or(Some(pest_callable! { $( $callable )* })) )?,
                            },
                        )
                    )
                )
            };
        }
        let pest_stack = HashMap::from([
            pest_builtin!(peek {'i} [PeekSlice1, PeekSlice2]),
            pest_builtin!(peek_all {'i}),
            pest_builtin!(drop {}),
            pest_builtin!(push(T)),
            pest_builtin!(pop {'i}),
            pest_builtin!(pop_all {'i}),
        ]);
        let bound = HashSet::from_iter(unicode_property_names().map(|unicode| {
            assert!(unicode.is_ascii());
            // let ident = format_ident!("{}", unicode.to_ascii_lowercase());
            unicode.to_ascii_lowercase()
        }));
        let mapper = |s: &'g str| {
            let ident = format_ident!("{}", s.to_ascii_uppercase());
            quote! {#ident}
        };
        let pest_unicode = ModuleNode::Map { bound, mapper };
        let pest = HashMap::from([
            pest_builtin!(SOI {}),
            pest_builtin!(EOI {}),
            pest_builtin!(soi {}),
            pest_builtin!(eoi {}),
            pest_builtin!(any {}),
            ("stack", Rc::new(ModuleNode::Collection(pest_stack))),
            ("unicode", Rc::new(pest_unicode)),
        ]);
        let root = ModuleNode::Collection(HashMap::from([(
            "pest",
            Rc::new(ModuleNode::Collection(pest)),
        )]));

        Self { root }
    }
    pub fn insert_rule(&mut self, rule: &'g ParseRule) {
        let key = &rule.name;
        let argc = rule.args.len();
        let value = RuleGenerics::Rule { argc };
        if let ModuleNode::Collection(root) = &mut self.root {
            root.insert(key, Rc::new(ModuleNode::Generics(value)));
        }
    }
    pub fn resolve(
        &self,
        rule: &'g ParseRule,
        rule_ref: &RuleRef<'g>,
        root: &TokenStream,
    ) -> TokenStream {
        // Rule generics arguments.
        if let None = rule_ref.args {
            if rule_ref.path.len() == 1 {
                let arg = rule.args.iter().find(|arg| arg == &rule_ref.path[0]);
                if let Some(arg) = arg {
                    let ident = format_ident!("r#{}", arg);
                    return quote! {#ident};
                }
            }
        }
        let mut rc = &self.root;
        for node in &rule_ref.path {
            rc = rc.child(node);
        }
        rc.get(
            root,
            rule_ref.path.last().unwrap(),
            rule_ref,
            &rule_ref.args,
        )
    }
}
