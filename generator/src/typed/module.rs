use super::{
    generator::{Intermediate, ProcessedPathArgs, RuleRef},
    output::generics,
};
use pest3_meta::parser::ParseRule;
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use std::{collections::HashMap, rc::Rc};

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
    ) -> Intermediate {
        let generics = generics();
        let name = format_ident!("r#{}", name);
        match self {
            Self::Rule { argc } => match (argc, args) {
                (0, None) => {
                    let typename = quote! { #root::rules::#name::<'i> };
                    Intermediate { typename }
                }
                (0, Some(ProcessedPathArgs::Call(args))) => {
                    if args.len() > 0 {
                        panic!("Unexpected arguments in `{}`.", rule_ref,);
                    } else {
                        let typename = quote! { #root::rules::#name::<'i> };
                        Intermediate { typename }
                    }
                }
                (0, Some(ProcessedPathArgs::Slice(slice))) => {
                    panic!("Unexpected slice argument {} in `{}`.", slice, rule_ref,);
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
                    let typename = quote! { #root::rules::#name::<'i, #(#args, )*> };
                    Intermediate { typename }
                }
            },
            Self::Unicode(ident) => {
                let typename = quote! { #root::rules::unicode::#ident };
                Intermediate { typename }
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
                    let typename = match lifetime {
                        true => quote! { #root::#generics::#name::<'i> },
                        false => quote! { #root::#generics::#name },
                    };
                    Intermediate { typename }
                }
                Some(ProcessedPathArgs::Slice(range)) => {
                    let (slice1, slice2) = slice.as_ref().expect(&format!(
                        "Built-in rule {rule_ref} can't be called without arguments."
                    ));
                    let start = range.start.unwrap_or(0);
                    let end = range.end;
                    let typename = match end {
                        Some(end) => quote! { #root::#generics::#slice2::<#start, #end> },
                        None => quote! { #root::#generics::#slice1::<#start> },
                    };
                    Intermediate { typename }
                }
                Some(ProcessedPathArgs::Call(args)) => {
                    let (argc,) = callable.expect(&format!(
                        "Built-in rule {rule_ref} can't be called without arguments."
                    ));
                    if let Some(argc) = argc {
                        assert_eq!(args.len(), argc, "Argument count not matched.");
                    }
                    let typename = quote! { #root::#generics::#name::< #( #args, )* > };
                    Intermediate { typename }
                }
            },
        }
    }
}

#[derive(Clone)]
enum ModuleNode<'g> {
    Collection(HashMap<&'g str, Rc<ModuleNode<'g>>>),
    Generics(RuleGenerics),
    Absolute(Rc<dyn Fn(&TokenStream) -> Intermediate>),
    Imported(Rc<Self>),
}

impl<'g> ModuleNode<'g> {
    /// [Err] here is actually not an error.
    fn child(&self, node: &'g str) -> Rc<Self> {
        match self {
            Self::Collection(map) => map
                .get(node)
                .unwrap_or_else(|| {
                    panic!(
                        "Node {} can't be resolved. Available nodes are {:?}.",
                        node,
                        map.keys()
                    )
                })
                .clone(),
            Self::Absolute(f) => panic!("No child found."),
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
    ) -> Intermediate {
        match self {
            Self::Collection(map) => panic!("Collection can't be called."),
            Self::Absolute(f) => {
                let empty = match args {
                    None => true,
                    Some(ProcessedPathArgs::Call(args)) => args.is_empty(),
                    _ => false,
                };
                assert!(empty);
                f(root)
            }
            Self::Generics(generics) => generics.call(node, rule_ref, args.as_ref(), root),
            Self::Imported(rc) => rc.get(root, node, rule_ref, args),
        }
    }
}

/// Module system for pest3.
///
/// `'g` stands for the lifetime of grammar file content.
#[derive(Clone)]
pub struct ModuleSystem<'g> {
    root: ModuleNode<'g>,
}

impl<'g> ModuleSystem<'g> {
    pub fn new(unicode_names: &'g [(impl AsRef<str>, impl AsRef<str>)]) -> Self {
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
        let pest_unicode = ModuleNode::Collection(
            unicode_names
                .iter()
                .map(|unicode| {
                    let (lower, upper) = unicode;
                    let lower = lower.as_ref();
                    let upper = upper.as_ref();

                    let name = lower;
                    let ident = format_ident!("{}", upper);
                    let node = ModuleNode::Absolute(Rc::new(move |root| {
                        let typename = quote! { #root::unicode::#ident};
                        Intermediate { typename }
                    }));
                    let node = Rc::new(node);
                    (name, node)
                })
                .collect(),
        );
        let pest = HashMap::from([
            pest_builtin!(SOI {}),
            pest_builtin!(EOI {}),
            pest_builtin!(soi {}),
            pest_builtin!(eoi {}),
            pest_builtin!(any {}),
            // ASCII.
            pest_builtin!(ascii_digit {}),
            pest_builtin!(ascii_nonzero_digit {}),
            pest_builtin!(ascii_bin_digit {}),
            pest_builtin!(ascii_oct_digit {}),
            pest_builtin!(ascii_hex_digit {}),
            pest_builtin!(ascii_alpha_lower {}),
            pest_builtin!(ascii_alpha_upper {}),
            pest_builtin!(ascii_alpha {}),
            pest_builtin!(ascii_alphanumeric {}),
            pest_builtin!(ascii {}),
            pest_builtin!(newline {}),
            // Submodule.
            ("stack", Rc::new(ModuleNode::Collection(pest_stack))),
            ("unicode", Rc::new(pest_unicode)),
        ]);
        // FIXME: make "pest" paths to refer to pest3 crate for the moment
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
    fn resolve_node(&self, mut iter: impl Iterator<Item = &'g str>) -> Rc<ModuleNode<'g>> {
        let r = &self.root;
        let first = iter.next().expect("Empty path.");
        let mut rc = r.child(first);
        for node in iter {
            rc = rc.child(node);
        }
        rc
    }
    pub fn resolve(&self, rule_ref: &RuleRef<'g>, root: &TokenStream) -> Intermediate {
        let rc = self.resolve_node(rule_ref.path.iter().cloned());
        rc.get(
            root,
            rule_ref.path.last().unwrap(),
            rule_ref,
            &rule_ref.args,
        )
    }
    pub fn alias(&mut self, source: impl Iterator<Item = &'g str>, name: &'g str) {
        let node = self.resolve_node(source);
        match &mut self.root {
            ModuleNode::Collection(collection) => {
                let res = collection.insert(name, node);
                assert!(res.is_none(), "node {name:?} already defined.")
            }
            _ => unreachable!(),
        }
    }
}
