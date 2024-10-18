use super::{
    generator::{Intermediate, ProcessedPathArgs, RuleRef},
    getter::GetterByName,
    output::generics,
};
use pest3_core::unicode::unicode_property_names;
use pest3_meta::parser::{ParseRule, Range};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use std::{collections::HashMap, rc::Rc, sync::OnceLock};

/// TODO: Implement [Display](std::fmt::Display) for this.
#[derive(Clone, Debug)]
pub enum ModuleError<'g> {
    EmptyPath,
    NodeNotFound(String, Vec<&'g str>),
    AlreadyDefined(&'g str),
    NotMutable,
    NoChildren,
    UnexpectedArguments(RuleRef<'g>),
    UnexpectedSliceArgument(Range<isize>, RuleRef<'g>),
    ArgumentCountNotMatch(RuleRef<'g>),
    CollectionCannotBeCalled(RuleRef<'g>),
    ExpectedArgcArguments(usize, RuleRef<'g>),
    ExpectedArguments(RuleRef<'g>),
}
impl<'g> ModuleError<'g> {
    fn check_argc(argc: usize, args: usize, rule_ref: &RuleRef<'g>) -> Result<(), Self> {
        if argc != args {
            Err(Self::ArgumentCountNotMatch(rule_ref.clone()))
        } else {
            Ok(())
        }
    }
}

#[derive(Clone, Debug)]
pub enum RuleGenerics {
    /// Defined rule in current module.
    Rule { argc: usize },
    BuiltIn {
        /// Built-in rule that accepts nothing as argument.
        ///
        /// - Whether it requires lifetime specifier.
        direct: Option<(bool,)>,
        /// Built-in rule that accepts a range as argument.
        ///
        /// - Slice 1.
        /// - Slice 2.
        slice: Option<(&'static str, &'static str)>,
        /// Built-in rule that accepts parsing expressions as arguments.
        ///
        /// - Argument count (optional).
        callable: Option<(Option<usize>,)>,
    },
}
impl RuleGenerics {
    pub fn call<'g>(
        &self,
        name_str: &'g str,
        rule_ref: &RuleRef<'g>,
        args: Option<&ProcessedPathArgs<'g>>,
        root: &TokenStream,
    ) -> Result<Intermediate<'g>, ModuleError<'g>> {
        let generics = generics();
        let name = format_ident!("r#{}", name_str);
        let current = rule_ref.path[0..rule_ref.path.len() - 1].iter().map(|sec| {
            let sec = format_ident!("{}", sec);
            quote! {#sec::rules::}
        });
        let res = match self {
            Self::Rule { argc } => match (argc, args) {
                (0, None) => {
                    let typename = quote! { #(#current)* #name::<'i> };
                    let getter = GetterByName::from_rule(&typename, name_str);
                    Intermediate { typename, getter }
                }
                (0, Some(ProcessedPathArgs::Call(args))) => {
                    if !args.is_empty() {
                        Err(ModuleError::UnexpectedArguments(rule_ref.clone()))?
                    } else {
                        let typename = quote! { #(#current)* #name::<'i> };
                        let getter = GetterByName::from_rule(&typename, name_str);
                        Intermediate { typename, getter }
                    }
                }
                (0, Some(ProcessedPathArgs::Slice(slice))) => Err(
                    ModuleError::UnexpectedSliceArgument(slice.clone(), rule_ref.clone()),
                )?,
                (argc, None) => Err(ModuleError::ExpectedArgcArguments(*argc, rule_ref.clone()))?,
                (_argc, Some(ProcessedPathArgs::Slice(slice))) => Err(
                    ModuleError::UnexpectedSliceArgument(slice.clone(), rule_ref.clone()),
                )?,
                (argc, Some(ProcessedPathArgs::Call(args))) => {
                    ModuleError::check_argc(*argc, args.len(), rule_ref)?;
                    let typename = quote! { #(#current)* #name::<'i, #(#args, )*> };
                    let getter = GetterByName::from_rule(&typename, name_str);
                    Intermediate { typename, getter }
                }
            },
            Self::BuiltIn {
                direct,
                slice,
                callable,
            } => match args {
                None => {
                    let (lifetime,) =
                        direct.ok_or_else(|| ModuleError::ExpectedArguments(rule_ref.clone()))?;
                    let typename = match lifetime {
                        true => quote! { #root::#generics::#name::<'i> },
                        false => quote! { #root::#generics::#name },
                    };
                    let getter = GetterByName::from_rule(&typename, name_str);
                    Intermediate { typename, getter }
                }
                Some(ProcessedPathArgs::Slice(range)) => {
                    let (slice1, slice2) = slice
                        .as_ref()
                        .ok_or_else(|| ModuleError::ExpectedArguments(rule_ref.clone()))?;
                    let slice1 = format_ident!("{}", slice1);
                    let slice2 = format_ident!("{}", slice2);
                    let start = range.start.unwrap_or(0);
                    let end = range.end;
                    let typename = match end {
                        Some(end) => quote! { #root::#generics::#slice2::<#start, #end> },
                        None => quote! { #root::#generics::#slice1::<#start> },
                    };
                    let getter = GetterByName::from_rule(&typename, name_str);
                    Intermediate { typename, getter }
                }
                Some(ProcessedPathArgs::Call(args)) => {
                    let (argc,) =
                        callable.ok_or_else(|| ModuleError::ExpectedArguments(rule_ref.clone()))?;
                    if let Some(argc) = argc {
                        ModuleError::check_argc(argc, args.len(), rule_ref)?;
                    }
                    let typename = quote! { #root::#generics::#name::< #( #args, )* > };
                    let getter = GetterByName::from_rule(&typename, name_str);
                    Intermediate { typename, getter }
                }
            },
        };
        Ok(res)
    }
}

pub enum ModuleNode<'g> {
    Collection(HashMap<&'g str, Rc<ModuleNode<'g>>>),
    Generics(RuleGenerics),
    Absolute(Box<dyn Fn(&TokenStream) -> Intermediate<'g>>),
}

impl<'g> ModuleNode<'g> {
    /// [Err] here is actually not an error.
    fn child(&self, node: &'_ str) -> Result<Rc<Self>, ModuleError<'g>> {
        let res = match self {
            Self::Collection(map) => map.get(node).cloned().ok_or_else(|| {
                ModuleError::NodeNotFound(node.to_owned(), map.keys().cloned().collect())
            })?,
            Self::Absolute(_) | Self::Generics(_) => Err(ModuleError::NoChildren)?,
        };
        Ok(res)
    }
    fn get(
        &self,
        root: &TokenStream,
        node: &'g str,
        rule_ref: &RuleRef<'g>,
        args: &Option<ProcessedPathArgs<'g>>,
    ) -> Result<Intermediate<'g>, ModuleError<'g>> {
        let res = match self {
            Self::Collection(_map) => Err(ModuleError::CollectionCannotBeCalled(rule_ref.clone()))?,
            Self::Absolute(f) => {
                let empty = match args {
                    None => true,
                    Some(ProcessedPathArgs::Call(args)) => args.is_empty(),
                    _ => false,
                };
                if !empty {
                    Err(ModuleError::UnexpectedArguments(rule_ref.clone()))?
                }
                f(root)
            }
            Self::Generics(generics) => generics.call(node, rule_ref, args.as_ref(), root)?,
        };
        Ok(res)
    }
    pub fn insert_rule(&mut self, rule: &'g ParseRule) -> Result<(), ModuleError<'g>> {
        let key = &rule.name;
        let argc = rule.args.len();
        let value = RuleGenerics::Rule { argc };
        if let ModuleNode::Collection(root) = self {
            let prev = root.insert(key, Rc::new(ModuleNode::Generics(value)));
            if prev.is_some() {
                Err(ModuleError::AlreadyDefined(key))
            } else {
                Ok(())
            }
        } else {
            Err(ModuleError::NotMutable)
        }
    }
    pub fn alias(&mut self, node: Rc<Self>, name: &'g str) -> Result<(), ModuleError<'g>> {
        match self {
            ModuleNode::Collection(collection) => {
                let res = collection.insert(name, node);
                if res.is_some() {
                    Err(ModuleError::AlreadyDefined(name))?
                }
                Ok(())
            }
            _ => Err(ModuleError::NotMutable),
        }
    }
    pub fn resolve_node<'s>(
        &self,
        mut iter: impl Iterator<Item = &'s str>,
    ) -> Result<Rc<Self>, ModuleError<'g>> {
        let r = self;
        let first = iter.next().ok_or_else(|| ModuleError::EmptyPath)?;
        let mut rc = r.child(first)?;
        for node in iter {
            rc = rc.child(node)?;
        }
        Ok(rc)
    }
}

/// Module system for pest3.
///
/// `'g` stands for the lifetime of grammar file content.
pub(crate) struct ModuleSystem<'g> {
    root: ModuleNode<'g>,
    pest: Rc<ModuleNode<'g>>,
}

impl<'g> ModuleSystem<'g> {
    pub fn make_global() -> Rc<ModuleNode<'g>> {
        static UNICODE_NAMES: OnceLock<Vec<(String, String)>> = OnceLock::new();
        let unicode_names = UNICODE_NAMES.get_or_init(|| {
            unicode_property_names()
                .map(|name| {
                    // name.is_ascii()
                    (name.to_ascii_lowercase(), name.to_ascii_uppercase())
                })
                .collect::<Vec<_>>()
        });
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
                ($( core::stringify!($T) ),*)
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

                    let name = lower.as_str();
                    let ident = format_ident!("{}", upper);
                    let node = ModuleNode::Absolute(Box::new(move |root| {
                        let typename = quote! { #root::unicode::#ident};
                        let getter = GetterByName::from_rule(&typename, name);
                        Intermediate { typename, getter }
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
        let root = ModuleNode::Collection(HashMap::from([(
            "pest",
            Rc::new(ModuleNode::Collection(pest)),
        )]));
        root.into()
    }
    pub fn new(pest: Rc<ModuleNode<'g>>) -> Self {
        let root = ModuleNode::Collection(Default::default());
        // FIXME: make "pest" paths to refer to pest3 crate for the moment
        Self { root, pest }
    }
    pub fn insert(&mut self, node: ModuleNode<'g>, name: &'g str) -> Result<(), ModuleError<'g>> {
        self.root.alias(node.into(), name)
    }
    pub fn insert_rule(&mut self, rule: &'g ParseRule) -> Result<(), ModuleError<'g>> {
        self.root.insert_rule(rule)
    }
    pub fn resolve_node(
        &self,
        iter: &'_ [impl AsRef<str>],
    ) -> Result<Rc<ModuleNode<'g>>, ModuleError<'g>> {
        self.pest
            .resolve_node(iter.iter().map(AsRef::as_ref))
            .or_else(|_err| self.root.resolve_node(iter.iter().map(AsRef::as_ref)))
    }
    pub fn resolve(
        &self,
        rule_ref: &'_ RuleRef<'g>,
        root: &TokenStream,
    ) -> Result<Intermediate<'g>, ModuleError<'g>> {
        let rc = self.resolve_node(&rule_ref.path)?;
        let res = rc.get(
            root,
            rule_ref.path.last().ok_or(ModuleError::EmptyPath)?,
            rule_ref,
            &rule_ref.args,
        )?;
        Ok(res)
    }
    pub fn alias(
        &mut self,
        source: &'g [impl AsRef<str>],
        name: &'g str,
    ) -> Result<(), ModuleError<'g>> {
        let node = self.resolve_node(source)?;
        self.root.alias(node, name)
    }
    pub fn take(self) -> ModuleNode<'g> {
        self.root
    }
}
