use std::collections::{btree_map::Entry, BTreeMap};

use crate::types::pest;

use super::{
    generator::{RuleConfig, RuleInfo},
    output::types_mod,
};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::Index;

#[derive(Clone)]
enum Edge {
    // Type remained.
    Content,
    ContentI(usize),
    // Type wrapped by Option.
    ChoiceI(usize),
    Optional,
    // Type wrapped by Vec.
    Contents,
}
#[derive(Clone, Debug)]
enum Node<'g> {
    /// Type resolved from #root::rules
    /// - Type: `& #(#root::rules::)* #ident`
    /// - Path: `.content.deref()`
    Rule(TokenStream, &'g ()),
    /// - Type: `&#ident`
    /// - Path: `.content.deref()`
    GenericsArg(&'g str),
    // Type remained.
    /// - Type: `#inner`
    /// - Path: `.content`
    Content(Box<Self>),
    /// - Type: `#inner`
    /// - Path: `.field_#index.matched`
    SequenceI(usize, Box<Self>),
    // Type wrapped by Option.
    /// - Type: `#opt::<#inner>`
    /// - Path: `._#index().and_then(|res| Some(#inner)) #flat`
    ChoiceI(usize, bool, Box<Self>),
    /// - Type: `#opt::<#inner>`
    /// - Path: `.as_ref().and_then(|res| Some(#inner)) #flat`
    Optional(bool, Box<Self>),
    // Type wrapped by Vec.
    /// - Type: `#vec::<#inner>`
    /// - Path: `.content.iter().map(|res| {let res = res.matched; #inner}).collect::<#vec<_>>()`
    Contents(Box<Self>),
    // Type wrapped by tuple.
    /// - Type: `(#(#inner),*)`
    /// - Path: `(#(#inner),*)`
    Tuple(Vec<Self>),
}

impl<'g> Node<'g> {
    fn from_rule(tokens: TokenStream) -> Self {
        Self::Rule(tokens, &())
    }
    fn from_generics_arg(name: &'g str) -> Self {
        Self::GenericsArg(name)
    }
    fn flattenable(&self) -> bool {
        match self {
            Node::Rule(..) | Node::GenericsArg(..) => false,
            Node::Content(inner) | Node::SequenceI(_, inner) => inner.flattenable(),
            Node::ChoiceI(_, false, _) | Node::Optional(false, _) => true,
            Node::ChoiceI(_, true, inner) | Node::Optional(true, inner) => inner.flattenable(),
            Node::Contents(_) | Node::Tuple(_) => false,
        }
    }
    pub fn wrap(self, edge: Edge) -> Self {
        match edge {
            Edge::Content => Self::Content(Box::new(self)),
            Edge::ContentI(i) => Self::SequenceI(i, Box::new(self)),
            Edge::ChoiceI(i) => Self::ChoiceI(i, self.flattenable(), Box::new(self)),
            Edge::Optional => Self::Optional(self.flattenable(), Box::new(self)),
            Edge::Contents => Self::Contents(Box::new(self)),
        }
    }
    pub fn merge(self, other: Self) -> Self {
        match self {
            Node::Tuple(vec) => match other {
                Node::Tuple(mut v) => {
                    let mut vec = vec;
                    vec.append(&mut v);
                    Node::Tuple(vec)
                }
                _ => {
                    let mut vec = vec;
                    vec.push(other);
                    Node::Tuple(vec)
                }
            },
            _ => match other {
                Node::Tuple(mut v) => {
                    let mut vec = vec![self];
                    vec.append(&mut v);
                    Node::Tuple(vec)
                }
                _ => Node::Tuple(vec![self, other]),
            },
        }
    }
    pub fn expand(
        &self,
        root: &TokenStream,
        config: &RuleConfig<'g>,
    ) -> (TokenStream, TokenStream) {
        let this = pest();
        let flat = |flatten: &bool| {
            if *flatten {
                quote! {.flatten()}
            } else {
                quote! {}
            }
        };
        let opt = |flatten: &bool, inner: TokenStream| {
            let opt = quote! {#this::std::Option};
            if *flatten {
                quote! {#inner}
            } else {
                quote! {#opt::<#inner>}
            }
        };
        let vec = quote! {#this::std::Vec};
        let rules_mod = types_mod();
        match self {
            Node::Rule(tokens, _) => (quote! {res}, quote! {&'s #tokens}),
            Node::GenericsArg(name) => {
                let ident = format_ident!("r#{}", name);
                (quote! {res}, quote! {&'s #ident})
            }
            Node::Content(inner) => {
                let (pa, ty) = inner.expand(root, config);
                (quote! {{let res = &res.content; #pa}}, quote! {#ty})
            }
            Node::SequenceI(i, inner) => {
                let (pa, ty) = inner.expand(root, config);
                let field = format_ident!("field_{}", i);
                (quote! {{let res = &res.#field; #pa}}, quote! {#ty})
            }
            Node::Optional(flatten, inner) => {
                let (pa, ty) = inner.expand(root, config);
                let flat = flat(flatten);
                (
                    quote! {{let res = res.as_ref().map(|res| #pa) #flat; res}},
                    opt(flatten, ty),
                )
            }
            Node::ChoiceI(index, flatten, inner) => {
                let (pa, ty) = inner.expand(root, config);
                let func = format_ident!("choice_{}", index);
                let flat = flat(flatten);
                (
                    quote! {{let res = res.#func().map(|res| #pa) #flat; res}},
                    opt(flatten, ty),
                )
            }
            Node::Contents(inner) => {
                let (pa, ty) = inner.expand(root, config);
                (
                    quote! {{let res = res.content.iter().map(|res| { #pa }).collect::<#vec<_>>(); res}},
                    quote! {#vec::<#ty>},
                )
            }
            Node::Tuple(tuple) => {
                let (pa, ty): (Vec<_>, Vec<_>) =
                    tuple.iter().map(|e| e.expand(root, config)).unzip();
                (quote! {{let res = (#(#pa),*); res}}, quote! {(#(#ty),*)})
            }
        }
    }
}

/// `'g` stands for the lifetime of rules.
#[derive(Clone, Debug)]
pub struct GetterByName<'g> {
    /// name -> (path, type)
    nodes: BTreeMap<&'g str, Node<'g>>,
}
impl<'g> Default for GetterByName<'g> {
    fn default() -> Self {
        Self::new()
    }
}
impl<'g> GetterByName<'g> {
    pub fn new() -> Self {
        Self {
            nodes: BTreeMap::new(),
        }
    }
    pub fn from_rule(tokens: &TokenStream, name: &'g str) -> Self {
        let res = BTreeMap::from([(name, Node::from_rule(tokens.clone()))]);
        Self { nodes: res }
    }
    pub fn from_generics_arg(name: &'g str) -> Self {
        let res = BTreeMap::from([(name, Node::from_generics_arg(name))]);
        Self { nodes: res }
    }
    pub fn content(self) -> Self {
        self.prepend(Edge::Content)
    }
    pub fn content_i(self, i: usize) -> Self {
        self.prepend(Edge::ContentI(i))
    }
    pub fn contents(self) -> Self {
        self.prepend(Edge::Contents)
    }
    pub fn optional(self) -> Self {
        self.prepend(Edge::Optional)
    }
    pub fn choice(self, i: usize) -> Self {
        self.prepend(Edge::ChoiceI(i))
    }
    #[inline]
    fn prepend(mut self, edge: Edge) -> Self {
        for (_, node) in self.nodes.iter_mut() {
            // TODO: Ellide clone here.
            *node = node.clone().wrap(edge.clone());
        }
        self
    }
    /// Join two getter forest in the same level.
    pub fn join_mut(&mut self, other: GetterByName<'g>) {
        other.nodes.into_iter().for_each(|(name, tree)| {
            let entry = self.nodes.entry(name);
            match entry {
                Entry::Vacant(entry) => {
                    entry.insert(tree);
                }
                Entry::Occupied(mut entry) => {
                    // TODO: Ellide clone here.
                    let t = entry.get_mut();
                    *t = t.clone().merge(tree);
                }
            }
        });
    }
    /// Join two getter forest in the same level.
    pub fn join(mut self, other: GetterByName<'g>) -> Self {
        self.join_mut(other);
        self
    }
    pub fn collect(
        &self,
        root: &TokenStream,
        config: &RuleConfig<'g>,
        rule_info: &RuleInfo<'g>,
    ) -> TokenStream {
        let getters = self.nodes.iter().map(|(name, node)| {
            let id = format_ident!("r#{}", name);
            let (paths, types) = node.expand(root, config);
            let deref = if rule_info.boxed {
                quote! {*}
            } else {
                quote! {}
            };
            let src = quote! {
                #[allow(non_snake_case)]
                pub fn #id<'s>(&'s self) -> #types {
                    let res = & #deref self.content;
                    #paths
                }
            };
            // We may generate source codes to help debugging here.
            let doc = format! {"A helper function to access [`{name}`]."};
            quote! {
                #[doc = #doc]
                #src
            }
        });
        quote! {
            #(#getters)*
        }
    }
}
