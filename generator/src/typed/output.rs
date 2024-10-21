use crate::{
    common::generate_rule_enum,
    types::{_str, pest},
};
use pest3_meta::parser::GrammarModule;
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use std::{
    collections::{BTreeMap, BTreeSet},
    marker::PhantomData,
};
use syn::Index;

pub fn generics() -> TokenStream {
    quote! {
        generics
    }
}
pub fn types_mod() -> TokenStream {
    quote! {rules}
}

pub fn constant_wrappers() -> TokenStream {
    quote! {wrapper}
}

pub(crate) struct Tracker<'g> {
    wrappers: Vec<TokenStream>,
    wrapper_counter: BTreeMap<&'g str, usize>,
    sequences: BTreeSet<usize>,
    choices: BTreeSet<usize>,
}
impl<'g> Tracker<'g> {
    pub fn new() -> Self {
        Self {
            wrappers: Vec::new(),
            wrapper_counter: BTreeMap::new(),
            sequences: BTreeSet::new(),
            choices: BTreeSet::new(),
        }
    }
    /// Record usage of Seq* generics.
    pub fn record_seq(&mut self, index: usize) {
        self.sequences.insert(index);
    }
    /// Record usage of Choices* generics.
    pub fn record_choice(&mut self, index: usize) {
        self.choices.insert(index);
    }
    /// Used sequences.
    pub fn seq(&self) -> &BTreeSet<usize> {
        &self.sequences
    }
    /// Used choices.
    pub fn choices(&self) -> &BTreeSet<usize> {
        &self.choices
    }
    /// Insert a string wrapper to corresponding module.
    /// Return the module path relative to module root.
    pub fn insert_string_wrapper(&mut self, string: &'g str) -> TokenStream {
        let wrapper_mod = constant_wrappers();
        if let Some(index) = self.wrapper_counter.get(string) {
            let s = format_ident!("W{}", index);
            quote! {#wrapper_mod::#s}
        } else {
            let index = self.wrapper_counter.len();
            let s = format_ident!("W{}", index);
            self.wrapper_counter.insert(string, index);
            let doc = format!("A wrapper for `{:?}`.", string);
            let str = _str();
            let this = pest();
            let wrapper = quote! {
                #[doc = #doc]
                #[derive(Clone, Debug, Hash, PartialEq, Eq)]
                pub struct #s;
                impl #this::typed::wrapper::String for #s {
                    const CONTENT: &'static #str = #string;
                }
            };
            self.wrappers.push(wrapper);
            quote! {#wrapper_mod::#s}
        }
    }
    pub fn collect(&self) -> TokenStream {
        let pest = pest();
        let wrappers = &self.wrappers;
        let wrapper_mod = constant_wrappers();
        let generics = {
            let fill = |set: &BTreeSet<usize>,
                        target: &mut Vec<TokenStream>,
                        prefix: &str,
                        mac: &Ident,
                        module: &Ident,
                        seq: bool| {
                for len in set.iter().cloned() {
                    let generics_i = format_ident!("{}{}", prefix, len);
                    let branch = (0..len).map(|i| {
                        if seq {
                            format_ident!("field_{}", i)
                        } else {
                            format_ident!("Choice{}", i)
                        }
                    });
                    let the_type = (0..len).map(|i| format_ident!("T{}", i));
                    let trivia_or_getter = (0..len).map(|i| {
                        if seq {
                            format_ident!("TR{}", i)
                        } else {
                            format_ident!("choice_{}", i)
                        }
                    });
                    // `pest` is already imported, so can be referred directly.
                    if len > 16 {
                        target.push(quote! {
                            #pest::#module::#mac!(#generics_i, #((#branch, #the_type, #trivia_or_getter),)*);
                        });
                    } else {
                        target.push(quote! {
                            pub use #pest::#module::#generics_i;
                        })
                    }
                }
            };
            let mut seq = vec![];
            let mut chs = vec![];
            fill(
                self.seq(),
                &mut seq,
                "Sequence",
                &format_ident!("sequence_type"),
                &format_ident!("sequence"),
                true,
            );
            fill(
                self.choices(),
                &mut chs,
                "Choice",
                &format_ident!("choice_type"),
                &format_ident!("choice"),
                false,
            );

            quote! {
                pub use #pest::typed::unicode;
                #[doc = "Used generics."]
                pub mod generics {
                    pub use #pest::typed::template::{
                        Positive, Negative,
                        CharRange, Str, Insens,
                        Rep, RepOnce, RepMin, RepMax, RepMinMax,
                        SOI as soi, EOI as eoi,
                        ANY as any,
                        PEEK as peek,
                        PeekSlice1, PeekSlice2,
                        PEEK_ALL as peek_all,
                        DROP as drop,
                        PUSH as push,
                        POP as pop,
                        POP_ALL as pop_all,
                        ascii_digit, ascii_nonzero_digit,
                        ascii_bin_digit, ascii_oct_digit,
                        ascii_hex_digit, ascii_alpha_lower,
                        ascii_alpha_upper, ascii_alpha,
                        ascii_alphanumeric, ascii,
                        NEWLINE as newline,
                    };
                    #(#seq)*
                    #(#chs)*
                }
            }
        };
        quote! {
            mod #wrapper_mod {
                #(#wrappers)*
            }
            #generics
        }
    }
}
impl ToTokens for Tracker<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(self.collect())
    }
}

pub(crate) struct Output<'g> {
    content: Vec<TokenStream>,
    optional_trivia: Option<TokenStream>,
    mandatory_trivia: Option<TokenStream>,
    modules: Vec<(Ident, Self)>,
    phantom: PhantomData<&'g ()>,
    rule_enum: TokenStream,
}
impl<'g> Output<'g> {
    pub fn new(module: &'g GrammarModule, modules: Vec<(Ident, Self)>) -> Self {
        let rule_enum = generate_rule_enum(module);
        Self {
            content: Vec::new(),
            optional_trivia: None,
            mandatory_trivia: None,
            modules,
            phantom: PhantomData,
            rule_enum,
        }
    }
    /// Insert rule struct to rule module.
    pub fn insert_rule_struct(&mut self, tokens: TokenStream) {
        self.content.push(tokens);
    }
    pub fn add_option_trivia(&mut self, tokens: TokenStream) {
        self.optional_trivia = Some(tokens);
    }
    pub fn add_mandatory_trivia(&mut self, tokens: TokenStream) {
        self.mandatory_trivia = Some(tokens);
    }
    /// Collect to final [TokenStream].
    pub fn collect(&self) -> TokenStream {
        let pest = pest();
        let content = &self.content;
        let types = types_mod();
        let rule_enum = &self.rule_enum;
        let optional_trivia = self
            .optional_trivia
            .clone()
            .unwrap_or(quote! {#pest::typed::template::Empty});
        let mandatory_trivia = self
            .mandatory_trivia
            .clone()
            .unwrap_or(quote! {#pest::typed::template::Empty});
        let modules = self.modules.iter().map(|(name, output)| {
            let output = output.collect();
            quote! {
                pub mod #name {
                    #output
                }
                impl #pest::typed::SubRule for #name::Rule {
                    type Super = super::Rule;
                    fn cvt_into(self) -> Self::Super {
                        super::Rule::#name(self)
                    }
                }
            }
        });
        quote! {
            #rule_enum
            impl #pest::typed::RuleType for Rule {
                const EOI: Self = Rule::EOI;
            }
            #[doc = "Definitions of statically typed nodes generated by pest-generator."]
            pub mod #types {
                pub type __OptionalTrivia<'i> = #optional_trivia;
                pub type __MandatoryTrivia<'i> = #mandatory_trivia;
                #(#modules)*
                #(#content)*
            }
        }
    }
}
