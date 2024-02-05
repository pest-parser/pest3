use super::{
    attr::parse_derive,
    config::Config,
    output::{generics, Output},
};
use crate::{
    common::{generate_include, generate_rule_enum},
    config::collect_data,
    typed::output::pest,
    types::option_type,
};
use pest_meta::{
    doc::DocComment,
    error::rename_meta_rule,
    parser::{self, ParseExpr, ParseRule, PathArgs, Trivia},
};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use std::{
    collections::{BTreeMap, BTreeSet},
    path::PathBuf,
};
use syn::{DeriveInput, Generics};

/// `rule_id` should be a valid identifier.
struct RuleConfig<'g> {
    pub rule_id: Ident,
    pub rule_name: &'g str,
    pub boxed: bool,
}
impl<'g> RuleConfig<'g> {
    fn from(rule: &'g ParseRule, not_boxed: &BTreeSet<RuleRef<'g>>) -> Self {
        let rule_name = rule.name.as_str();
        let path = vec![];
        let rule_ref = RuleRef::from_current(rule_name, path);
        let boxed = !not_boxed.contains(&rule_ref);
        let rule_id = format_ident!("r#{}", rule_name);
        Self {
            rule_id,
            rule_name,
            boxed,
        }
    }
}

/// `'g` refers to grammar.
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct RuleRef<'g> {
    path: Vec<&'g str>,
    name: &'g str,
    args: Option<&'g PathArgs>,
}
impl<'g> RuleRef<'g> {
    /// Create from a [Path](ParseExpr::Path).
    fn new(path: &'g Vec<String>, args: &'g Option<PathArgs>) -> Self {
        let mut path = path.iter().map(String::as_str).collect::<Vec<_>>();
        let name = path.pop().expect("Null path found.");
        let args = args.as_ref();
        Self { path, name, args }
    }
    /// Create from a name of a rule defined in current module.
    fn from_current(name: &'g str, path: Vec<&'g str>) -> Self {
        let args = None;
        Self { path, name, args }
    }
    /// Get name if it's inside given module.
    fn get_name_if_in(&self, path: Vec<&'g str>) -> Option<&'g str> {
        if path != self.path {
            None
        } else {
            Some(self.name)
        }
    }
}

fn create_rule<'g>(
    rule_config: &RuleConfig<'g>,
    type_name: TokenStream,
    root: TokenStream,
) -> TokenStream {
    fn create<'g>(
        rule_config: &RuleConfig<'g>,
        inner_type: TokenStream,
        root: TokenStream,
    ) -> TokenStream {
        let name = &rule_config.rule_id;
        let this = pest();
        quote! {
            #[derive(Clone, Debug, Eq, PartialEq)]
            pub struct #name {
                content: #inner_type,
            }
            impl<'i> #this::typed::TypedNode<'i, #root::Rule> for #name {
                fn try_parse_with_partial(
                    input: #this::Position<'i>,
                    stack: &mut #this::Stack<#this::Span<'i>>,
                    tracker: &mut #this::typed::Tracker<'i, #root::Rule>,
                ) -> ::core::option::Option<(#this::Position<'i>, Self)> {
                    let (pos, content) = #inner_type::try_parse_with_partial(input, stack, tracker)?;
                    let res = Self{ content};
                    ::core::option::Option::Some((pos, res))
                }
            }
        }
    }
    create(rule_config, type_name, root)
}

struct Intermediate {
    typename: TokenStream,
}

impl Intermediate {
    fn from_path(rule_ref: RuleRef<'_>) -> Self {
        let prefix = rule_ref
            .path
            .iter()
            .map(|module| format_ident!("{}", module));
        let name = format_ident!("{}", rule_ref.name);
        let typename = quote! { #(#prefix::)* #name };
        Self { typename }
    }
}

fn collect_sequence<'p>(
    left: &'p ParseExpr,
    right: &'p ParseExpr,
    mut trivia: Trivia,
) -> Vec<(Trivia, &'p ParseExpr)> {
    let mut res = vec![(Trivia::None, left)];
    let mut node = right;
    while let ParseExpr::Seq(left, right, next_trivia) = node {
        res.push((trivia, &left.expr));
        trivia = next_trivia.clone().unwrap_or(Trivia::None);
        node = &right.expr;
    }
    res.push((trivia, node));
    res
}

fn collect_choices<'p>(left: &'p ParseExpr, right: &'p ParseExpr) -> Vec<&'p ParseExpr> {
    let mut res = vec![left];
    let mut node = right;
    while let ParseExpr::Choice(left, right) = node {
        res.push(&left.expr);
        node = &right.expr;
    }
    res.push(node);
    res
}

/// Returns type name.
fn generate_graph_node<'g>(
    expr: &'g ParseExpr,
    rule_config: &RuleConfig<'g>,
    output: &mut Output<'g>,
    config: Config,
    root: &TokenStream,
) -> Intermediate {
    let generics = generics();
    match expr {
        ParseExpr::Str(content) => {
            let wrapper = output.insert_string_wrapper(content.as_str());
            let typename = quote! {#root::#generics::Str::<#root::#wrapper>};
            Intermediate { typename }
        }
        ParseExpr::Insens(content) => {
            let wrapper = output.insert_string_wrapper(content.as_str());
            let typename = quote! {#root::#generics::Insens::<'i, #root::#wrapper>};
            Intermediate { typename }
        }
        ParseExpr::Range(start, end) => {
            let typename = quote! {#root::#generics::CharRange::<#start, #end>};
            Intermediate { typename }
        }
        ParseExpr::Path(prefix, args) => Intermediate::from_path(RuleRef::new(prefix, args)),
        ParseExpr::PosPred(node) => {
            let inner = generate_graph_node(&node.expr, rule_config, output, config, root);
            let inner_typename = &inner.typename;
            let typename = quote! {#root::#generics::Positive::<#inner_typename>};
            Intermediate { typename }
        }
        ParseExpr::NegPred(node) => {
            // Impossible to access inner tokens.
            let inner = generate_graph_node(&node.expr, rule_config, output, config, root);
            let inner_typename = &inner.typename;
            let typename = quote! {#root::#generics::Negative::<#inner_typename>};
            Intermediate { typename }
        }
        ParseExpr::Seq(left, right, trivia) => {
            let vec = collect_sequence(
                &left.expr,
                &right.expr,
                trivia.clone().unwrap_or(Trivia::None),
            );
            let mut types = Vec::with_capacity(vec.len());
            for (trivia, expr) in vec.into_iter() {
                let child = generate_graph_node(expr, rule_config, output, config, root);
                types.push((child, trivia));
            }
            let typenames = types.iter().map(|(inter, trivia)| {
                let typename = &inter.typename;
                let trivia = matches!(trivia, Trivia::Mandatory | Trivia::Optional);
                quote! {#typename, #trivia}
            });

            let seq = format_ident!("Sequence{}", types.len());
            output.record_seq(types.len());

            let typename = quote! { #root::#generics::#seq::<#(#typenames, )*> };
            Intermediate { typename }
        }
        ParseExpr::Choice(left, right) => {
            let vec = collect_choices(&left.expr, &right.expr);
            let mut types = vec![];
            for expr in vec.into_iter() {
                let child = generate_graph_node(expr, rule_config, output, config, root);
                types.push(child);
            }
            let typenames = types.iter().map(|inter| &inter.typename);

            let choice = format_ident!("Choice{}", types.len());
            output.record_choice(types.len());

            let typename = quote! { #root::#generics::#choice::<#(#typenames, )*> };
            Intermediate { typename }
        }
        ParseExpr::Opt(inner) => {
            let inner = generate_graph_node(&inner.expr, rule_config, output, config, root);
            let option = option_type();
            let inner_name = &inner.typename;
            let typename = quote! {#option::<#inner_name>};
            Intermediate { typename }
        }
        ParseExpr::Rep(inner) => {
            let inner = generate_graph_node(&inner.expr, rule_config, output, config, root);
            let inner_name = &inner.typename;
            let typename = quote! { #root::#generics::Rep::<'i, #inner_name> };
            Intermediate { typename }
        }
        ParseExpr::RepOnce(inner) => {
            let inner = generate_graph_node(&inner.expr, rule_config, output, config, root);
            let inner_name = &inner.typename;
            let typename = quote! { #root::#generics::RepOnce::<'i, #inner_name> };
            Intermediate { typename }
        }
        ParseExpr::RepRange(inner, range) => {
            let inner = generate_graph_node(&inner.expr, rule_config, output, config, root);
            let inner_name = &inner.typename;
            let parser::Range { start, end } = range;
            let typename = match (start, end) {
                (Some(start), Some(end)) => {
                    quote! { #root::#generics::RepRange::<'i, #inner_name, #start, #end> }
                }
                (Some(start), None) => {
                    quote! { #root::#generics::RepMin::<'i, #inner_name, #start> }
                }
                (None, Some(end)) => {
                    quote! { #root::#generics::RepRange::<'i, #inner_name, 0, #end> }
                }
                (None, None) => {
                    quote! { #root::#generics::Rep::<'i, #inner_name> }
                }
            };
            Intermediate { typename }
        }
        ParseExpr::Separated(inner, trivia) => {
            let inner = generate_graph_node(&inner.expr, rule_config, output, config, root);
            let inner_name = &inner.typename;
            let typename = quote! { #root::#generics::Rep::<'i, #inner_name> };
            Intermediate { typename }
        }
    }
}

fn generate_graph<'g: 'f, 'f>(
    rules: &'g [ParseRule],
    defined: &'f BTreeSet<&'g str>,
    not_boxed: &'f BTreeSet<RuleRef<'g>>,
    config: Config,
) -> Output<'g> {
    let mut res = Output::new();
    for rule in rules.iter() {
        if matches!(rule.name.as_str(), "~" | "^") {
            todo!()
        } else {
            let rule_config = RuleConfig::from(rule, not_boxed);
            let inter = generate_graph_node(
                &rule.node.expr,
                &rule_config,
                &mut res,
                config,
                &quote! {super},
            );
            res.insert_rule_struct(create_rule(&rule_config, inter.typename, quote! {super}));
        }
    }
    res
}

/// `'g` refers to grammar.
fn collect_used_rule<'g>(
    rule: &'g ParseRule,
    rule_trivia: Option<&'g ParseRule>,
    res: &mut BTreeSet<RuleRef<'g>>,
) {
    let mut nodes = vec![&rule.node];
    let expect_trivia = format!(
        "Please define trivia with `~ = \"...\"`. It's used in rule `{}`.",
        rule.name
    );
    while let Some(expr) = nodes.pop() {
        match &expr.expr {
            ParseExpr::Str(_) | ParseExpr::Insens(_) | ParseExpr::Range(_, _) => (),
            ParseExpr::PosPred(node) | ParseExpr::NegPred(node) => nodes.push(node),
            ParseExpr::Seq(lhs, rhs, trivia) => {
                nodes.push(lhs);
                nodes.push(rhs);
                if let Some(Trivia::Mandatory) | Some(Trivia::Optional) = trivia {
                    let rule_trivia = rule_trivia.expect(&expect_trivia);
                    nodes.push(&rule_trivia.node);
                }
            }
            ParseExpr::Choice(lhs, rhs) => {
                nodes.push(lhs);
                nodes.push(rhs);
            }
            ParseExpr::Opt(node)
            | ParseExpr::Rep(node)
            | ParseExpr::RepOnce(node)
            | ParseExpr::RepRange(node, _) => nodes.push(node),
            ParseExpr::Path(path, args) => {
                res.insert(RuleRef::new(path, args));
            }
            ParseExpr::Separated(node, trivia) => {
                nodes.push(node);
                if let Trivia::Mandatory | Trivia::Optional = trivia {
                    let rule_trivia = rule_trivia.expect(&expect_trivia);
                    nodes.push(&rule_trivia.node);
                }
            }
        }
    }
}

fn collect_reachability<'g>(rules: &'g [ParseRule]) -> BTreeMap<RuleRef, BTreeSet<RuleRef>> {
    let mut res = BTreeMap::new();
    let rule_trivia = rules.iter().find(|rule| rule.name == "~");
    let path = vec![];
    for rule in rules {
        let entry = res
            .entry(RuleRef::from_current(rule.name.as_str(), path.clone()))
            .or_default();
        collect_used_rule(rule, rule_trivia, entry);
    }
    for _ in 0..rules.len() {
        for rule in rules {
            let rule_ref = RuleRef::from_current(rule.name.as_str(), path.clone());
            if let Some(cur) = res.remove(&rule_ref) {
                let mut new = cur.clone();
                for referenced in cur {
                    if let Some(iter) = res.get(&referenced) {
                        new.extend(iter.iter().cloned());
                    }
                }
                if !new.contains(&rule_ref) {
                    res.insert(rule_ref, new);
                }
            }
        }
    }
    res
}

fn generate_typed_pair_from_rule(rules: &[ParseRule], config: Config) -> TokenStream {
    let defined_rules: BTreeSet<&str> = rules.iter().map(|rule| rule.name.as_str()).collect();
    let not_boxed = collect_reachability(rules).keys().cloned().collect();
    let graph = generate_graph(rules, &defined_rules, &not_boxed, config);
    graph.collect()
}

/// Generate codes for Parser.
fn generate_typed(
    name: Ident,
    generics: &Generics,
    paths: Vec<PathBuf>,
    rules: Vec<ParseRule>,
    doc_comment: &DocComment,
    include_grammar: bool,
    config: Config,
) -> TokenStream {
    let include_fix = if include_grammar {
        generate_include(&name, paths)
    } else {
        quote!()
    };
    let rule_enum = generate_rule_enum(&rules, doc_comment);
    let definition = generate_typed_pair_from_rule(&rules, config);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let res = quote! {
        #include_fix
        #rule_enum
        #definition
    };
    res
}

pub fn derive_typed_parser(input: TokenStream, include_grammar: bool) -> TokenStream {
    let ast: DeriveInput = syn::parse2(input).unwrap();
    let (name, generics, contents, config) = parse_derive(ast);

    let (data, paths) = collect_data(contents);

    let (rules, doc) = match parser::parse_with_doc_comment(&data, &data) {
        Ok(pairs) => pairs,
        Err(error) => panic!("error parsing \n{}", error.renamed_rules(rename_meta_rule)),
    };

    generate_typed(name, &generics, paths, rules, &doc, include_grammar, config)
}
