use super::{
    attr::parse_derive,
    config::Config,
    module::ModuleSystem,
    output::{generics, Output},
};
use crate::{
    common::{generate_include, generate_rule_enum},
    config::collect_data,
    types::option_type,
    types::pest,
};
use pest_meta::{
    doc::DocComment,
    error::rename_meta_rule,
    parser::{self, fmt_sep, ParseExpr, ParseNode, ParseRule, PathArgs, Range, Trivia},
};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Display,
    path::PathBuf,
};
use syn::{DeriveInput, Generics};

/// Configuration that controls inner content.
struct RuleConfig<'g> {
    grammar: &'g ParseRule,
}
impl<'g> RuleConfig<'g> {
    fn from(rule: &'g ParseRule) -> Self {
        Self { grammar: rule }
    }
}

/// Information about the output of this rule.
///
/// `rule_id` should be a valid identifier.
struct RuleInfo<'g> {
    pub rule_id: Ident,
    pub rule_name: &'g str,
    pub silent: bool,
    pub boxed: bool,
}
impl<'g> RuleInfo<'g> {
    fn new(
        rule: &'g ParseRule,
        config: Config,
        reachability: &BTreeMap<&str, BTreeSet<&str>>,
    ) -> Self {
        let rule_name = rule.name.as_str();
        let boxed = match config.box_all_rules {
            true => true,
            false => !reachability.contains_key(rule_name),
        };
        let rule_id = format_ident!("r#{}", rule_name);
        let silent = rule.silent;
        Self {
            rule_id,
            rule_name,
            boxed,
            silent,
        }
    }
}

#[derive(Clone, Debug)]
pub enum ProcessedPathArgs {
    Call(Vec<Intermediate>),
    Slice(Range<isize>),
}

impl Display for ProcessedPathArgs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Call(args) => {
                write!(f, "(")?;
                fmt_sep(args, ", ", f)?;
                write!(f, ")")
            }
            Self::Slice(range) => write!(f, "{}", range),
        }
    }
}

impl ProcessedPathArgs {
    fn process<'g>(
        args: &'g PathArgs,
        rule_config: &RuleConfig<'g>,
        output: &mut Output<'g>,
        config: Config,
        mod_sys: &ModuleSystem<'g>,
        root: &TokenStream,
    ) -> Self {
        match args {
            PathArgs::Call(args) => {
                let args = args
                    .iter()
                    .map(|arg| process_expr(&arg.expr, rule_config, output, config, mod_sys, root))
                    .collect();
                Self::Call(args)
            }
            PathArgs::Slice(range) => Self::Slice(range.clone()),
        }
    }
}

/// Reference to a rule.
///
/// `'g` refers to grammar.
#[derive(Clone, Debug)]
pub struct RuleRef<'g> {
    pub path: Vec<&'g str>,
    pub args: Option<ProcessedPathArgs>,
}
impl<'g> RuleRef<'g> {
    /// Create from a [Path](ParseExpr::Path).
    fn new(path: &'g Vec<String>, args: Option<ProcessedPathArgs>) -> Self {
        let path = path.iter().map(String::as_str).collect::<Vec<_>>();
        Self { path, args }
    }
}
impl<'g> Display for RuleRef<'g> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt_sep(&self.path, "::", f)?;
        match &self.args {
            Some(args) => write!(f, "{}", args)?,
            None => (),
        }
        Ok(())
    }
}

fn create_rule<'g>(
    config: Config,
    rule_config: &RuleConfig<'g>,
    rule_info: &RuleInfo<'g>,
    inner_type: TokenStream,
    root: TokenStream,
) -> TokenStream {
    let name = &rule_info.rule_id;
    let this = pest();
    let doc = format!(
        "Generated for rule `{}`. Grammar: `{}`.",
        rule_info.rule_name, rule_config.grammar.node,
    );
    let args: Vec<_> = rule_config
        .grammar
        .args
        .iter()
        .map(|s| format_ident!("r#{}", s))
        .collect();
    // Pairs inside silent rule will be ignored.
    let pair_api = match rule_info.silent {
        true => quote! {
            impl<'i, #(#args, )*> #this::typed::PairContainer<#root::Rule> for #name<'i, #(#args, )*> {
                fn for_each_child_pair(&self, f: &mut impl #this::std::FnMut(#this::token::Pair<#root::Rule>)) {}
                fn for_self_or_for_each_child_pair(&self, f: &mut impl #this::std::FnMut(#this::token::Pair<#root::Rule>)) {}
            }
        },
        false => quote! {
            impl<'i, #(#args, )*> #this::typed::PairContainer<#root::Rule> for #name<'i, #(#args, )*> {
                fn for_each_child_pair(&self, f: &mut impl #this::std::FnMut(#this::token::Pair<#root::Rule>)) {
                    self.content.for_self_or_for_each_child_pair(f)
                }
                fn for_self_or_for_each_child_pair(&self, f: &mut impl #this::std::FnMut(#this::token::Pair<#root::Rule>)) {
                    use #this::typed::PairTree;
                    f(self.as_pair_tree())
                }
            }
            impl<'i, #(#args, )*> #this::typed::PairTree<#root::Rule> for #name<'i, #(#args, )*> {
                fn get_span(&self) -> (#this::std::usize, #this::std::usize) {
                    (self.span.start(), self.span.end())
                }
            }
        },
    };
    let content_type = if rule_info.boxed {
        quote! {#this::std::Box<#inner_type>}
    } else {
        inner_type.clone()
    };
    quote! {
        #[doc = #doc]
        #[derive(Clone, Debug, Eq, PartialEq)]
        pub struct #name<'i, #(#args, )*> {
            /// Matched structure.
            pub content: #content_type,
            /// Matched span.
            pub span: #this::Span<'i>,
        }
        impl<'i, #(#args, )*> #this::typed::wrapper::Rule for #name<'i, #(#args, )*> {
            type Rule = #root::Rule;
            const RULE: #root::Rule = #root::Rule::#name;
        }
        impl<'i, #(#args, )*> #this::typed::FullRuleStruct<'i> for #name<'i, #(#args, )*> {
            type Inner = #inner_type;
            type Content = #content_type;
            #[inline]
            fn new(content: <Self as #this::typed::FullRuleStruct<'i>>::Content, span: #this::Span<'i>) -> Self {
                Self { content, span }
            }
        }
        #pair_api
    }
}

#[derive(Clone, Debug)]
pub struct Intermediate {
    typename: TokenStream,
}

impl Intermediate {}

impl Display for Intermediate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.typename.fmt(f)
    }
}

impl ToTokens for Intermediate {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(self.typename.clone())
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
        trivia = next_trivia.clone();
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

fn get_trivia(expr: &ParseExpr) -> Trivia {
    match expr {
        ParseExpr::Separated(_, trivia) => trivia.clone(),
        _ => Trivia::None,
    }
}

/// Returns type name.
fn process_expr<'g>(
    expr: &'g ParseExpr,
    rule_config: &RuleConfig<'g>,
    output: &mut Output<'g>,
    config: Config,
    mod_sys: &ModuleSystem<'g>,
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
        ParseExpr::Path(prefix, args) => {
            let args = args.as_ref().map(|args| {
                ProcessedPathArgs::process(args, rule_config, output, config, mod_sys, root)
            });
            let rule_ref = RuleRef::new(prefix, args);
            let typename = mod_sys
                .resolve(rule_config.grammar, &rule_ref, root);
            Intermediate { typename }
        }
        ParseExpr::PosPred(node) => {
            let inner = process_expr(&node.expr, rule_config, output, config, mod_sys, root);
            let inner_typename = &inner.typename;
            let typename = quote! {#root::#generics::Positive::<#inner_typename>};
            Intermediate { typename }
        }
        ParseExpr::NegPred(node) => {
            // Impossible to access inner tokens.
            let inner = process_expr(&node.expr, rule_config, output, config, mod_sys, root);
            let inner_typename = &inner.typename;
            let typename = quote! {#root::#generics::Negative::<#inner_typename>};
            Intermediate { typename }
        }
        ParseExpr::Seq(left, right, trivia) => {
            let vec = collect_sequence(&left.expr, &right.expr, trivia.clone());
            let mut types = Vec::with_capacity(vec.len());
            for (trivia, expr) in vec.into_iter() {
                let child = process_expr(expr, rule_config, output, config, mod_sys, root);
                types.push((child, trivia));
            }
            let typenames = types.iter().map(|(inter, trivia)| {
                let typename = &inter.typename;
                let trivia = trivia.get_code();
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
                let child = process_expr(expr, rule_config, output, config, mod_sys, root);
                types.push(child);
            }
            let typenames = types.iter().map(|inter| &inter.typename);

            let choice = format_ident!("Choice{}", types.len());
            output.record_choice(types.len());

            let typename = quote! { #root::#generics::#choice::<#(#typenames, )*> };
            Intermediate { typename }
        }
        ParseExpr::Opt(inner) => {
            let inner = process_expr(&inner.expr, rule_config, output, config, mod_sys, root);
            let option = option_type();
            let inner_name = &inner.typename;
            let typename = quote! {#option::<#inner_name>};
            Intermediate { typename }
        }
        ParseExpr::Rep(inner_node) => {
            let inner = process_expr(&inner_node.expr, rule_config, output, config, mod_sys, root);
            let inner_name = &inner.typename;
            let trivia = get_trivia(&inner_node.expr).get_code();
            let typename = quote! { #root::#generics::Rep::<#inner_name, #trivia> };
            Intermediate { typename }
        }
        ParseExpr::RepOnce(inner_node) => {
            let inner = process_expr(&inner_node.expr, rule_config, output, config, mod_sys, root);
            let inner_name = &inner.typename;
            let trivia = get_trivia(&inner_node.expr).get_code();
            let typename = quote! { #root::#generics::RepOnce::<#inner_name, #trivia> };
            Intermediate { typename }
        }
        ParseExpr::RepRange(inner_node, range) => {
            let inner = process_expr(&inner_node.expr, rule_config, output, config, mod_sys, root);
            let inner_name = &inner.typename;
            let parser::Range { start, end } = range;
            let trivia = get_trivia(&inner_node.expr).get_code();
            let typename = match (start, end) {
                (Some(start), Some(end)) => {
                    quote! { #root::#generics::RepMinMax::<#inner_name, #trivia, #start, #end> }
                }
                (Some(start), None) => {
                    quote! { #root::#generics::RepMin::<#inner_name, #trivia, #start> }
                }
                (None, Some(end)) => {
                    quote! { #root::#generics::RepMax::<#inner_name, #trivia, #end> }
                }
                (None, None) => {
                    quote! { #root::#generics::Rep::<#inner_name, #trivia> }
                }
            };
            Intermediate { typename }
        }
        ParseExpr::Separated(inner, _trivia) => {
            process_expr(&inner.expr, rule_config, output, config, mod_sys, root)
        }
    }
}

fn process_rule<'g: 'f, 'f>(
    rule: &'g ParseRule,
    mod_sys: &ModuleSystem<'g>,
    reachability: &BTreeMap<&str, BTreeSet<&str>>,
    config: Config,
    res: &mut Output<'g>,
) {
    let rule_config = RuleConfig::from(rule);
    let inter = process_expr(
        &rule.node.expr,
        &rule_config,
        res,
        config,
        &mod_sys,
        &quote! {super},
    );
    match rule.name.as_str() {
        "~" => res.add_option_trivia(inter.typename),
        "^" => res.add_mandatory_trivia(inter.typename),
        _ => {
            let rule_info = RuleInfo::new(rule, config, reachability);
            res.insert_rule_struct(create_rule(
                config,
                &rule_config,
                &rule_info,
                inter.typename,
                quote! {super},
            ));
        }
    }
}

fn process_rules<'g: 'f, 'f>(
    rules: &'g [ParseRule],
    mod_sys: &mut ModuleSystem<'g>,
    config: Config,
) -> Output<'g> {
    let reachability = collect_reachability(rules);
    for rule in rules.iter() {
        match rule.name.as_str() {
            "~" | "^" => {}
            _ => mod_sys.insert_rule(rule),
        }
    }
    let mut res = Output::new();
    for rule in rules.iter() {
        process_rule(rule, mod_sys, &reachability, config, &mut res);
    }
    res
}

/// `'g` refers to grammar.
fn collect_used_rule<'g>(
    rule: &'g ParseRule,
    rule_trivia: Option<&'g ParseRule>,
    res: &mut BTreeSet<&'g str>,
) {
    let mut nodes: Vec<&'g ParseNode> = vec![];
    let expect_trivia = format!(
        "Please define trivia with `~ = \"...\"`. It's used in rule `{}`.",
        rule.name
    );
    let mut f = |expr: &'g parser::ParseNode, nodes: &mut Vec<&'g ParseNode>| match &expr.expr {
        ParseExpr::Str(_) | ParseExpr::Insens(_) | ParseExpr::Range(_, _) => (),
        ParseExpr::PosPred(node) | ParseExpr::NegPred(node) => nodes.push(node),
        ParseExpr::Seq(lhs, rhs, trivia) => {
            nodes.push(lhs);
            nodes.push(rhs);
            if let Trivia::Mandatory | Trivia::Optional = trivia {
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
            match args {
                // Normally nodes are linked directly.
                Some(PathArgs::Call(args)) => nodes.extend(args),
                _ => (),
            }
            // Generics from another module is ignored.
            if path.len() == 1 {
                res.insert(&path[0]);
            }
        }
        ParseExpr::Separated(node, trivia) => {
            nodes.push(node);
            if let Trivia::Mandatory | Trivia::Optional = trivia {
                let rule_trivia = rule_trivia.expect(&expect_trivia);
                nodes.push(&rule_trivia.node);
            }
        }
    };
    f(&rule.node, &mut nodes);
    while let Some(expr) = nodes.pop() {
        if expr == &rule.node {
            continue;
        }
        f(expr, &mut nodes);
    }
}

#[cfg(test)]
fn collect_used_rules<'s>(rules: &'s [ParseRule]) -> BTreeSet<&'s str> {
    let mut res = BTreeSet::new();
    let rule_trivia = rules.iter().find(|rule| rule.name == "~");
    for rule in rules {
        collect_used_rule(rule, rule_trivia, &mut res);
    }
    res
}

/// Wrap some nodes in [std::boxed::Box] to avoid infinite size struct,
/// which can break the edges in the reference graph,
/// and then collect reachability.
///
/// Rules that are not in map keys are wrapped.
///
/// We won't promise anything on which nodes are boxed.
fn collect_reachability(rules: &[ParseRule]) -> BTreeMap<&str, BTreeSet<&str>> {
    let mut res = BTreeMap::new();
    let rule_trivia = rules.iter().find(|rule| rule.name == "~");
    for rule in rules {
        let entry = res.entry(rule.name.as_str()).or_default();
        collect_used_rule(rule, rule_trivia, entry);
    }
    // Length of any path is no more than `rules.len()`.
    for _ in 0..rules.len() {
        // Before the `i`-th iteration,
        // `res[a]` contains all nodes that can be reached from `a`
        // in no more than `i+1` steps.
        for rule in rules {
            let rule_ref = rule.name.as_str();
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
    let mut mod_sys = ModuleSystem::new();
    let graph = process_rules(rules, &mut mod_sys, config);
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
    impl_parser: bool,
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
    let pest = pest();

    let impl_parser = match impl_parser {
        true => quote! {
            #[allow(clippy::all)]
            impl #impl_generics #pest::typed::TypedParser<Rule> for #name #ty_generics #where_clause {}
        },
        false => quote! {},
    };

    let res = quote! {
        #include_fix
        #rule_enum
        #impl_parser
        #definition
    };
    res
}

/// Derive typed parser from given grammar, attributes and type.
pub fn derive_typed_parser(
    input: TokenStream,
    include_grammar: bool,
    impl_parser: bool,
) -> TokenStream {
    let ast: DeriveInput = syn::parse2(input).unwrap();
    let (name, generics, contents, config) = parse_derive(ast);

    let (data, paths) = collect_data(contents);

    let (rules, doc) = match parser::parse_with_doc_comment(&data, &data) {
        Ok(pairs) => pairs,
        Err(error) => panic!("error parsing \n{}", error.renamed_rules(rename_meta_rule)),
    };

    generate_typed(
        name,
        &generics,
        paths,
        rules,
        &doc,
        include_grammar,
        impl_parser,
        config,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use lazy_static::lazy_static;
    use pest_meta::{
        doc::DocComment,
        parser::{self, ParseRule},
    };
    use std::string::String;

    lazy_static! {
        static ref SYNTAX: String =
            String::from_utf8(std::fs::read("tests/syntax.pest").unwrap()).unwrap();
        static ref PARSE_RESULT: (Vec<ParseRule>, DocComment) =
            parser::parse_with_doc_comment(&SYNTAX, &"tests/syntax.pest").unwrap();
    }

    #[test]
    fn inlined_used_rules() {
        let rules = parser::parse(
            r#"
x = a - b
a = "a"*
b = "b"+
"#,
            &file!(),
        )
        .unwrap();
        let used = collect_used_rules(&rules);
        assert_eq!(used, BTreeSet::from(["a", "b"]));
    }

    #[test]
    /// Check we can actually break the cycles.
    fn inter_reference() {
        let rules = parser::parse(
            &r#"
a = "a" - b*
b = "b" - c?
c = a+
"#,
            &file!(),
        )
        .unwrap();
        let used = collect_used_rules(&rules);
        assert_eq!(used, BTreeSet::from(["a", "b", "c"]));
        let graph = collect_reachability(&rules);
        assert_eq!(graph, BTreeMap::from([("b", BTreeSet::from(["a", "c"]))]));
    }
}
