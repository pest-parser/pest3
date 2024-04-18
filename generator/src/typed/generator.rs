use super::{
    attr::parse_derive,
    config::Config,
    module::{ModuleNode, ModuleSystem},
    output::{generics, Output, Tracker},
};
use crate::{common::generate_include, config::collect_data, types::option_type, types::pest};
use pest3_meta::{
    error::rename_meta_rule,
    parser::{
        self, fmt_sep, GrammarModule, Import, ParseExpr, ParseNode, ParseRule, PathArgs, Range,
        Trivia,
    },
};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Display,
    path::PathBuf,
    rc::Rc,
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
        tracker: &mut Tracker<'g>,
        output: &mut Output<'g>,
        config: Config,
        mod_sys: &ModuleSystem<'g>,
        root: &TokenStream,
    ) -> Self {
        match args {
            PathArgs::Call(args) => {
                let args = args
                    .iter()
                    .map(|arg| {
                        process_expr(
                            &arg.expr,
                            rule_config,
                            tracker,
                            output,
                            config,
                            mod_sys,
                            root,
                        )
                    })
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
    fn new(path: &'g [String], args: Option<ProcessedPathArgs>) -> Self {
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
    prefix: &[String],
    inner_type: TokenStream,
    root: TokenStream,
) -> TokenStream {
    let name = &rule_info.rule_id;
    let this = pest();
    let doc = format!(
        "Generated for rule `{}`. Grammar: `{}`.",
        rule_info.rule_name, rule_config.grammar.node,
    );
    let rule = quote! {#root::Rule};
    let args: Vec<_> = rule_config
        .grammar
        .args
        .iter()
        .map(|s| format_ident!("r#{}", s))
        .collect();
    // Pairs inside silent rule will be ignored.
    let pair_api = match (config.no_pair, rule_info.silent) {
        (true, _) => quote! {},
        (false, true) => {
            let rule = (0..=prefix.len())
                .map(|n| {
                    let path = prefix.iter().take(n);
                    let rule = quote! {
                        #root::#(#path::)* Rule
                    };
                    quote! {
                        #[allow(non_camel_case_types)]
                        impl<'i, #(#args, )*> #this::typed::PairContainer<#rule> for #name<'i, #(#args, )*> {
                            fn for_each_child_pair(&self, _f: &mut impl #this::std::FnMut(#this::token::Pair<#rule>)) {}
                            fn for_self_or_for_each_child_pair(&self, _f: &mut impl #this::std::FnMut(#this::token::Pair<#rule>)) {}
                        }
                    }
                })
                ;
            quote! {#(#rule)*}
        }
        (false, false) => {
            let rule = (0..=prefix.len())
                .map(|n| {
                    let supers = prefix.iter().take(n).map(|s|quote!{super::super::});
                    let rule = quote! {
                        super::#(#supers)* Rule
                    };
                    let converts = (0..n).map(|_|quote!{
                        .cvt_into()
                    });
                    quote! {
                        #[allow(non_camel_case_types)]
                        impl<'i, #(#args: #this::typed::PairContainer<#rule>, )*> #this::typed::PairContainer<#rule> for #name<'i, #(#args, )*> {
                            fn for_each_child_pair(&self, f: &mut impl #this::std::FnMut(#this::token::Pair<#rule>)) {
                                self.content.for_self_or_for_each_child_pair(f)
                            }
                            fn for_self_or_for_each_child_pair(&self, f: &mut impl #this::std::FnMut(#this::token::Pair<#rule>)) {
                                use #this::typed::PairTree;
                                f(self.as_pair_tree())
                            }
                        }
                        #[allow(non_camel_case_types)]
                        impl<'i, #(#args: #this::typed::PairContainer<#rule>, )*> #this::typed::PairTree<#rule> for #name<'i, #(#args, )*> {
                            fn get_rule() -> #rule {
                                #[allow(unused_imports)]
                                use #this::typed::SubRule as _;
                                #root::Rule::#name #(#converts)*
                            }
                            fn get_span(&self) -> (#this::std::usize, #this::std::usize) {
                                (self.span.start(), self.span.end())
                            }
                        }
                    }
                })
                ;
            quote! {#(#rule)*}
        }
    };
    let content_type = if rule_info.boxed {
        quote! {#this::std::Box<#inner_type>}
    } else {
        inner_type.clone()
    };
    let typed_node = {
        let rule = (0..=prefix.len()).map(|n| {
            let supers = prefix.iter().take(n).map(|s| quote! {super::super::});
            let rule = quote! {
                super::#(#supers)* Rule
            };
            let mut rule_val = quote!{#root::Rule::#name};
            for _ in 0..n {
                rule_val = quote!{#rule_val.cvt_into()};
            }
            quote! {
                #[allow(unused_imports)]
                use pest3::typed::SubRule as _;
                #this::full_rule_struct!(#name, (#(#args),*), #rule, #rule_val, #inner_type, #content_type, );
            }
        });
        quote! {#(#rule)*}
    };
    quote! {
        #[doc = #doc]
        #[derive(Clone, Debug, Eq, PartialEq)]
        #[allow(non_camel_case_types)]
        pub struct #name<'i, #(#args, )*> {
            /// Matched structure.
            pub content: #content_type,
            /// Matched span.
            pub span: #this::Span<'i>,
        }
        #[allow(non_camel_case_types)]
        impl<'i, #(#args, )*> #this::typed::wrapper::Rule for #name<'i, #(#args, )*> {
            type Rule = #rule;
            const RULE: #rule = #rule::#name;
        }
        #typed_node
        #pair_api
    }
}

#[derive(Clone, Debug)]
pub struct Intermediate {
    pub typename: TokenStream,
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
    tracker: &mut Tracker<'g>,
    output: &mut Output<'g>,
    config: Config,
    mod_sys: &ModuleSystem<'g>,
    root: &TokenStream,
) -> Intermediate {
    fn embed_trivia(trivia: &Trivia) -> TokenStream {
        let this = pest();
        match trivia {
            Trivia::Mandatory => quote! {__MandatoryTrivia::<'i>},
            Trivia::Optional => quote! {__OptionalTrivia::<'i>},
            Trivia::None => quote! {#this::typed::template::Empty},
        }
    }
    let generics = generics();
    match expr {
        ParseExpr::Str(content) => {
            let wrapper = tracker.insert_string_wrapper(content.as_str());
            let typename = quote! {#root::#generics::Str::<#root::#wrapper>};
            Intermediate { typename }
        }
        ParseExpr::Insens(content) => {
            let wrapper = tracker.insert_string_wrapper(content.as_str());
            let typename = quote! {#root::#generics::Insens::<'i, #root::#wrapper>};
            Intermediate { typename }
        }
        ParseExpr::Range(start, end) => {
            let typename = quote! {#root::#generics::CharRange::<#start, #end>};
            Intermediate { typename }
        }
        ParseExpr::Path(prefix, args) => {
            if prefix.len() == 1 && rule_config.grammar.args.contains(&prefix[0]) {
                let ident = format_ident!("r#{}", prefix[0]);
                let typename = quote! {#ident};
                return Intermediate { typename };
            }
            let args = args.as_ref().map(|args| {
                ProcessedPathArgs::process(
                    args,
                    rule_config,
                    tracker,
                    output,
                    config,
                    mod_sys,
                    root,
                )
            });
            let rule_ref = RuleRef::new(prefix, args);
            mod_sys.resolve(&rule_ref, root).unwrap()
        }
        ParseExpr::PosPred(node) => {
            let inner = process_expr(
                &node.expr,
                rule_config,
                tracker,
                output,
                config,
                mod_sys,
                root,
            );
            let inner_typename = &inner.typename;
            let typename = quote! {#root::#generics::Positive::<#inner_typename>};
            Intermediate { typename }
        }
        ParseExpr::NegPred(node) => {
            // Impossible to access inner tokens.
            let inner = process_expr(
                &node.expr,
                rule_config,
                tracker,
                output,
                config,
                mod_sys,
                root,
            );
            let inner_typename = &inner.typename;
            let typename = quote! {#root::#generics::Negative::<#inner_typename>};
            Intermediate { typename }
        }
        ParseExpr::Seq(left, right, trivia) => {
            let vec = collect_sequence(&left.expr, &right.expr, trivia.clone());
            let mut types = Vec::with_capacity(vec.len());
            for (trivia, expr) in vec.into_iter() {
                let child = process_expr(expr, rule_config, tracker, output, config, mod_sys, root);
                types.push((child, trivia));
            }
            let typenames = types.iter().map(|(inter, trivia)| {
                let typename = &inter.typename;
                let trivia = embed_trivia(trivia);
                quote! {#typename, #trivia}
            });

            let seq = format_ident!("Sequence{}", types.len());
            tracker.record_seq(types.len());

            let typename = quote! { #root::#generics::#seq::<#(#typenames, )*> };
            Intermediate { typename }
        }
        ParseExpr::Choice(left, right) => {
            let vec = collect_choices(&left.expr, &right.expr);
            let mut types = vec![];
            for expr in vec.into_iter() {
                let child = process_expr(expr, rule_config, tracker, output, config, mod_sys, root);
                types.push(child);
            }
            let typenames = types.iter().map(|inter| &inter.typename);

            let choice = format_ident!("Choice{}", types.len());
            tracker.record_choice(types.len());

            let typename = quote! { #root::#generics::#choice::<#(#typenames, )*> };
            Intermediate { typename }
        }
        ParseExpr::Opt(inner) => {
            let inner = process_expr(
                &inner.expr,
                rule_config,
                tracker,
                output,
                config,
                mod_sys,
                root,
            );
            let option = option_type();
            let inner_name = &inner.typename;
            let typename = quote! {#option::<#inner_name>};
            Intermediate { typename }
        }
        ParseExpr::Rep(inner_node) => {
            let inner = process_expr(
                &inner_node.expr,
                rule_config,
                tracker,
                output,
                config,
                mod_sys,
                root,
            );
            let inner_name = &inner.typename;
            let trivia = embed_trivia(&get_trivia(&inner_node.expr));
            let typename = quote! { #root::#generics::Rep::<#inner_name, #trivia> };
            Intermediate { typename }
        }
        ParseExpr::RepOnce(inner_node) => {
            let inner = process_expr(
                &inner_node.expr,
                rule_config,
                tracker,
                output,
                config,
                mod_sys,
                root,
            );
            let inner_name = &inner.typename;
            let trivia = embed_trivia(&get_trivia(&inner_node.expr));
            let typename = quote! { #root::#generics::RepOnce::<#inner_name, #trivia> };
            Intermediate { typename }
        }
        ParseExpr::RepRange(inner_node, range) => {
            let inner = process_expr(
                &inner_node.expr,
                rule_config,
                tracker,
                output,
                config,
                mod_sys,
                root,
            );
            let inner_name = &inner.typename;
            let parser::Range { start, end } = range;
            let trivia = embed_trivia(&get_trivia(&inner_node.expr));
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
        ParseExpr::Separated(inner, _trivia) => process_expr(
            &inner.expr,
            rule_config,
            tracker,
            output,
            config,
            mod_sys,
            root,
        ),
    }
}

fn process_rule<'g: 'f, 'f>(
    rule: &'g ParseRule,
    mod_sys: &ModuleSystem<'g>,
    reachability: &BTreeMap<&str, BTreeSet<&str>>,
    config: Config,
    root: &TokenStream,
    prefix: &[String],
    tracker: &mut Tracker<'g>,
    res: &mut Output<'g>,
) {
    let rule_config = RuleConfig::from(rule);
    let inter = process_expr(
        &rule.node.expr,
        &rule_config,
        tracker,
        res,
        config,
        mod_sys,
        root,
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
                prefix,
                inter.typename,
                quote! {super},
            ));
        }
    }
}

fn process_rules<'g>(
    module: &'g GrammarModule,
    mod_sys: &mut ModuleSystem<'g>,
    config: Config,
    global: Rc<ModuleNode<'g>>,
    prefix: &[String],
    root: &TokenStream,
    tracker: &mut Tracker<'g>,
) -> Output<'g> {
    let GrammarModule(rules, _doc, imports) = module;
    let mut new_prefix = prefix.to_owned();
    let modules = imports
        .iter()
        .filter_map(|module| match module {
            Import::Builtin(name, path) => {
                new_prefix.push(name.clone());
                mod_sys.alias(path, name).unwrap();
                new_prefix.pop().unwrap();
                None
            }
            Import::File(name, module) => {
                let mut child_mod_sys = ModuleSystem::new(global.clone());
                new_prefix.push(name.clone());
                let module = (
                    format_ident!("{name}"),
                    process_rules(
                        module,
                        &mut child_mod_sys,
                        config,
                        global.clone(),
                        &new_prefix,
                        &quote! {super::super::#root},
                        tracker,
                    ),
                );
                let imported = child_mod_sys.take();
                mod_sys.insert(imported, name);
                new_prefix.pop().unwrap();
                Some(module)
            }
        })
        .collect();
    let mut output = Output::new(module, modules);
    let reachability = collect_reachability(rules);
    for rule in rules.iter() {
        match rule.name.as_str() {
            "~" | "^" => {}
            _ => mod_sys.insert_rule(rule).unwrap(),
        }
    }
    for rule in rules.iter() {
        process_rule(
            rule,
            mod_sys,
            &reachability,
            config,
            root,
            prefix,
            tracker,
            &mut output,
        );
    }
    output
}

fn collect_used_rule_without_trivia_into<'g>(
    rule: &'g ParseRule,
    used: &mut BTreeSet<&'g str>,
) -> (bool, bool) {
    let mut nodes: Vec<&'g ParseNode> = vec![];
    let mut res = (false, false);
    let (used_optional_trivia, used_mandatory_trivia) = &mut res;
    let mut mark = |trivia: &Trivia| match trivia {
        Trivia::Mandatory => {
            *used_mandatory_trivia = true;
        }
        Trivia::Optional => {
            *used_optional_trivia = true;
        }
        Trivia::None => (),
    };
    let mut f = |expr: &'g parser::ParseNode, nodes: &mut Vec<&'g ParseNode>| match &expr.expr {
        ParseExpr::Str(_) | ParseExpr::Insens(_) | ParseExpr::Range(_, _) => (),
        ParseExpr::PosPred(node) | ParseExpr::NegPred(node) => nodes.push(node),
        ParseExpr::Seq(lhs, rhs, trivia) => {
            nodes.push(lhs);
            nodes.push(rhs);
            mark(trivia);
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
            if let Some(PathArgs::Call(args)) = args {
                // Normally nodes are linked directly.
                nodes.extend(args)
            }
            // Generics from another module is ignored.
            if path.len() == 1 {
                used.insert(&path[0]);
            }
        }
        ParseExpr::Separated(node, trivia) => {
            nodes.push(node);
            mark(trivia);
        }
    };
    f(&rule.node, &mut nodes);
    while let Some(expr) = nodes.pop() {
        if expr == &rule.node {
            continue;
        }
        f(expr, &mut nodes);
    }
    res
}

/// (Whether optional trivia is used, Whether mandatory trivia is used, Used rules except trivias.)
fn collect_used_rule_without_trivia(rule: &'_ ParseRule) -> (bool, bool, BTreeSet<&'_ str>) {
    let mut used = BTreeSet::new();
    let (opt, man) = collect_used_rule_without_trivia_into(rule, &mut used);
    (opt, man, used)
}

/// `'g` refers to grammar.
fn collect_used_rule<'g>(
    rule: &'g ParseRule,
    optional_trivia: Option<&BTreeSet<&'g str>>,
    mandatory_trivia: Option<&BTreeSet<&'g str>>,
    res: &mut BTreeSet<&'g str>,
) {
    let mut nodes: Vec<&'g ParseNode> = vec![];
    let require_trivia = |trivia: Trivia| {
        let expect_trivia = {
            format!(
                "Please define trivia with `{} = \"...\"`. It's used in rule `{}`.",
                trivia, rule.name,
            )
        };
        match trivia {
            Trivia::Mandatory => {
                mandatory_trivia.expect(&expect_trivia);
            }
            Trivia::Optional => {
                optional_trivia.expect(&expect_trivia);
            }
            Trivia::None => (),
        }
    };
    let (opt, man) = collect_used_rule_without_trivia_into(rule, res);
    if opt {
        require_trivia(Trivia::Optional);
    }
    if man {
        require_trivia(Trivia::Mandatory);
    }
}

fn collect_trivia<'g>(
    opt: Option<&'g ParseRule>,
    man: Option<&'g ParseRule>,
) -> (Option<BTreeSet<&'g str>>, Option<BTreeSet<&'g str>>) {
    let mut optional = opt.map(collect_used_rule_without_trivia);
    let mut mandatory = man.map(collect_used_rule_without_trivia);
    if let (Some(optional), Some(mandatory)) = (&mut optional, &mut mandatory) {
        if optional.1 {
            optional.2.extend(mandatory.2.iter());
        }
        if mandatory.0 {
            mandatory.2.extend(optional.2.iter());
        }
    }
    (optional.map(|o| o.2), mandatory.map(|m| m.2))
}

#[cfg(test)]
fn collect_used_rules<'s>(rules: &'s [ParseRule]) -> BTreeSet<&'s str> {
    let mut res = BTreeSet::new();
    let optional_trivia = rules.iter().find(|rule| rule.name == "~");
    let mandatory_trivia = rules.iter().find(|rule| rule.name == "^");

    let (optional, mandatory) = collect_trivia(optional_trivia, mandatory_trivia);

    for rule in rules {
        collect_used_rule(rule, optional.as_ref(), mandatory.as_ref(), &mut res);
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
    let optional_trivia = rules.iter().find(|rule| rule.name == "~");
    let mandatory_trivia = rules.iter().find(|rule| rule.name == "^");

    let (optional, mandatory) = collect_trivia(optional_trivia, mandatory_trivia);

    for rule in rules {
        let entry = res.entry(rule.name.as_str()).or_default();
        collect_used_rule(rule, optional.as_ref(), mandatory.as_ref(), entry);
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

fn generate_typed_pair_from_rule<'g>(
    module: &'g GrammarModule,
    config: Config,
    global: Rc<ModuleNode<'g>>,
    tracker: &mut Tracker<'g>,
) -> TokenStream {
    let mut mod_sys = ModuleSystem::new(global.clone());
    let output = process_rules(
        module,
        &mut mod_sys,
        config,
        global,
        &[],
        &quote! {super},
        tracker,
    );
    let output = output.collect();
    quote! {#output}
}

/// Generate codes for Parser.
fn generate_typed(
    name: Ident,
    generics: &Generics,
    paths: Vec<PathBuf>,
    module: GrammarModule,
    include_grammar: bool,
    impl_parser: bool,
    config: Config,
) -> TokenStream {
    let include_fix = if include_grammar {
        generate_include(&name, paths)
    } else {
        quote!()
    };
    let mut tracker = Tracker::new();
    let global = ModuleSystem::make_global();
    let definition = generate_typed_pair_from_rule(&module, config, global, &mut tracker);
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
        #impl_parser
        #definition
        #tracker
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

    let data = collect_data(contents);

    let module = data
        .iter()
        .map(|(input, root)| {
            let mut root = root.clone().unwrap_or_else(|| {
                PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap_or(".".to_owned()))
            });
            root.push("_.rs");
            match parser::parse(input, &root) {
                Ok(pairs) => pairs,
                Err(error) => panic!("error parsing \n{}", error.renamed_rules(rename_meta_rule)),
            }
        })
        .collect();
    let paths = data.iter().filter_map(|(_, p)| p.clone()).collect();

    generate_typed(
        name,
        &generics,
        paths,
        module,
        include_grammar,
        impl_parser,
        config,
    )
}

#[cfg(test)]
#[allow(unused)]
mod tests {
    use super::*;
    use pest3_meta::parser;
    use std::{
        string::String,
        sync::{Arc, OnceLock},
    };

    static SYNTAX: OnceLock<String> = OnceLock::new();
    static PARSE_RESULT: OnceLock<Arc<GrammarModule>> = OnceLock::new();

    fn get() -> (&'static String, &'static Arc<GrammarModule>) {
        let syntax = SYNTAX.get_or_init(|| {
            String::from_utf8(std::fs::read("tests/syntax.pest").unwrap()).unwrap()
        });
        let parse_result =
            PARSE_RESULT.get_or_init(|| parser::parse(&syntax, &"tests/syntax.pest").unwrap());
        (syntax, parse_result)
    }

    #[test]
    fn inlined_used_rules() {
        let module = parser::parse(
            r#"
x = a - b
a = "a"*
b = "b"+
"#,
            &file!(),
        )
        .unwrap();
        let GrammarModule(rules, _, _) = module.as_ref();
        let used = collect_used_rules(&rules);
        assert_eq!(used, BTreeSet::from(["a", "b"]));
    }

    #[test]
    /// Check we can actually break the cycles.
    fn inter_reference() {
        let module = parser::parse(
            &r#"
a = "a" - b*
b = "b" - c?
c = a+
"#,
            &file!(),
        )
        .unwrap();
        let GrammarModule(rules, _, _) = module.as_ref();
        let used = collect_used_rules(&rules);
        assert_eq!(used, BTreeSet::from(["a", "b", "c"]));
        let graph = collect_reachability(&rules);
        assert_eq!(graph, BTreeMap::from([("b", BTreeSet::from(["a", "c"]))]));
    }
}
