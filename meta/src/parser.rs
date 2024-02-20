#![allow(clippy::result_large_err)]
use std::{
    char,
    fmt::{write, Display},
    fs::File,
    io::Read,
    mem,
    ops::{Bound, RangeBounds},
    path::{Path, PathBuf},
    rc::Rc,
    str::FromStr,
};

use pest::{
    error::{Error, ErrorVariant},
    iterators::{Pair, Pairs},
    pratt_parser::{Assoc, Op, PrattParser},
    Parser,
};

pub(crate) mod grammar {
    use pest_derive::Parser;

    #[derive(Parser)]
    #[grammar = "grammar.pest"]
    #[non_exhaustive]
    pub struct Parser;
}

use grammar::Rule;

use crate::doc::DocComment;

#[derive(Clone, Debug, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub struct Span {
    pub path: PathBuf,
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn start(&self, start: usize) -> Self {
        Self {
            path: self.path.clone(),
            start,
            end: self.end,
        }
    }

    pub fn end(&self, end: usize) -> Self {
        Self {
            path: self.path.clone(),
            start: self.start,
            end,
        }
    }

    pub fn start_end(&self, start: usize, end: usize) -> Self {
        Self {
            path: self.path.clone(),
            start,
            end,
        }
    }

    pub fn from_pest(&self, span: pest::Span) -> Self {
        self.start_end(span.start(), span.end())
    }

    pub fn union(&self, other: &Self) -> Self {
        Self {
            path: self.path.clone(),
            start: self.start,
            end: other.end,
        }
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub struct Range<T> {
    pub start: Option<T>,
    pub end: Option<T>,
}

impl<T: Copy> Range<T> {
    pub fn from<R: RangeBounds<T>>(range: R) -> Range<T> {
        let start = match range.start_bound() {
            Bound::Included(start) => Some(*start),
            Bound::Unbounded => None,
            _ => unimplemented!(),
        };

        let end = match range.end_bound() {
            Bound::Excluded(end) => Some(*end),
            Bound::Unbounded => None,
            _ => unimplemented!(),
        };

        Range { start, end }
    }
}

impl<T: Display> Display for Range<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(start) = &self.start {
            write!(f, "{}", start)?;
        }
        write!(f, "..")?;
        if let Some(end) = &self.end {
            write!(f, "{}", end)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct ParseRule {
    pub doc: Vec<String>,
    pub name: String,
    pub args: Vec<String>,
    pub span: Span,
    pub node: ParseNode,
    pub silent: bool,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub struct ParseNode {
    pub expr: ParseExpr,
    pub span: Span,
}

impl Display for ParseNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expr)
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub enum PathArgs {
    Call(Vec<ParseNode>),
    Slice(Range<isize>),
}

impl Display for PathArgs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Call(args) => {
                write!(f, "(")?;
                fmt_sep(args, ", ", f)?;
                write!(f, ")")
            }
            Self::Slice(range) => write!(f, "[{}]", range),
        }
    }
}

#[inline]
pub fn fmt_sep<T: Display>(
    vec: &[T],
    sep: &str,
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    let mut iter = vec.iter();
    if let Some(first) = iter.next() {
        write!(f, "{}", first)?;
    }
    for res in iter {
        write!(f, "{}{}", sep, res)?;
    }
    Ok(())
}

#[derive(Clone, Debug, Hash, Default, Eq, PartialEq, PartialOrd, Ord)]
pub enum Trivia {
    /// Without trivia.
    #[default]
    None,
    /// With optional trivia.
    Optional,
    /// With mandatory trivia.
    Mandatory,
}

impl Trivia {
    fn get_repr(&self) -> &'static str {
        match self {
            Self::None => "-",
            Self::Optional => "~",
            Self::Mandatory => "^",
        }
    }
    pub fn get_code(&self) -> u8 {
        match self {
            Self::None => 0,
            Self::Optional => 1,
            Self::Mandatory => 2,
        }
    }
}

impl Display for Trivia {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.get_repr())
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub enum ParseExpr {
    Str(String),
    Insens(String),
    Range(char, char),
    Path(Vec<String>, Option<PathArgs>),
    PosPred(Box<ParseNode>),
    NegPred(Box<ParseNode>),
    Seq(Box<ParseNode>, Box<ParseNode>, Trivia),
    Choice(Box<ParseNode>, Box<ParseNode>),
    Opt(Box<ParseNode>),
    Rep(Box<ParseNode>),
    RepOnce(Box<ParseNode>),
    RepRange(Box<ParseNode>, Range<usize>),
    Separated(Box<ParseNode>, Trivia),
}

impl Display for ParseExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Str(str) => write!(f, "{:?}", str),
            Self::Insens(str) => write!(f, "^{:?}", str),
            Self::Range(start, end) => write!(f, "{:?}..{:?}", start, end),
            Self::Path(path, args) => {
                fmt_sep(path, "::", f)?;
                if let Some(args) = args {
                    write!(f, "{}", args)?;
                }
                Ok(())
            }
            Self::PosPred(inner) => write!(f, "&{}", inner),
            Self::NegPred(inner) => write!(f, "!{}", inner),
            Self::Seq(lhs, rhs, trivia) => write!(f, "({} {} {})", lhs, trivia, rhs),
            Self::Choice(lhs, rhs) => write!(f, "({} | {})", lhs, rhs),
            Self::Opt(inner) => write!(f, "{}?", inner),
            Self::Rep(inner) => write!(f, "{}*", inner),
            Self::RepOnce(inner) => write!(f, "{}+", inner),
            Self::RepRange(inner, range) => write!(f, "{}[{}]", inner, range),
            Self::Separated(inner, trivia) => write!(f, "{}{}", inner, trivia),
        }
    }
}

fn skip(rule: Rule, pairs: &mut Pairs<Rule>) {
    if let Some(current_rule) = pairs.peek().map(|pair| pair.as_rule()) {
        if current_rule == rule {
            pairs
                .next()
                .unwrap_or_else(|| panic!("expected {:?}", rule));
        }
    }
}

fn _parse<P: AsRef<Path>>(
    input: &str,
    root: &P,
    rules: &mut Vec<ParseRule>,
    doc: &mut DocComment,
    module: Option<&str>,
) -> Result<(), Error<Rule>> {
    let pairs = grammar::Parser::parse(Rule::grammar_rules, input)?;

    for pair in pairs {
        match pair.as_rule() {
            Rule::import => {
                let mut pairs = pair.into_inner();

                let path_pair = pairs.next().expect("import Pair must contain path");
                let path_string = string_content(&path_pair)?;
                if path_string.starts_with("pest") {
                    continue;
                }
                let path = Path::new(&path_string);

                let path = if path.is_relative() {
                    root.as_ref().join(path)
                } else {
                    path.to_path_buf()
                };
                // FIXME: provide an alternative for pest_vm / web environments
                if let Ok(mut file) = File::open(&path) {
                    let root = path.parent().expect("path cannot be root");
                    let mut input = String::new();

                    if file.read_to_string(&mut input).is_err() {
                        return Err(Error::new_from_span(
                            ErrorVariant::CustomError {
                                message: format!("cannot read from '{}'", path_string),
                            },
                            path_pair.as_span(),
                        ));
                    }

                    let mut is_aliased = false;

                    if let Some(pair) = pairs.next() {
                        if pair.as_rule() == Rule::identifier {
                            is_aliased = true;

                            match module {
                                Some(module) => _parse(
                                    &input,
                                    &root,
                                    rules,
                                    doc,
                                    Some(&format!("{}::{}", module, pair.as_str())),
                                )?,
                                None => _parse(&input, &root, rules, doc, Some(pair.as_str()))?,
                            };
                        }
                    }

                    if !is_aliased {
                        _parse(&input, &root, rules, doc, module)?;
                    }
                } else {
                    return Err(Error::new_from_span(
                        ErrorVariant::CustomError {
                            message: format!("cannot open '{}'", path_string),
                        },
                        path_pair.as_span(),
                    ));
                }
            }
            Rule::grammar_rule => {
                rules.push(parse_rule(pair, root.as_ref().to_path_buf())?);
            }
            Rule::grammar_doc => doc.grammar_doc.push(parse_grammar_doc(pair)?),
            _ => (),
        }
    }

    Ok(())
}

pub fn parse<P: AsRef<Path>>(input: &str, root: &P) -> Result<Vec<ParseRule>, Error<Rule>> {
    let mut rules = vec![];
    let mut doc = DocComment::default();

    _parse(input, root, &mut rules, &mut doc, None)?;

    Ok(rules)
}

pub fn parse_with_doc_comment<P: AsRef<Path>>(
    input: &str,
    root: &P,
) -> Result<(Vec<ParseRule>, DocComment), Error<Rule>> {
    let mut rules = vec![];
    let mut doc = DocComment::default();

    _parse(input, root, &mut rules, &mut doc, None)?;

    Ok((rules, doc))
}

fn parse_rule(rule: Pair<Rule>, path: PathBuf) -> Result<ParseRule, Error<Rule>> {
    let span = rule.as_span();
    let span = Span {
        path,
        start: span.start(),
        end: span.end(),
    };

    let mut pairs = rule.into_inner();

    let mut doc = vec![];
    while let Some(pair) = pairs.peek() {
        if pair.as_rule() == Rule::rule_doc {
            doc.push(parse_rule_doc(
                pairs
                    .next()
                    .expect("Peek says there is still at least one more pair."),
            )?);
        } else {
            break;
        }
    }

    let name = pairs
        .next()
        .expect("expected Rule::identifier")
        .as_str()
        .to_string();

    let args: Vec<_> = pairs
        .next()
        .unwrap()
        .into_inner()
        .filter(|pair| pair.as_rule() == Rule::identifier)
        .map(|pair| pair.as_str().to_string())
        .collect();

    let silent = matches!(pairs.peek().unwrap().as_rule(), Rule::silent_modifier);

    if silent {
        pairs.next().unwrap(); // modifier
    }
    skip(Rule::opening_brace, &mut pairs);
    skip(Rule::assignment_operator, &mut pairs);

    let pratt_parser = PrattParser::new()
        .op(Op::infix(Rule::choice_operator, Assoc::Right))
        .op(Op::infix(Rule::sequence_operator, Assoc::Right)
            | Op::infix(Rule::tilde_operator, Assoc::Right)
            | Op::infix(Rule::caret_operator, Assoc::Right));
    let node = parse_node(pairs.next().unwrap(), &span, &pratt_parser)?;

    Ok(ParseRule {
        doc,
        name,
        args,
        span,
        node,
        silent,
    })
}

fn parse_prefix(pair: Pair<Rule>, child: ParseNode) -> ParseNode {
    let span = child.span.start(pair.as_span().start());
    let expr = match pair.as_rule() {
        Rule::positive_predicate_operator => ParseExpr::PosPred(Box::new(child)),
        Rule::negative_predicate_operator => ParseExpr::NegPred(Box::new(child)),
        _ => unreachable!(),
    };

    ParseNode { expr, span }
}

fn parse_path(
    pair: Pair<Rule>,
    span: &Span,
    pratt_parser: &PrattParser<Rule>,
) -> Result<ParseNode, Error<Rule>> {
    let span = span.from_pest(pair.as_span());
    let mut path = vec![];

    let mut pairs = pair.into_inner();

    while let Some(Rule::identifier) = pairs.peek().map(|pair| pair.as_rule()) {
        path.push(pairs.next().unwrap().as_str().to_string());
        skip(Rule::ancestry_operator, &mut pairs);
    }

    let args = if let Some(pair) = pairs.next() {
        match pair.as_rule() {
            Rule::call => {
                let mut args = vec![];
                let mut pairs = pair.into_inner();

                pairs.next().unwrap(); // opening_paren

                while let Some(Rule::expression) = pairs.peek().map(|pair| pair.as_rule()) {
                    args.push(parse_node(pairs.next().unwrap(), &span, pratt_parser)?);
                    skip(Rule::comma, &mut pairs);
                }

                Some(PathArgs::Call(args))
            }
            Rule::slice => Some(PathArgs::Slice(parse_range(&mut pair.into_inner())?)),
            _ => unreachable!(),
        }
    } else {
        None
    };

    let expr = ParseExpr::Path(path, args);

    Ok(ParseNode { expr, span })
}

fn parse_grammar_doc(pair: Pair<Rule>) -> Result<String, Error<Rule>> {
    let string = &pair.as_str()[3..];
    let string = string.trim();
    let content = Some(string).ok_or(Error::new_from_span(
        ErrorVariant::CustomError {
            message: "incorrect grammar doc comment".to_string(),
        },
        pair.as_span(),
    ))?;

    Ok(content.to_string())
}

fn parse_rule_doc(pair: Pair<Rule>) -> Result<String, Error<Rule>> {
    let string = &pair.as_str()[3..];
    let string = string.trim();
    let content = Some(string).ok_or(Error::new_from_span(
        ErrorVariant::CustomError {
            message: "incorrect rule doc comment".to_string(),
        },
        pair.as_span(),
    ))?;

    Ok(content.to_string())
}

fn parse_string(pair: Pair<Rule>, span: &Span) -> Result<ParseNode, Error<Rule>> {
    let string = pair.as_str();
    let content = unescape(&string[1..string.len() - 1]).ok_or(Error::new_from_span(
        ErrorVariant::CustomError {
            message: "incorrect string literal".to_string(),
        },
        pair.as_span(),
    ))?;

    let expr = ParseExpr::Str(content.to_owned());
    let span = span.from_pest(pair.as_span());

    Ok(ParseNode { expr, span })
}

fn parse_raw_string(pair: Pair<Rule>, span: &Span) -> ParseNode {
    let span = span.from_pest(pair.as_span());
    let mut pairs = pair.into_inner();

    pairs.next().unwrap(); // quote

    let content = pairs.next().unwrap().as_str();
    let expr = ParseExpr::Str(content.to_owned());

    ParseNode { expr, span }
}

fn parse_insensitive_string(pair: Pair<Rule>, span: &Span) -> Result<ParseNode, Error<Rule>> {
    let string = pair.as_str();
    let content = unescape(&string[2..string.len() - 1]).ok_or(Error::new_from_span(
        ErrorVariant::CustomError {
            message: "incorrect string literal".to_string(),
        },
        pair.as_span(),
    ))?;

    let expr = ParseExpr::Insens(content.to_owned());
    let span = span.from_pest(pair.as_span());

    Ok(ParseNode { expr, span })
}

fn parse_char_range(pair: Pair<Rule>, span: &Span) -> Result<ParseNode, Error<Rule>> {
    let span = span.from_pest(pair.as_span());
    let mut pairs = pair.into_inner();

    let pair = pairs.next().unwrap();
    let string = pair.as_str();
    let content_start = unescape(&string[1..string.len() - 1]).ok_or(Error::new_from_span(
        ErrorVariant::CustomError {
            message: "incorrect char literal".to_string(),
        },
        pair.as_span(),
    ))?;

    pairs.next().unwrap(); // range_operator

    let pair = pairs.next().unwrap();
    let string = pair.as_str();
    let content_end = unescape(&string[1..string.len() - 1]).ok_or(Error::new_from_span(
        ErrorVariant::CustomError {
            message: "incorrect char literal".to_string(),
        },
        pair.as_span(),
    ))?;

    let expr = ParseExpr::Range(
        content_start.chars().next().unwrap(),
        content_end.chars().next().unwrap(),
    );

    Ok(ParseNode { expr, span })
}

fn pair_to<F: FromStr>(pair: Pair<Rule>) -> Result<F, Error<Rule>> {
    if let Ok(number) = pair.as_str().parse() {
        Ok(number)
    } else {
        Err(Error::new_from_span(
            ErrorVariant::CustomError {
                message: format!("number cannot overflow {} bytes", mem::size_of::<F>()),
            },
            pair.as_span(),
        ))
    }
}

fn parse_range<F: FromStr + Copy>(pairs: &mut Pairs<Rule>) -> Result<Range<F>, Error<Rule>> {
    pairs.next().unwrap(); // opening_brace | opening_brack

    let rules: Vec<_> = pairs.clone().map(|pair| pair.as_rule()).collect();
    let numbers = pairs
        .filter(|pair| pair.as_rule() == Rule::number || pair.as_rule() == Rule::integer)
        .map(pair_to::<F>)
        .collect::<Result<Vec<_>, _>>()?;

    let range = match &rules[..] {
        // a..b}
        [_, Rule::range_operator, _, _] => Range {
            start: Some(numbers[0]),
            end: Some(numbers[1]),
        },
        // a..}
        [_, Rule::range_operator, _] => Range {
            start: Some(numbers[0]),
            end: None,
        },
        // ..b}
        [Rule::range_operator, _, _] => Range {
            start: None,
            end: Some(numbers[0]),
        },
        // ..}
        [Rule::range_operator, _] => Range {
            start: None,
            end: None,
        },
        // a}
        [_, _] => Range {
            start: Some(numbers[0]),
            end: Some(numbers[0]),
        },
        _ => unreachable!(),
    };
    Ok(range)
}

fn parse_bounded_repeat(pair: Pair<Rule>, child: ParseNode) -> Result<ParseExpr, Error<Rule>> {
    fn inner_span(pair: Pair<Rule>) -> pest::Span {
        let mut pairs = pair.into_inner();

        pairs.next().unwrap(); // opening_brace
        let start = pairs.peek().unwrap().as_span().start_pos();

        let mut pairs = pairs.rev();
        pairs.next().unwrap(); // closing_brace
        let end = pairs.next().unwrap().as_span().end_pos();

        start.span(&end)
    }

    let range = parse_range(&mut pair.clone().into_inner())?;

    if let Range {
        start: Some(start),
        end: Some(end),
    } = range
    {
        if start > end {
            return Err(Error::new_from_span(
                ErrorVariant::CustomError {
                    message: format!("repetition range void ({} > {})", start, end),
                },
                inner_span(pair),
            ));
        }
    }

    if let Some(0) = range.end {
        return Err(Error::new_from_span(
            ErrorVariant::CustomError {
                message: "cannot repeat 0 times".to_string(),
            },
            inner_span(pair),
        ));
    }

    Ok(ParseExpr::RepRange(Box::new(child), range))
}

fn parse_postfix(pair: Pair<Rule>, child: ParseNode) -> Result<ParseNode, Error<Rule>> {
    let span = match pair.as_rule() {
        Rule::overridable_operator => child.span.start(pair.as_span().start()),
        _ => child.span.end(pair.as_span().end()),
    };
    let expr = match pair.as_rule() {
        Rule::optional_operator => ParseExpr::Opt(Box::new(child)),
        Rule::tilde_operator => ParseExpr::Separated(Box::new(child), Trivia::Optional),
        Rule::caret_operator => ParseExpr::Separated(Box::new(child), Trivia::Mandatory),
        Rule::repeat_operator => ParseExpr::Rep(Box::new(child)),
        Rule::repeat_once_operator => ParseExpr::RepOnce(Box::new(child)),
        Rule::bounded_repeat => parse_bounded_repeat(pair, child)?,
        _ => unreachable!(),
    };

    Ok(ParseNode { expr, span })
}

fn parse_term(
    pair: Pair<Rule>,
    span: &Span,
    pratt_parser: &PrattParser<Rule>,
) -> Result<ParseNode, Error<Rule>> {
    let mut prefix_pairs = vec![];
    let mut postfix_pairs = vec![];
    let mut expression = None;

    for pair in pair.into_inner() {
        match pair.as_rule() {
            Rule::positive_predicate_operator | Rule::negative_predicate_operator => {
                prefix_pairs.push(pair)
            }
            Rule::optional_operator
            | Rule::tilde_operator
            | Rule::caret_operator
            | Rule::repeat_operator
            | Rule::repeat_once_operator
            | Rule::bounded_repeat => postfix_pairs.push(pair),
            Rule::opening_paren | Rule::closing_paren => (),
            Rule::expression => expression = Some(parse_node(pair, span, pratt_parser)?),
            Rule::path => expression = Some(parse_path(pair, span, pratt_parser)?),
            Rule::string => expression = Some(parse_string(pair, span)?),
            Rule::raw_string => expression = Some(parse_raw_string(pair, span)),
            Rule::insensitive_string => expression = Some(parse_insensitive_string(pair, span)?),
            Rule::range => expression = Some(parse_char_range(pair, span)?),
            _ => unreachable!(),
        }
    }

    let expression = postfix_pairs
        .into_iter()
        .try_fold(expression.unwrap(), |child, pair| {
            parse_postfix(pair, child)
        });

    Ok(prefix_pairs
        .into_iter()
        .rfold(expression?, |child, pair| parse_prefix(pair, child)))
}

fn parse_node(
    expr: Pair<Rule>,
    span: &Span,
    pratt_parser: &PrattParser<Rule>,
) -> Result<ParseNode, Error<Rule>> {
    let infix = |lhs: Result<ParseNode, Error<Rule>>,
                 op: Pair<Rule>,
                 rhs: Result<ParseNode, Error<Rule>>| {
        let lhs = lhs?;
        let rhs = rhs?;

        let span = lhs.span.union(&rhs.span);
        let expr = match op.as_rule() {
            Rule::sequence_operator => ParseExpr::Seq(Box::new(lhs), Box::new(rhs), Trivia::None),
            Rule::tilde_operator => ParseExpr::Seq(Box::new(lhs), Box::new(rhs), Trivia::Optional),
            Rule::caret_operator => ParseExpr::Seq(Box::new(lhs), Box::new(rhs), Trivia::Mandatory),
            Rule::choice_operator => ParseExpr::Choice(Box::new(lhs), Box::new(rhs)),
            _ => unreachable!(),
        };

        Ok(ParseNode { expr, span })
    };
    let mut pairs = expr.into_inner();

    skip(Rule::choice_operator, &mut pairs);
    pratt_parser
        .map_primary(|pair| parse_term(pair, span, pratt_parser))
        .map_infix(move |lhs, op, rhs| infix(lhs, op, rhs))
        .parse(pairs)
}

fn string_content(pair: &Pair<Rule>) -> Result<String, Error<Rule>> {
    let string = unescape(pair.as_str()).ok_or(Error::new_from_span(
        ErrorVariant::CustomError {
            message: "invalid string literal".to_string(),
        },
        pair.as_span(),
    ))?;
    Ok(string)
}

fn unescape(string: &str) -> Option<String> {
    let mut result = String::new();
    let mut chars = string.chars();

    loop {
        match chars.next() {
            Some('\\') => match chars.next()? {
                '"' => result.push('"'),
                '\\' => result.push('\\'),
                'r' => result.push('\r'),
                'n' => result.push('\n'),
                't' => result.push('\t'),
                '0' => result.push('\0'),
                '\'' => result.push('\''),
                'x' => {
                    let string: String = chars.clone().take(2).collect();

                    if string.len() != 2 {
                        return None;
                    }

                    for _ in 0..string.len() {
                        chars.next()?;
                    }

                    let value = u8::from_str_radix(&string, 16).ok()?;

                    if value > 0x7f {
                        return None;
                    }

                    result.push(char::from(value));
                }
                'u' => {
                    if chars.next()? != '{' {
                        return None;
                    }

                    let string: String = chars.clone().take_while(|c| *c != '}').collect();

                    if string.is_empty() || 6 < string.len() {
                        return None;
                    }

                    for _ in 0..string.len() + 1 {
                        chars.next()?;
                    }

                    let value = u32::from_str_radix(&string, 16).ok()?;

                    if value > 0x10ffff {
                        return None;
                    }

                    result.push(char::from_u32(value)?);
                }
                _ => return None,
            },
            Some('"') => (),
            Some(c) => result.push(c),
            None => return Some(result),
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pest::{consumes_to, fails_with, parses_to};

    type PestParser = grammar::Parser;

    #[test]
    fn grammar_test() {
        let input = include_str!("../tests/pest3sample.pest");
        let parsed = parse_with_doc_comment(input, &Path::new("../tests/pest3sample.pest"));
        let (rules, doc_comment) = match parsed {
            Ok(parsed) => parsed,
            Err(err) => panic!("{err}"),
        };
        assert_ne!(rules.len(), 0);
        assert_ne!(doc_comment.grammar_doc.len(), 0);
    }

    #[test]
    fn rules() {
        parses_to! {
            parser: PestParser,
            input: "a = b  c = d",
            rule: Rule::grammar_rules,
            tokens: [
                grammar_rule(0, 7, [
                    identifier(0, 1),
                    assignment_operator(2, 3),
                    expression(4, 7, [
                        term(4, 7, [
                            path(4, 7, [
                                identifier(4, 5)
                            ])
                        ])
                    ])
                ]),
                grammar_rule(7, 12, [
                    identifier(7, 8),
                    assignment_operator(9, 10),
                    expression(11, 12, [
                        term(11, 12, [
                            path(11, 12, [
                                identifier(11, 12)
                            ])
                        ])
                    ])
                ])
            ]
        };
    }

    #[test]
    fn rules_with_braces() {
        parses_to! {
            parser: PestParser,
            input: "a = { b } c = { d }",
            rule: Rule::grammar_rules,
            tokens: [
                grammar_rule(0, 9, [
                    identifier(0, 1),
                    assignment_operator(2, 3),
                    opening_brace(4, 5),
                    expression(6, 8, [
                        term(6, 8, [
                            path(6, 8, [
                                identifier(6, 7)
                            ])
                        ])
                    ]),
                    closing_brace(8, 9)
                ]),
                grammar_rule(10, 19, [
                    identifier(10, 11),
                    assignment_operator(12, 13),
                    opening_brace(14, 15),
                    expression(16, 18, [
                        term(16, 18, [
                            path(16, 18, [
                                identifier(16, 17)
                            ])
                        ])
                    ]),
                    closing_brace(18, 19)
                ])
            ]
        };
    }

    #[test]
    fn rule() {
        parses_to! {
            parser: PestParser,
            input: "a = b - c",
            rule: Rule::grammar_rule,
            tokens: [
                grammar_rule(0, 9, [
                    identifier(0, 1),
                    assignment_operator(2, 3),
                    expression(4, 9, [
                        term(4, 6, [
                            path(4, 6, [
                                identifier(4, 5)
                            ])
                        ]),
                        sequence_operator(6, 7),
                        term(8, 9, [
                            path(8, 9, [
                                identifier(8, 9)
                            ])
                        ])
                    ])
                ])
            ]
        };
    }

    #[test]
    fn rule_with_brace() {
        parses_to! {
            parser: PestParser,
            input: "a = { b - c }",
            rule: Rule::grammar_rule,
            tokens: [
                grammar_rule(0, 13, [
                    identifier(0, 1),
                    assignment_operator(2, 3),
                    opening_brace(4, 5),
                    expression(6, 12, [
                        term(6, 8, [
                            path(6, 8, [
                                identifier(6, 7)
                            ])
                        ]),
                        sequence_operator(8, 9),
                        term(10, 12, [
                            path(10, 12, [
                                identifier(10, 11)
                            ])
                        ])
                    ]),
                    closing_brace(12, 13)
                ])
            ]
        };
    }

    #[test]
    fn path() {
        parses_to! {
            parser: PestParser,
            input: "a::b :: c",
            rule: Rule::path,
            tokens: [
                path(0, 9, [
                    identifier(0, 1),
                    ancestry_operator(1, 3),
                    identifier(3, 4),
                    ancestry_operator(5, 7),
                    identifier(8, 9)
                ])
            ]
        };
    }

    #[test]
    fn path_with_args() {
        parses_to! {
            parser: PestParser,
            input: "a(b, c)",
            rule: Rule::path,
            tokens: [
                path(0, 7, [
                    identifier(0, 1),
                    call(1, 7, [
                        opening_paren(1, 2),
                        expression(2, 3, [
                            term(2, 3, [
                                path(2, 3, [
                                    identifier(2, 3)
                                ])
                            ])
                        ]),
                        comma(3, 4),
                        expression(5, 6, [
                            term(5, 6, [
                                path(5, 6, [
                                    identifier(5, 6)
                                ])
                            ])
                        ]),
                        closing_paren(6, 7)
                    ])
                ])
            ]
        };
    }

    #[test]
    fn path_with_slice() {
        parses_to! {
            parser: PestParser,
            input: "a[-1..-2]",
            rule: Rule::path,
            tokens: [
                path(0, 9, [
                    identifier(0, 1),
                    slice(1, 9, [
                        opening_brack(1, 2),
                        integer(2, 4),
                        range_operator(4, 6),
                        integer(6, 8),
                        closing_brack(8, 9)
                    ])
                ])
            ]
        };
    }

    #[test]
    fn string() {
        parses_to! {
            parser: PestParser,
            input: r#""hello""#,
            rule: Rule::string,
            tokens: [
                string(0, 7, [
                    quote(0, 1),
                    inner_str(1, 6),
                    quote(6, 7)
                ])
            ]
        };
    }

    #[test]
    fn raw_string_zero_pounds() {
        parses_to! {
            parser: PestParser,
            input: r#"r"hello""#,
            rule: Rule::raw_string,
            tokens: [
                raw_string(0, 8, [
                    quote(1, 2),
                    inner_raw_str(2, 7),
                    quote(7, 8)
                ])
            ]
        };
    }

    #[test]
    fn raw_string_one_pound() {
        parses_to! {
            parser: PestParser,
            input: r##"r#"hello"#"##,
            rule: Rule::raw_string,
            tokens: [
                raw_string(0, 10, [
                    pound(1, 2),
                    quote(2, 3),
                    inner_raw_str(3, 8),
                    quote(8, 9)
                ])
            ]
        };
    }

    #[test]
    fn raw_string_two_pounds() {
        parses_to! {
            parser: PestParser,
            input: r###"r##"hello"##"###,
            rule: Rule::raw_string,
            tokens: [
                raw_string(0, 12, [
                    pound(1, 2),
                    pound(2, 3),
                    quote(3, 4),
                    inner_raw_str(4, 9),
                    quote(9, 10)
                ])
            ]
        };
    }

    #[test]
    fn insensitive_string() {
        parses_to! {
            parser: PestParser,
            input: r#"i"hello""#,
            rule: Rule::insensitive_string,
            tokens: [
                insensitive_string(0, 8, [
                    string(1, 8, [
                        quote(1, 2),
                        inner_str(2, 7),
                        quote(7, 8)
                    ])
                ])
            ]
        };
    }

    #[test]
    fn range() {
        parses_to! {
            parser: PestParser,
            input: r"'\n' .. '\x1a'",
            rule: Rule::range,
            tokens: [
                range(0, 14, [
                    character(0, 4, [
                        single_quote(0, 1),
                        inner_chr(1, 3),
                        single_quote(3, 4)
                    ]),
                    range_operator(5, 7),
                    character(8, 14, [
                        single_quote(8, 9),
                        inner_chr(9, 13),
                        single_quote(13, 14)
                    ])
                ])
            ]
        };
    }

    #[test]
    fn comment() {
        parses_to! {
            parser: PestParser,
            input: "a ~    // asda\n b",
            rule: Rule::expression,
            tokens: [
                expression(0, 17, [
                    term(0, 2, [
                        path(0, 2, [
                            identifier(0, 1)
                        ])
                    ]),
                    tilde_operator(2, 3),
                    term(16, 17, [
                        path(16, 17, [
                            identifier(16, 17)
                        ])
                    ])
                ])
            ]
        };
    }

    #[test]
    fn wrong_identifier() {
        fails_with! {
            parser: PestParser,
            input: "0",
            rule: Rule::grammar_rules,
            positives: vec![
                Rule::import,
                Rule::grammar_rule,
                Rule::grammar_doc
            ],
            negatives: vec![],
            pos: 0
        };
    }

    #[test]
    fn missing_assignment_operator() {
        fails_with! {
            parser: PestParser,
            input: "a ",
            rule: Rule::grammar_rules,
            positives: vec![
                Rule::assignment_operator,
                Rule::opening_paren
            ],
            negatives: vec![],
            pos: 2
        };
    }

    #[test]
    fn wrong_modifier() {
        fails_with! {
            parser: PestParser,
            input: "a = *",
            rule: Rule::grammar_rules,
            positives: vec![
                Rule::opening_brace,
                Rule::silent_modifier,
                Rule::expression
            ],
            negatives: vec![],
            pos: 4
        };
    }

    #[test]
    fn empty_rule() {
        fails_with! {
            parser: PestParser,
            input: "a = {}",
            rule: Rule::grammar_rules,
            positives: vec![Rule::expression],
            negatives: vec![],
            pos: 5
        };
    }

    #[test]
    fn missing_rhs() {
        fails_with! {
            parser: PestParser,
            input: "a = b -",
            rule: Rule::grammar_rules,
            positives: vec![Rule::term],
            negatives: vec![],
            pos: 7
        };
    }

    #[test]
    fn wrong_op() {
        fails_with! {
            parser: PestParser,
            input: "a = b %",
            rule: Rule::grammar_rules,
            positives: vec![
                Rule::EOI,
                Rule::import,
                Rule::grammar_rule,
                Rule::opening_brace,
                Rule::opening_paren,
                Rule::opening_brack,
                Rule::sequence_operator,
                Rule::choice_operator,
                Rule::optional_operator,
                Rule::repeat_operator,
                Rule::repeat_once_operator,
                Rule::tilde_operator,
                Rule::caret_operator,
                Rule::ancestry_operator
            ],
            negatives: vec![],
            pos: 6
        };
    }

    #[test]
    fn string_missing_ending_quote() {
        fails_with! {
            parser: PestParser,
            input: r#"a = ""#,
            rule: Rule::grammar_rules,
            positives: vec![Rule::quote],
            negatives: vec![],
            pos: 5
        };
    }

    #[test]
    fn char_missing_ending_single_quote() {
        fails_with! {
            parser: PestParser,
            input: "a = ' ",
            rule: Rule::grammar_rules,
            positives: vec![Rule::single_quote],
            negatives: vec![],
            pos: 6
        };
    }

    #[test]
    fn range_missing_range_operator() {
        fails_with! {
            parser: PestParser,
            input: "a = 'a'",
            rule: Rule::grammar_rules,
            positives: vec![Rule::range_operator],
            negatives: vec![],
            pos: 7
        };
    }

    #[test]
    fn args_missing_comma() {
        fails_with! {
            parser: PestParser,
            input: "a(b c)",
            rule: Rule::grammar_rules,
            positives: vec![
                Rule::closing_paren,
                Rule::comma
            ],
            negatives: vec![],
            pos: 4
        };
    }

    fn child() -> ParseNode {
        ParseNode {
            expr: ParseExpr::Str("".to_string()),
            span: span("", 0),
        }
    }

    fn span(input: &str, offset: usize) -> Span {
        Span {
            start: offset,
            end: offset + input.len(),
            path: PathBuf::from("span"),
        }
    }

    fn climber() -> PrattParser<Rule> {
        PrattParser::new()
            .op(Op::infix(Rule::choice_operator, Assoc::Right))
            .op(Op::infix(Rule::sequence_operator, Assoc::Right)
                | Op::infix(Rule::tilde_operator, Assoc::Right)
                | Op::infix(Rule::caret_operator, Assoc::Right))
    }

    macro_rules! expr {
        ( Str( $string:expr ) ) => {
            ParseExpr::Str($string.to_string())
        };

        ( Insens( $string:expr ) ) => {
            ParseExpr::Insens($string.to_string())
        };

        ( Range( $start:expr, $end:expr ) ) => {
            ParseExpr::Range($start, $end)
        };

        ( Path( [ $( $ident:expr ),+ ] ) ) => {
            ParseExpr::Path(
                vec![ $( $ident.to_string() ),+ ],
                None
            )
        };

        ( Path( [ $( $ident:expr ),+ ], Call( $( $args:expr ),* ) ) ) => {
            ParseExpr::Path(
                vec![ $( $ident.to_string() ),+ ],
                Some(PathArgs::Call(vec![ $( $args ),* ]))
            )
        };

        ( Path( [ $( $ident:expr ),+ ], Slice( $range:expr ) ) ) => {
            ParseExpr::Path(
                vec![ $( $ident.to_string() ),+ ],
                Some(PathArgs::Slice(Range::from($range)))
            )
        };

        ( PosPred( $child:expr ) ) => {
            ParseExpr::PosPred(Box::new($child))
        };

        ( NegPred( $child:expr ) ) => {
            ParseExpr::NegPred(Box::new($child))
        };

        ( Seq( $lhs:expr, $rhs:expr ) ) => {
            ParseExpr::Seq(
                Box::new($lhs),
                Box::new($rhs),
                Trivia::None
            )
        };

        ( Seq( $lhs:expr, $rhs:expr, $op:expr ) ) => {
            ParseExpr::Seq(
                Box::new($lhs),
                Box::new($rhs),
                $op
            )
        };

        ( Choice( $lhs:expr, $rhs:expr ) ) => {
            ParseExpr::Choice(
                Box::new($lhs),
                Box::new($rhs)
            )
        };

        ( Opt( $child:expr ) ) => {
            ParseExpr::Opt(Box::new($child))
        };

        ( Rep( $child:expr ) ) => {
            ParseExpr::Rep(Box::new($child))
        };

        ( RepOnce( $child:expr ) ) => {
            ParseExpr::RepOnce(Box::new($child))
        };

        ( RepRange( $child:expr, $range:expr ) ) => {
            ParseExpr::RepRange(Box::new($child), Range::from($range))
        };

        ( Separated( $child:expr, $op:expr ) ) => {
            ParseExpr::Separated(
                Box::new($child),
                $op
            )
        };
    }

    #[test]
    fn parse_comment() {
        let pair = PestParser::parse(Rule::COMMENT, r#"// A test for grammar with doc comments."#)
            .unwrap();
        assert_eq!(pair.into_iter().len(), 0);

        PestParser::parse(
            Rule::COMMENT,
            r#"//! A test for grammar with doc comments."#,
        )
        .unwrap_err();

        PestParser::parse(
            Rule::COMMENT,
            r#"/// A test for grammar with doc comments."#,
        )
        .unwrap_err();
    }

    #[test]
    fn parse_grammar_doc_comment() {
        let input = r#"//! A test for grammar with doc comments."#;

        let pair = PestParser::parse(Rule::grammar_doc, input)
            .unwrap_or_else(|e| panic!("{e}"))
            .next()
            .unwrap();

        assert_eq!(
            parse_grammar_doc(pair).unwrap(),
            "A test for grammar with doc comments.",
        );

        let input =
            "//! A test for grammar with doc comments.\n//! Second line should not be included.";

        let pair = PestParser::parse(Rule::grammar_doc, input)
            .unwrap_or_else(|e| panic!("{e}"))
            .next()
            .unwrap();

        assert_eq!(
            parse_grammar_doc(pair).unwrap(),
            "A test for grammar with doc comments.",
        );

        let input = r#"
//! A test for grammar with doc comments.
x = " "+
"#;
        PestParser::parse(Rule::grammar_rules, input).unwrap_or_else(|e| panic!("{e}"));
    }

    #[test]
    fn parse_rule_doc_comment() {
        let input = r#"/// A test for rule with doc comments."#;

        let pair = PestParser::parse(Rule::rule_doc, input)
            .unwrap_or_else(|e| panic!("{e}"))
            .next()
            .unwrap();

        assert_eq!(
            parse_rule_doc(pair).unwrap(),
            "A test for rule with doc comments.",
        );

        let input = "\
/// A test for grammar with doc comments.
x = pest::any+";
        PestParser::parse(Rule::grammar_rule, input).unwrap_or_else(|e| panic!("{e}"));
    }

    #[test]
    fn parse_string_escaped() {
        let input = r#""a\nb""#;

        let pair = PestParser::parse(Rule::string, input)
            .unwrap()
            .next()
            .unwrap();

        assert_eq!(
            parse_string(pair, &span(input, 0)).unwrap(),
            ParseNode {
                expr: expr!(Str("a\nb")),
                span: span(input, 0)
            }
        );
    }

    #[test]
    fn parse_string_raw() {
        let input = r##"r"a""##;

        let pair = PestParser::parse(Rule::raw_string, input)
            .unwrap()
            .next()
            .unwrap();

        assert_eq!(
            parse_raw_string(pair, &span(input, 0)),
            ParseNode {
                expr: expr!(Str("a")),
                span: span(input, 0)
            }
        );
    }

    #[test]
    fn parse_insensitive_string_escaped() {
        let input = r#"i"a\nb""#;

        let pair = PestParser::parse(Rule::insensitive_string, input)
            .unwrap()
            .next()
            .unwrap();

        assert_eq!(
            parse_insensitive_string(pair, &span(input, 0)).unwrap(),
            ParseNode {
                expr: expr!(Insens("a\nb")),
                span: span(input, 0)
            }
        );
    }

    #[test]
    fn parse_char_range_escaped() {
        let input = "'\x12'..'\u{555}'";

        let pair = PestParser::parse(Rule::range, input)
            .unwrap()
            .next()
            .unwrap();

        assert_eq!(
            parse_char_range(pair, &span(input, 0)).unwrap(),
            ParseNode {
                expr: expr!(Range('\x12', '\u{555}')),
                span: span(input, 0)
            }
        );
    }

    #[test]
    fn parse_path_simple() {
        let input = "a::b";

        let pair = PestParser::parse(Rule::path, input)
            .unwrap()
            .next()
            .unwrap();

        assert_eq!(
            parse_path(pair, &span(input, 0), &climber()).unwrap(),
            ParseNode {
                expr: expr!(Path(["a", "b"])),
                span: span(input, 0)
            }
        );
    }

    #[test]
    fn parse_path_with_call() {
        let input = "a::b::c(d, e)";

        let pair = PestParser::parse(Rule::path, input)
            .unwrap()
            .next()
            .unwrap();

        assert_eq!(
            parse_path(pair, &span(input, 0), &climber()).unwrap(),
            ParseNode {
                expr: expr!(Path(
                    ["a", "b", "c"],
                    Call(
                        ParseNode {
                            expr: expr!(Path(["d"])),
                            span: span("d", 8)
                        },
                        ParseNode {
                            expr: expr!(Path(["e"])),
                            span: span("e", 11)
                        }
                    )
                )),
                span: span(input, 0)
            }
        );
    }

    #[test]
    fn parse_path_with_slice() {
        let input = "a[0..1]";

        let pair = PestParser::parse(Rule::path, input)
            .unwrap()
            .next()
            .unwrap();

        assert_eq!(
            parse_path(pair, &span(input, 0), &climber()).unwrap(),
            ParseNode {
                expr: expr!(Path(["a"], Slice(0..1))),
                span: span(input, 0)
            }
        );
    }

    #[test]
    fn parse_range_full() {
        let input = "{1..2}";

        let pair = PestParser::parse(Rule::bounded_repeat, input)
            .unwrap()
            .next()
            .unwrap();

        assert_eq!(
            parse_bounded_repeat(pair, child()).unwrap(),
            expr!(RepRange(child(), 1..2))
        );
    }

    #[test]
    fn parse_range_lower_bound_only() {
        let input = "{1..}";

        let pair = PestParser::parse(Rule::bounded_repeat, input)
            .unwrap()
            .next()
            .unwrap();

        assert_eq!(
            parse_bounded_repeat(pair, child()).unwrap(),
            expr!(RepRange(child(), 1..))
        );
    }

    #[test]
    fn parse_range_upper_bound_only() {
        let input = "{..2}";

        let pair = PestParser::parse(Rule::bounded_repeat, input)
            .unwrap()
            .next()
            .unwrap();

        assert_eq!(
            parse_bounded_repeat(pair, child()).unwrap(),
            expr!(RepRange(child(), ..2))
        );
    }

    #[test]
    fn parse_range_unbounded() {
        let input = "{..}";

        let pair = PestParser::parse(Rule::bounded_repeat, input)
            .unwrap()
            .next()
            .unwrap();

        assert_eq!(
            parse_bounded_repeat(pair, child()).unwrap(),
            expr!(RepRange(child(), ..))
        );
    }

    #[test]
    fn parse_range_exact() {
        let input = "{1}";

        let pair = PestParser::parse(Rule::bounded_repeat, input)
            .unwrap()
            .next()
            .unwrap();

        assert_eq!(
            parse_bounded_repeat(pair, child()).unwrap(),
            expr!(RepRange(child(), 1..1))
        );
    }

    #[test]
    fn parse_prefixes() {
        let input = "!&a";

        let pair = PestParser::parse(Rule::expression, input)
            .unwrap()
            .next()
            .unwrap();

        assert_eq!(
            parse_node(pair, &span(input, 0), &climber()).unwrap(),
            ParseNode {
                expr: expr!(NegPred(ParseNode {
                    expr: expr!(PosPred(ParseNode {
                        expr: expr!(Path(["a"])),
                        span: span("a", 2)
                    })),
                    span: span("&a", 1)
                })),
                span: span(input, 0)
            }
        );
    }

    #[test]
    fn parse_infixes() {
        let input = "a ~ b ^ c | d - e";

        let pair = PestParser::parse(Rule::expression, input)
            .unwrap()
            .next()
            .unwrap();

        let lhs = expr!(Seq(
            ParseNode {
                expr: expr!(Path(["a"])),
                span: span("a ", 0)
            },
            ParseNode {
                expr: expr!(Seq(
                    ParseNode {
                        expr: expr!(Path(["b"])),
                        span: span("b ", 4)
                    },
                    ParseNode {
                        expr: expr!(Path(["c"])),
                        span: span("c ", 8)
                    },
                    Trivia::Mandatory //'^'
                )),
                span: span("b ^ c ", 4)
            },
            Trivia::Optional
        ));

        let rhs = expr!(Seq(
            ParseNode {
                expr: expr!(Path(["d"])),
                span: span("d ", 12)
            },
            ParseNode {
                expr: expr!(Path(["e"])),
                span: span("e", 16)
            }
        ));

        assert_eq!(
            parse_node(pair, &span(input, 0), &climber()).unwrap(),
            ParseNode {
                expr: expr!(Choice(
                    ParseNode {
                        expr: lhs,
                        span: span("a ~ b ^ c ", 0)
                    },
                    ParseNode {
                        expr: rhs,
                        span: span("d - e", 12)
                    }
                )),
                span: span(input, 0)
            }
        );
    }

    #[test]
    fn parse_postfixes() {
        let input = "a~{1}+*?";

        let pair = PestParser::parse(Rule::expression, input)
            .unwrap()
            .next()
            .unwrap();

        let exact = expr!(RepRange(
            ParseNode {
                expr: expr!(Separated(
                    ParseNode {
                        expr: expr!(Path(["a"])),
                        span: span("a", 0)
                    },
                    Trivia::Optional
                )),
                span: span("a~", 0)
            },
            1..1
        ));

        let repeat = expr!(Rep(ParseNode {
            expr: expr!(RepOnce(ParseNode {
                expr: exact,
                span: span("a~{1}", 0)
            })),
            span: span("a~{1}+", 0)
        }));

        assert_eq!(
            parse_node(pair, &span(input, 0), &climber()).unwrap(),
            ParseNode {
                expr: expr!(Opt(ParseNode {
                    expr: repeat,
                    span: span("a~{1}+*", 0)
                })),
                span: span(input, 0)
            }
        );
    }

    #[test]
    #[should_panic(expected = " --> 1:3
  |
1 | a{18446744073709551616}
  |   ^------------------^
  |
  = number cannot overflow 8 bytes")]
    fn repeat_overflow() {
        let input = "a{18446744073709551616}";

        let pair = PestParser::parse(Rule::expression, input)
            .unwrap()
            .next()
            .unwrap();
        parse_node(pair, &span(input, 0), &climber()).unwrap_or_else(|e| panic!("{}", e));
    }

    #[test]
    #[should_panic(expected = " --> 1:3
  |
1 | a{18446744073709551616..}
  |   ^------------------^
  |
  = number cannot overflow 8 bytes")]
    fn repeat_overflow_start() {
        let input = "a{18446744073709551616..}";

        let pair = PestParser::parse(Rule::expression, input)
            .unwrap()
            .next()
            .unwrap();
        parse_node(pair, &span(input, 0), &climber()).unwrap_or_else(|e| panic!("{}", e));
    }

    #[test]
    #[should_panic(expected = " --> 1:5
  |
1 | a{..18446744073709551616}
  |     ^------------------^
  |
  = number cannot overflow 8 bytes")]
    fn repeat_overflow_end() {
        let input = "a{..18446744073709551616}";

        let pair = PestParser::parse(Rule::expression, input)
            .unwrap()
            .next()
            .unwrap();
        parse_node(pair, &span(input, 0), &climber()).unwrap_or_else(|e| panic!("{}", e));
    }

    #[test]
    #[should_panic(expected = " --> 1:3
  |
1 | a{0}
  |   ^
  |
  = cannot repeat 0 times")]
    fn repeat_zero() {
        let input = "a{0}";

        let pair = PestParser::parse(Rule::expression, input)
            .unwrap()
            .next()
            .unwrap();
        parse_node(pair, &span(input, 0), &climber()).unwrap_or_else(|e| panic!("{}", e));
    }

    #[test]
    #[should_panic(expected = " --> 1:3
  |
1 | a{1..0}
  |   ^--^
  |
  = repetition range void (1 > 0)")]
    fn repeat_void() {
        let input = "a{1..0}";

        let pair = PestParser::parse(Rule::expression, input)
            .unwrap()
            .next()
            .unwrap();
        parse_node(pair, &span(input, 0), &climber()).unwrap_or_else(|e| panic!("{}", e));
    }

    #[test]
    #[should_panic(expected = r#" --> 1:1
  |
1 | "\u{ffffff}"
  | ^----------^
  |
  = incorrect string literal"#)]
    fn incorrect_string() {
        let input = r#""\u{ffffff}""#;

        let pair = PestParser::parse(Rule::expression, input)
            .unwrap()
            .next()
            .unwrap();
        parse_node(pair, &span(input, 0), &climber()).unwrap_or_else(|e| panic!("{}", e));
    }

    #[test]
    #[should_panic(expected = r#" --> 1:1
  |
1 | i"\u{ffffff}"
  | ^-----------^
  |
  = incorrect string literal"#)]
    fn incorrect_insensitive_string() {
        let input = r#"i"\u{ffffff}""#;

        let pair = PestParser::parse(Rule::expression, input)
            .unwrap()
            .next()
            .unwrap();
        parse_node(pair, &span(input, 0), &climber()).unwrap_or_else(|e| panic!("{}", e));
    }

    #[test]
    #[should_panic(expected = r" --> 1:1
  |
1 | '\xff'..'a'
  | ^----^
  |
  = incorrect char literal")]
    fn incorrect_char_start() {
        let input = r"'\xff'..'a'";

        let pair = PestParser::parse(Rule::expression, input)
            .unwrap()
            .next()
            .unwrap();
        parse_node(pair, &span(input, 0), &climber()).unwrap_or_else(|e| panic!("{}", e));
    }

    #[test]
    #[should_panic(expected = r" --> 1:6
  |
1 | 'a'..'\xff'
  |      ^----^
  |
  = incorrect char literal")]
    fn incorrect_char_end() {
        let input = r"'a'..'\xff'";

        let pair = PestParser::parse(Rule::expression, input)
            .unwrap()
            .next()
            .unwrap();
        parse_node(pair, &span(input, 0), &climber()).unwrap_or_else(|e| panic!("{}", e));
    }

    #[test]
    fn unescape_defaults() {
        let string = r#"\"\\\r\n\t\0\'"#;

        assert_eq!(unescape(string), Some("\"\\\r\n\t\0\'".to_string()));
    }

    #[test]
    fn unescape_wrong_default() {
        let string = r#"\q"#;

        assert_eq!(unescape(string), None);
    }

    #[test]
    fn unescape_bytes() {
        let string = r#"\x00\x7f"#;

        assert_eq!(unescape(string), Some("\x00\x7f".to_string()));
    }

    #[test]
    fn unescape_wrong_byte() {
        let string = r#"\x80"#;

        assert_eq!(unescape(string), None);
    }

    #[test]
    fn unescape_unicode() {
        let string = r#"\u{0}\u{10ffff}"#;

        assert_eq!(unescape(string), Some("\u{0}\u{10ffff}".to_string()));
    }

    #[test]
    fn unescape_wrong_unicode() {
        let string = r#"\u{110000}"#;

        assert_eq!(unescape(string), None);
    }

    #[test]
    fn unescape_unicode_missing_opening_brace() {
        let string = r#"\u0}"#;

        assert_eq!(unescape(string), None);
    }

    #[test]
    fn unescape_unicode_missing_closing_brace() {
        let string = r#"\u{0"#;

        assert_eq!(unescape(string), None);
    }

    #[test]
    fn unescape_unicode_empty() {
        let string = r#"\u{}"#;

        assert_eq!(unescape(string), None);
    }
}
