//! Runtime parser for pest3 grammars.

#![warn(rust_2018_idioms, rust_2021_compatibility, missing_docs)]

use pest3_core::{token::Pair, unicode};
use pest3_meta::parser::{self, GrammarModule, Import, ParseExpr, ParseNode, ParseRule, PathArgs};
use std::{
    borrow::Cow,
    collections::BTreeSet,
    fmt::{Display, Formatter},
    path::Path,
    sync::Arc,
};

#[derive(Clone, Debug, PartialEq, Eq)]
struct MatchResult {
    end: usize,
    pairs: Vec<Pair<String>>,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct Failure {
    farthest: usize,
    expected: BTreeSet<String>,
}

impl Failure {
    fn record(&mut self, position: usize, expected: impl Into<String>) {
        match position.cmp(&self.farthest) {
            std::cmp::Ordering::Greater => {
                self.farthest = position;
                self.expected.clear();
                self.expected.insert(expected.into());
            }
            std::cmp::Ordering::Equal => {
                self.expected.insert(expected.into());
            }
            std::cmp::Ordering::Less => {}
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Builtin {
    Soi,
    Eoi,
    Any,
    Newline,
    AsciiDigit,
    AsciiNonzeroDigit,
    AsciiBinDigit,
    AsciiOctDigit,
    AsciiHexDigit,
    AsciiAlphaLower,
    AsciiAlphaUpper,
    AsciiAlpha,
    AsciiAlphanumeric,
    Ascii,
    Unicode(String),
    StackPush(ParseNode),
    StackPeek(Option<(isize, Option<isize>)>),
    StackPeekAll,
    StackDrop,
    StackPop,
    StackPopAll,
}

#[derive(Clone, Debug)]
enum Target<'g> {
    Rule {
        module: &'g GrammarModule,
        rule: &'g ParseRule,
    },
    Builtin(Builtin),
}

/// Successful runtime parse output.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParseOutput {
    /// Start byte offset.
    pub start: usize,
    /// End byte offset.
    pub end: usize,
    /// Top-level produced pairs.
    pub pairs: Vec<Pair<String>>,
}

/// Parser failure information.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParseError {
    /// Requested top-level rule.
    pub rule: String,
    /// Farthest failure byte offset.
    pub position: usize,
    /// Expected items recorded at the farthest failure.
    pub expected: BTreeSet<String>,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "failed to parse rule `{}` at byte {}",
            self.rule, self.position
        )?;
        if !self.expected.is_empty() {
            write!(f, " (expected ")?;
            let mut iter = self.expected.iter();
            if let Some(first) = iter.next() {
                write!(f, "{first}")?;
            }
            for item in iter {
                write!(f, ", {item}")?;
            }
            write!(f, ")")?;
        }
        Ok(())
    }
}

impl std::error::Error for ParseError {}

/// Runtime VM errors.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error {
    /// The grammar source could not be parsed.
    GrammarParse(String),
    /// A referenced rule or module path could not be resolved.
    UnknownPath(Vec<String>),
    /// A requested entry rule does not exist.
    UnknownRule(String),
    /// A rule or builtin was called with the wrong number of arguments.
    ArgumentCount {
        /// Target path.
        path: Vec<String>,
        /// Expected argument count.
        expected: usize,
        /// Actual argument count.
        actual: usize,
    },
    /// A slice argument was used where it is not supported.
    UnsupportedSlice(Vec<String>),
    /// The runtime parser hit a parsing failure.
    Parse(ParseError),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::GrammarParse(message) => write!(f, "failed to parse grammar: {message}"),
            Self::UnknownPath(path) => write!(f, "unknown path `{}`", path.join("::")),
            Self::UnknownRule(rule) => write!(f, "unknown rule `{rule}`"),
            Self::ArgumentCount {
                path,
                expected,
                actual,
            } => write!(
                f,
                "path `{}` expects {expected} arguments but got {actual}",
                path.join("::")
            ),
            Self::UnsupportedSlice(path) => {
                write!(
                    f,
                    "path `{}` does not support slice arguments",
                    path.join("::")
                )
            }
            Self::Parse(error) => Display::fmt(error, f),
        }
    }
}

impl std::error::Error for Error {}

/// Tracing event emitted during runtime parsing.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TraceEvent<'a> {
    /// Entering a named rule.
    EnterRule {
        /// Rule name.
        name: &'a str,
        /// Input byte offset at entry.
        position: usize,
    },
    /// Exiting a named rule.
    ExitRule {
        /// Rule name.
        name: &'a str,
        /// Input byte offset at entry.
        start: usize,
        /// Successful end position, if any.
        end: Option<usize>,
    },
    /// Entering a grammar expression node.
    EnterExpr {
        /// Expression being evaluated.
        expr: &'a ParseExpr,
        /// Input byte offset at entry.
        position: usize,
    },
    /// Exiting a grammar expression node.
    ExitExpr {
        /// Expression being evaluated.
        expr: &'a ParseExpr,
        /// Input byte offset at entry.
        start: usize,
        /// Successful end position, if any.
        end: Option<usize>,
    },
    /// Pushed a span to the stack.
    StackPush {
        /// Span start byte offset.
        start: usize,
        /// Span end byte offset.
        end: usize,
        /// Stack depth after the push.
        depth: usize,
    },
    /// Popped a span from the stack.
    StackPop {
        /// Span start byte offset.
        start: usize,
        /// Span end byte offset.
        end: usize,
        /// Stack depth after the pop.
        depth: usize,
    },
}

/// Trace sink for debugger-style integrations.
pub trait Tracer {
    /// Receive a single tracing event.
    fn event(&mut self, event: TraceEvent<'_>);
}

impl<F> Tracer for F
where
    F: for<'a> FnMut(TraceEvent<'a>),
{
    fn event(&mut self, event: TraceEvent<'_>) {
        self(event);
    }
}

struct NoopTracer;

impl Tracer for NoopTracer {
    fn event(&mut self, _event: TraceEvent<'_>) {}
}

/// Runtime parser for a pest3 grammar.
#[derive(Clone, Debug)]
pub struct Vm {
    module: Arc<GrammarModule>,
}

impl Vm {
    /// Create a VM from a parsed grammar module.
    pub fn new(module: Arc<GrammarModule>) -> Self {
        Self { module }
    }

    /// Parse a pest3 grammar source and build a VM.
    pub fn from_source<P: AsRef<Path>>(grammar: &str, path: P) -> Result<Self, Error> {
        let module = parser::parse(grammar, &path)
            .map_err(|error| Error::GrammarParse(error.to_string()))?;
        Ok(Self::new(module))
    }

    /// Parse input with a named entry rule.
    pub fn parse(&self, rule: &str, input: &str) -> Result<ParseOutput, Error> {
        let mut tracer = NoopTracer;
        self.parse_with_tracer(rule, input, &mut tracer)
    }

    /// Parse input with a named entry rule and emit tracing events.
    pub fn parse_with_tracer(
        &self,
        rule: &str,
        input: &str,
        tracer: &mut impl Tracer,
    ) -> Result<ParseOutput, Error> {
        let entry = self
            .find_rule(self.module.as_ref(), rule)
            .ok_or_else(|| Error::UnknownRule(rule.to_owned()))?;
        let mut runtime = Runtime {
            vm: self,
            input,
            failure: Failure::default(),
            tracer,
        };
        let mut stack = Vec::new();
        let matched = runtime.eval_rule(self.module.as_ref(), entry, &entry.node, 0, &mut stack)?;
        if matched.end != input.len() {
            runtime.failure.record(matched.end, "pest::eoi");
            return Err(Error::Parse(ParseError {
                rule: rule.to_owned(),
                position: runtime.failure.farthest,
                expected: runtime.failure.expected,
            }));
        }
        Ok(ParseOutput {
            start: 0,
            end: matched.end,
            pairs: matched.pairs,
        })
    }

    /// Check whether a named rule accepts the full input.
    pub fn matches(&self, rule: &str, input: &str) -> Result<(), Error> {
        self.parse(rule, input).map(|_| ())
    }

    fn find_rule<'g>(&self, module: &'g GrammarModule, name: &str) -> Option<&'g ParseRule> {
        module.rules.iter().find(|rule| rule.name == name)
    }

    fn resolve_target<'g>(
        &self,
        module: &'g GrammarModule,
        path: &[String],
        args: &Option<PathArgs>,
    ) -> Result<Target<'g>, Error> {
        if path.is_empty() {
            return Err(Error::UnknownPath(path.to_vec()));
        }

        if path[0] == "pest" {
            return self.resolve_builtin(path, args).map(Target::Builtin);
        }

        if path.len() == 1 {
            if let Some(rule) = self.find_rule(module, &path[0]) {
                return Ok(Target::Rule { module, rule });
            }
        }

        for import in &module.imports {
            match import {
                Import::Builtin(alias, prefix) if alias == &path[0] => {
                    let mut full = prefix.clone();
                    full.extend_from_slice(&path[1..]);
                    return self.resolve_builtin(&full, args).map(Target::Builtin);
                }
                Import::File(alias, imported) if alias == &path[0] => {
                    if path.len() == 1 {
                        return Err(Error::UnknownPath(path.to_vec()));
                    }
                    return self.resolve_target(imported.as_ref(), &path[1..], args);
                }
                _ => {}
            }
        }

        Err(Error::UnknownPath(path.to_vec()))
    }

    fn resolve_builtin(&self, path: &[String], args: &Option<PathArgs>) -> Result<Builtin, Error> {
        let builtin = match path {
            [pest, rule] if pest == "pest" => match rule.as_str() {
                "soi" => {
                    expect_no_args(path, args)?;
                    Builtin::Soi
                }
                "eoi" => {
                    expect_no_args(path, args)?;
                    Builtin::Eoi
                }
                "any" => {
                    expect_no_args(path, args)?;
                    Builtin::Any
                }
                "newline" => {
                    expect_no_args(path, args)?;
                    Builtin::Newline
                }
                "ascii_digit" => {
                    expect_no_args(path, args)?;
                    Builtin::AsciiDigit
                }
                "ascii_nonzero_digit" => {
                    expect_no_args(path, args)?;
                    Builtin::AsciiNonzeroDigit
                }
                "ascii_bin_digit" => {
                    expect_no_args(path, args)?;
                    Builtin::AsciiBinDigit
                }
                "ascii_oct_digit" => {
                    expect_no_args(path, args)?;
                    Builtin::AsciiOctDigit
                }
                "ascii_hex_digit" => {
                    expect_no_args(path, args)?;
                    Builtin::AsciiHexDigit
                }
                "ascii_alpha_lower" => {
                    expect_no_args(path, args)?;
                    Builtin::AsciiAlphaLower
                }
                "ascii_alpha_upper" => {
                    expect_no_args(path, args)?;
                    Builtin::AsciiAlphaUpper
                }
                "ascii_alpha" => {
                    expect_no_args(path, args)?;
                    Builtin::AsciiAlpha
                }
                "ascii_alphanumeric" => {
                    expect_no_args(path, args)?;
                    Builtin::AsciiAlphanumeric
                }
                "ascii" => {
                    expect_no_args(path, args)?;
                    Builtin::Ascii
                }
                _ => return Err(Error::UnknownPath(path.to_vec())),
            },
            [pest, unicode_mod, property] if pest == "pest" && unicode_mod == "unicode" => {
                expect_no_args(path, args)?;
                Builtin::Unicode(property.to_ascii_uppercase())
            }
            [pest, stack_mod, rule] if pest == "pest" && stack_mod == "stack" => {
                match rule.as_str() {
                    "push" => {
                        let call = expect_call(path, args, Some(1))?;
                        Builtin::StackPush(call[0].clone())
                    }
                    "peek" => match args {
                        None => Builtin::StackPeek(None),
                        Some(PathArgs::Slice(range)) => {
                            Builtin::StackPeek(Some((range.start.unwrap_or(0), range.end)))
                        }
                        Some(PathArgs::Call(_)) => {
                            return Err(Error::UnsupportedSlice(path.to_vec()))
                        }
                    },
                    "peek_all" => {
                        expect_no_args(path, args)?;
                        Builtin::StackPeekAll
                    }
                    "drop" => {
                        expect_no_args(path, args)?;
                        Builtin::StackDrop
                    }
                    "pop" => {
                        expect_no_args(path, args)?;
                        Builtin::StackPop
                    }
                    "pop_all" => {
                        expect_no_args(path, args)?;
                        Builtin::StackPopAll
                    }
                    _ => return Err(Error::UnknownPath(path.to_vec())),
                }
            }
            _ => return Err(Error::UnknownPath(path.to_vec())),
        };
        Ok(builtin)
    }

    fn trivia_rule<'g>(&self, module: &'g GrammarModule, mandatory: bool) -> Option<&'g ParseRule> {
        let name = if mandatory { "^" } else { "~" };
        self.find_rule(module, name)
    }
}

struct Runtime<'a, 'i, T: Tracer> {
    vm: &'a Vm,
    input: &'i str,
    failure: Failure,
    tracer: &'a mut T,
}

impl<'a, 'i, T: Tracer> Runtime<'a, 'i, T> {
    fn eval_rule(
        &mut self,
        module: &GrammarModule,
        rule: &ParseRule,
        node: &ParseNode,
        position: usize,
        stack: &mut Vec<(usize, usize)>,
    ) -> Result<MatchResult, Error> {
        self.tracer.event(TraceEvent::EnterRule {
            name: &rule.name,
            position,
        });
        let start = position;
        let snapshot = stack.clone();
        let result = match self.eval_node(module, node, position, stack)? {
            Some(result) if rule.silent => Some(result),
            Some(result) => Some(MatchResult {
                end: result.end,
                pairs: vec![Pair {
                    rule: rule.name.clone(),
                    start,
                    end: result.end,
                    children: result.pairs,
                }],
            }),
            None => None,
        };
        if result.is_none() {
            *stack = snapshot;
        }
        self.tracer.event(TraceEvent::ExitRule {
            name: &rule.name,
            start,
            end: result.as_ref().map(|result| result.end),
        });
        result.ok_or_else(|| {
            Error::Parse(ParseError {
                rule: rule.name.clone(),
                position: self.failure.farthest,
                expected: self.failure.expected.clone(),
            })
        })
    }

    fn eval_rule_target(
        &mut self,
        module: &GrammarModule,
        rule: &ParseRule,
        args: &Option<PathArgs>,
        position: usize,
        stack: &mut Vec<(usize, usize)>,
    ) -> Result<Option<MatchResult>, Error> {
        let node = if rule.args.is_empty() {
            if let Some(PathArgs::Call(args)) = args {
                if !args.is_empty() {
                    return Err(Error::ArgumentCount {
                        path: vec![rule.name.clone()],
                        expected: 0,
                        actual: args.len(),
                    });
                }
            } else if matches!(args, Some(PathArgs::Slice(_))) {
                return Err(Error::UnsupportedSlice(vec![rule.name.clone()]));
            }
            Cow::Borrowed(&rule.node)
        } else {
            let path = vec![rule.name.clone()];
            let call = expect_call(&path, args, Some(rule.args.len()))?;
            let mut node = rule.node.clone();
            for (name, arg) in rule.args.iter().zip(call.iter()) {
                node.expr.replace(name, &arg.expr);
            }
            Cow::Owned(node)
        };

        let snapshot = stack.clone();
        self.tracer.event(TraceEvent::EnterRule {
            name: &rule.name,
            position,
        });
        let start = position;
        let result = match self.eval_node(module, &node, position, stack)? {
            Some(result) if rule.silent => Some(result),
            Some(result) => Some(MatchResult {
                end: result.end,
                pairs: vec![Pair {
                    rule: rule.name.clone(),
                    start,
                    end: result.end,
                    children: result.pairs,
                }],
            }),
            None => None,
        };
        if result.is_none() {
            *stack = snapshot;
        }
        self.tracer.event(TraceEvent::ExitRule {
            name: &rule.name,
            start,
            end: result.as_ref().map(|result| result.end),
        });
        Ok(result)
    }

    fn eval_node(
        &mut self,
        module: &GrammarModule,
        node: &ParseNode,
        position: usize,
        stack: &mut Vec<(usize, usize)>,
    ) -> Result<Option<MatchResult>, Error> {
        self.tracer.event(TraceEvent::EnterExpr {
            expr: &node.expr,
            position,
        });
        let snapshot = stack.clone();
        let start = position;
        let result = self.eval_expr(module, &node.expr, position, stack)?;
        if result.is_none() {
            *stack = snapshot;
        }
        self.tracer.event(TraceEvent::ExitExpr {
            expr: &node.expr,
            start,
            end: result.as_ref().map(|result| result.end),
        });
        Ok(result)
    }

    fn eval_expr(
        &mut self,
        module: &GrammarModule,
        expr: &ParseExpr,
        position: usize,
        stack: &mut Vec<(usize, usize)>,
    ) -> Result<Option<MatchResult>, Error> {
        match expr {
            ParseExpr::Str(content) => Ok(match_literal(self.input, position, content)
                .map(|end| MatchResult { end, pairs: vec![] })
                .or_else(|| {
                    self.failure.record(position, format!("{content:?}"));
                    None
                })),
            ParseExpr::Insens(content) => Ok(match_insensitive(self.input, position, content)
                .map(|end| MatchResult { end, pairs: vec![] })
                .or_else(|| {
                    self.failure.record(position, format!("i{content:?}"));
                    None
                })),
            ParseExpr::Range(start, end) => Ok(match_range(self.input, position, *start, *end)
                .map(|end| MatchResult { end, pairs: vec![] })
                .or_else(|| {
                    self.failure.record(position, format!("{start:?}..{end:?}"));
                    None
                })),
            ParseExpr::Path(path, args) => self.eval_path(module, path, args, position, stack),
            ParseExpr::PosPred(node) => {
                let mut snapshot = stack.clone();
                Ok(
                    match self.eval_node(module, node, position, &mut snapshot)? {
                        Some(_) => Some(MatchResult {
                            end: position,
                            pairs: vec![],
                        }),
                        None => {
                            *stack = snapshot;
                            None
                        }
                    },
                )
            }
            ParseExpr::NegPred(node) => {
                let mut snapshot = stack.clone();
                Ok(
                    match self.eval_node(module, node, position, &mut snapshot)? {
                        Some(_) => None,
                        None => {
                            *stack = snapshot;
                            Some(MatchResult {
                                end: position,
                                pairs: vec![],
                            })
                        }
                    },
                )
            }
            ParseExpr::Seq(left, right, trivia) => {
                let left = match self.eval_node(module, left, position, stack)? {
                    Some(left) => left,
                    None => return Ok(None),
                };
                let after_trivia = match self.apply_trivia(module, trivia, left.end, stack)? {
                    Some(pos) => pos,
                    None => return Ok(None),
                };
                let right = match self.eval_node(module, right, after_trivia, stack)? {
                    Some(right) => right,
                    None => return Ok(None),
                };
                let mut pairs = left.pairs;
                pairs.extend(right.pairs);
                Ok(Some(MatchResult {
                    end: right.end,
                    pairs,
                }))
            }
            ParseExpr::Choice(left, right) => {
                if let Some(left) = self.eval_node(module, left, position, stack)? {
                    return Ok(Some(left));
                }
                self.eval_node(module, right, position, stack)
            }
            ParseExpr::Opt(node) => Ok(Some(
                self.eval_node(module, node, position, stack)?
                    .unwrap_or(MatchResult {
                        end: position,
                        pairs: vec![],
                    }),
            )),
            ParseExpr::Rep(node) => self.eval_repetition(module, node, position, stack, 0, None),
            ParseExpr::RepOnce(node) => {
                self.eval_repetition(module, node, position, stack, 1, None)
            }
            ParseExpr::RepRange(node, range) => {
                let min = range.start.unwrap_or(0);
                let max = range.end;
                self.eval_repetition(module, node, position, stack, min, max)
            }
            ParseExpr::Separated(node, _) => self.eval_node(module, node, position, stack),
        }
    }

    fn eval_path(
        &mut self,
        module: &GrammarModule,
        path: &[String],
        args: &Option<PathArgs>,
        position: usize,
        stack: &mut Vec<(usize, usize)>,
    ) -> Result<Option<MatchResult>, Error> {
        match self.vm.resolve_target(module, path, args)? {
            Target::Rule { module, rule } => {
                self.eval_rule_target(module, rule, args, position, stack)
            }
            Target::Builtin(builtin) => self.eval_builtin(module, &builtin, position, stack),
        }
    }

    fn eval_builtin(
        &mut self,
        module: &GrammarModule,
        builtin: &Builtin,
        position: usize,
        stack: &mut Vec<(usize, usize)>,
    ) -> Result<Option<MatchResult>, Error> {
        let result = match builtin {
            Builtin::Soi => {
                if position == 0 {
                    Some(MatchResult {
                        end: position,
                        pairs: vec![],
                    })
                } else {
                    self.failure.record(position, "pest::soi");
                    None
                }
            }
            Builtin::Eoi => {
                if position == self.input.len() {
                    Some(MatchResult {
                        end: position,
                        pairs: vec![],
                    })
                } else {
                    self.failure.record(position, "pest::eoi");
                    None
                }
            }
            Builtin::Any => match next_char_end(self.input, position) {
                Some(end) => Some(MatchResult { end, pairs: vec![] }),
                None => {
                    self.failure.record(position, "pest::any");
                    None
                }
            },
            Builtin::Newline => match match_newline(self.input, position) {
                Some(end) => Some(MatchResult { end, pairs: vec![] }),
                None => {
                    self.failure.record(position, "pest::newline");
                    None
                }
            },
            Builtin::AsciiDigit => {
                match match_predicate(self.input, position, |ch| ch.is_ascii_digit()) {
                    Some(end) => Some(MatchResult { end, pairs: vec![] }),
                    None => {
                        self.failure.record(position, "pest::ascii_digit");
                        None
                    }
                }
            }
            Builtin::AsciiNonzeroDigit => {
                match match_predicate(self.input, position, |ch| ('1'..='9').contains(&ch)) {
                    Some(end) => Some(MatchResult { end, pairs: vec![] }),
                    None => {
                        self.failure.record(position, "pest::ascii_nonzero_digit");
                        None
                    }
                }
            }
            Builtin::AsciiBinDigit => {
                match match_predicate(self.input, position, |ch| matches!(ch, '0' | '1')) {
                    Some(end) => Some(MatchResult { end, pairs: vec![] }),
                    None => {
                        self.failure.record(position, "pest::ascii_bin_digit");
                        None
                    }
                }
            }
            Builtin::AsciiOctDigit => {
                match match_predicate(self.input, position, |ch| ('0'..='7').contains(&ch)) {
                    Some(end) => Some(MatchResult { end, pairs: vec![] }),
                    None => {
                        self.failure.record(position, "pest::ascii_oct_digit");
                        None
                    }
                }
            }
            Builtin::AsciiHexDigit => {
                match match_predicate(self.input, position, |ch| ch.is_ascii_hexdigit()) {
                    Some(end) => Some(MatchResult { end, pairs: vec![] }),
                    None => {
                        self.failure.record(position, "pest::ascii_hex_digit");
                        None
                    }
                }
            }
            Builtin::AsciiAlphaLower => {
                match match_predicate(self.input, position, |ch| ch.is_ascii_lowercase()) {
                    Some(end) => Some(MatchResult { end, pairs: vec![] }),
                    None => {
                        self.failure.record(position, "pest::ascii_alpha_lower");
                        None
                    }
                }
            }
            Builtin::AsciiAlphaUpper => {
                match match_predicate(self.input, position, |ch| ch.is_ascii_uppercase()) {
                    Some(end) => Some(MatchResult { end, pairs: vec![] }),
                    None => {
                        self.failure.record(position, "pest::ascii_alpha_upper");
                        None
                    }
                }
            }
            Builtin::AsciiAlpha => {
                match match_predicate(self.input, position, |ch| ch.is_ascii_alphabetic()) {
                    Some(end) => Some(MatchResult { end, pairs: vec![] }),
                    None => {
                        self.failure.record(position, "pest::ascii_alpha");
                        None
                    }
                }
            }
            Builtin::AsciiAlphanumeric => {
                match match_predicate(self.input, position, |ch| ch.is_ascii_alphanumeric()) {
                    Some(end) => Some(MatchResult { end, pairs: vec![] }),
                    None => {
                        self.failure.record(position, "pest::ascii_alphanumeric");
                        None
                    }
                }
            }
            Builtin::Ascii => match match_predicate(self.input, position, |ch| ch.is_ascii()) {
                Some(end) => Some(MatchResult { end, pairs: vec![] }),
                None => {
                    self.failure.record(position, "pest::ascii");
                    None
                }
            },
            Builtin::Unicode(property) => {
                let predicate = unicode::by_name(property).expect("unicode property must exist");
                match match_predicate(self.input, position, predicate) {
                    Some(end) => Some(MatchResult { end, pairs: vec![] }),
                    None => {
                        self.failure
                            .record(position, format!("pest::unicode::{property}"));
                        None
                    }
                }
            }
            Builtin::StackPush(node) => match self.eval_node(module, node, position, stack)? {
                Some(result) => {
                    stack.push((position, result.end));
                    self.tracer.event(TraceEvent::StackPush {
                        start: position,
                        end: result.end,
                        depth: stack.len(),
                    });
                    Some(result)
                }
                None => None,
            },
            Builtin::StackPeek(slice) => match peek_stack(self.input, position, stack, *slice) {
                Some(end) => Some(MatchResult { end, pairs: vec![] }),
                None => {
                    self.failure.record(position, "pest::stack::peek");
                    None
                }
            },
            Builtin::StackPeekAll => match peek_all(self.input, position, stack) {
                Some(end) => Some(MatchResult { end, pairs: vec![] }),
                None => {
                    self.failure.record(position, "pest::stack::peek_all");
                    None
                }
            },
            Builtin::StackDrop => match stack.pop() {
                Some((start, end)) => {
                    self.tracer.event(TraceEvent::StackPop {
                        start,
                        end,
                        depth: stack.len(),
                    });
                    Some(MatchResult {
                        end: position,
                        pairs: vec![],
                    })
                }
                None => {
                    self.failure.record(position, "pest::stack::drop");
                    None
                }
            },
            Builtin::StackPop => match stack.pop() {
                Some((start, end)) => {
                    match match_literal(self.input, position, &self.input[start..end]) {
                        Some(next) => {
                            self.tracer.event(TraceEvent::StackPop {
                                start,
                                end,
                                depth: stack.len(),
                            });
                            Some(MatchResult {
                                end: next,
                                pairs: vec![],
                            })
                        }
                        None => {
                            self.failure.record(position, "pest::stack::pop");
                            None
                        }
                    }
                }
                None => {
                    self.failure.record(position, "pest::stack::pop");
                    None
                }
            },
            Builtin::StackPopAll => {
                let popped = stack.clone();
                match peek_all(self.input, position, &popped) {
                    Some(next) => {
                        while let Some((start, end)) = stack.pop() {
                            self.tracer.event(TraceEvent::StackPop {
                                start,
                                end,
                                depth: stack.len(),
                            });
                        }
                        Some(MatchResult {
                            end: next,
                            pairs: vec![],
                        })
                    }
                    None => {
                        self.failure.record(position, "pest::stack::pop_all");
                        None
                    }
                }
            }
        };
        Ok(result)
    }

    fn apply_trivia(
        &mut self,
        module: &GrammarModule,
        trivia: &pest3_meta::parser::Trivia,
        position: usize,
        stack: &mut Vec<(usize, usize)>,
    ) -> Result<Option<usize>, Error> {
        let Some(rule) = (match trivia {
            pest3_meta::parser::Trivia::None => return Ok(Some(position)),
            pest3_meta::parser::Trivia::Optional => self.vm.trivia_rule(module, false),
            pest3_meta::parser::Trivia::Mandatory => self.vm.trivia_rule(module, true),
        }) else {
            return Ok(Some(position));
        };

        match self.eval_rule_target(module, rule, &None, position, stack)? {
            Some(result) => Ok(Some(result.end)),
            None if matches!(trivia, pest3_meta::parser::Trivia::Optional) => Ok(Some(position)),
            None => Ok(None),
        }
    }

    fn eval_repetition(
        &mut self,
        module: &GrammarModule,
        node: &ParseNode,
        position: usize,
        stack: &mut Vec<(usize, usize)>,
        min: usize,
        max: Option<usize>,
    ) -> Result<Option<MatchResult>, Error> {
        let (item, trivia) = match &node.expr {
            ParseExpr::Separated(item, trivia) => (&**item, trivia),
            _ => (node, &pest3_meta::parser::Trivia::None),
        };
        let mut current = position;
        let mut pairs = Vec::new();
        let mut count = 0usize;

        while max.is_none_or(|max| count < max) {
            let attempt = match self.eval_node(module, item, current, stack)? {
                Some(attempt) => attempt,
                None => break,
            };
            if attempt.end == current {
                break;
            }
            current = attempt.end;
            pairs.extend(attempt.pairs);
            count += 1;

            let trivia_position = match self.apply_trivia(module, trivia, current, stack)? {
                Some(pos) => pos,
                None => break,
            };
            if trivia_position == current {
                continue;
            }
            current = trivia_position;
        }

        if count < min {
            return Ok(None);
        }
        Ok(Some(MatchResult {
            end: current,
            pairs,
        }))
    }
}

fn expect_no_args(path: &[String], args: &Option<PathArgs>) -> Result<(), Error> {
    match args {
        None => Ok(()),
        Some(PathArgs::Call(args)) if args.is_empty() => Ok(()),
        Some(PathArgs::Call(args)) => Err(Error::ArgumentCount {
            path: path.to_vec(),
            expected: 0,
            actual: args.len(),
        }),
        Some(PathArgs::Slice(_)) => Err(Error::UnsupportedSlice(path.to_vec())),
    }
}

fn expect_call<'a>(
    path: &[String],
    args: &'a Option<PathArgs>,
    expected: Option<usize>,
) -> Result<&'a [ParseNode], Error> {
    match args {
        Some(PathArgs::Call(args)) => {
            if let Some(expected) = expected {
                if args.len() != expected {
                    return Err(Error::ArgumentCount {
                        path: path.to_vec(),
                        expected,
                        actual: args.len(),
                    });
                }
            }
            Ok(args)
        }
        Some(PathArgs::Slice(_)) => Err(Error::UnsupportedSlice(path.to_vec())),
        None => Err(Error::ArgumentCount {
            path: path.to_vec(),
            expected: expected.unwrap_or(1),
            actual: 0,
        }),
    }
}

fn match_literal(input: &str, position: usize, value: &str) -> Option<usize> {
    let end = position.checked_add(value.len())?;
    (input.as_bytes().get(position..end) == Some(value.as_bytes())).then_some(end)
}

fn match_insensitive(input: &str, position: usize, value: &str) -> Option<usize> {
    let end = position.checked_add(value.len())?;
    input
        .get(position..end)
        .filter(|slice| slice.eq_ignore_ascii_case(value))
        .map(|_| end)
}

fn match_range(input: &str, position: usize, start: char, end: char) -> Option<usize> {
    match_predicate(input, position, |ch| start <= ch && ch <= end)
}

fn next_char_end(input: &str, position: usize) -> Option<usize> {
    input[position..]
        .chars()
        .next()
        .map(|ch| position + ch.len_utf8())
}

fn match_predicate(
    input: &str,
    position: usize,
    predicate: impl FnOnce(char) -> bool,
) -> Option<usize> {
    let ch = input[position..].chars().next()?;
    predicate(ch).then_some(position + ch.len_utf8())
}

fn match_newline(input: &str, position: usize) -> Option<usize> {
    match_literal(input, position, "\r\n")
        .or_else(|| match_literal(input, position, "\n"))
        .or_else(|| match_literal(input, position, "\r"))
}

fn constrain_index(index: isize, len: usize) -> Option<usize> {
    if index > len as isize {
        None
    } else if index >= 0 {
        Some(index as usize)
    } else if index >= -(len as isize) {
        Some((index + len as isize) as usize)
    } else {
        None
    }
}

fn constrain_range(start: isize, end: Option<isize>, len: usize) -> Option<std::ops::Range<usize>> {
    let start = constrain_index(start, len)?;
    let end = end.map_or(Some(len), |end| constrain_index(end, len))?;
    Some(start..end)
}

fn match_span_sequence(
    input: &str,
    mut position: usize,
    spans: impl IntoIterator<Item = (usize, usize)>,
) -> Option<usize> {
    for (start, end) in spans {
        position = match_literal(input, position, input.get(start..end)?)?;
    }
    Some(position)
}

fn peek_all(input: &str, position: usize, stack: &[(usize, usize)]) -> Option<usize> {
    match_span_sequence(input, position, stack.iter().rev().copied())
}

fn peek_stack(
    input: &str,
    position: usize,
    stack: &[(usize, usize)],
    slice: Option<(isize, Option<isize>)>,
) -> Option<usize> {
    match slice {
        None => match stack.last().copied() {
            Some(span) => match_span_sequence(input, position, [span]),
            None => None,
        },
        Some((start, end)) => {
            let range = constrain_range(start, end, stack.len())?;
            if range.end <= range.start {
                return Some(position);
            }
            match_span_sequence(input, position, stack[range].iter().copied())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn vm_from_inline(grammar: &str) -> Vm {
        Vm::from_source(
            grammar,
            "/home/runner/work/pest3/pest3/vm/tests/inline.pest",
        )
        .unwrap()
    }

    #[test]
    fn parses_meta_rules_and_trivia() {
        let vm = vm_from_inline(
            r#"
item = "a"
list(e) = e ~ ("," ~ e)*
main = list(item)
~ = " "*
"#,
        );

        let output = vm.parse("main", "a, a").unwrap();
        assert_eq!(output.pairs.len(), 1);
        assert_eq!(output.pairs[0].rule, "main");
        assert_eq!(output.end, 4);
    }

    #[test]
    fn supports_tracing_hooks() {
        let vm = vm_from_inline(
            r#"
main = pest::soi - "a" - pest::eoi
"#,
        );

        let mut saw_enter_rule = false;
        let mut saw_expr_success = false;
        vm.parse_with_tracer("main", "a", &mut |event: TraceEvent<'_>| match event {
            TraceEvent::EnterRule { name: "main", .. } => saw_enter_rule = true,
            TraceEvent::ExitExpr { end: Some(1), .. } => saw_expr_success = true,
            _ => {}
        })
        .unwrap();

        assert!(saw_enter_rule);
        assert!(saw_expr_success);
    }

    #[test]
    fn parses_runtime_grammar_with_imports_and_stack_ops() {
        let grammar = include_str!("/home/runner/work/pest3/pest3/meta/tests/pest3sample.pest");
        let vm = Vm::from_source(
            grammar,
            "/home/runner/work/pest3/pest3/meta/tests/pest3sample.pest",
        )
        .unwrap();

        vm.parse("peek_", "0111").unwrap();
        vm.parse("checkpoint_restore", "a").unwrap();
    }
}
