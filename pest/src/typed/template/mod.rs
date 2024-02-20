//! Predefined tree nodes generics.
//! The generator may use this for convenience.
//! Normally you don't need to reference this module by yourself.

//! Copied from [pest-typed/predefined_node/mod.rs] and modified.
//!
//! [pest-typed/predefined_node/mod.rs]: https://github.com/TheVeryDarkness/pest-typed/blob/0.12.1/main/src/predefined_node/mod.rs

mod repetition;

use super::{
    tracker::Tracker,
    traits::{EmptyPairContainer, PairContainer},
    wrapper::String as StringWrapper,
    NeverFailedTypedNode, RuleType, TypedNode,
};
use crate::{Position, Span, Stack};
use core::{
    fmt::Debug,
    marker::PhantomData,
    ops::{Deref, DerefMut, Range},
};
pub use repetition::{Rep, RepMax, RepMin, RepMinMax, RepOnce};

/// Match given string case sensitively.
///
/// The `CONTENT` on the type (by [`StringWrapper`]) is the original string to match.
///
/// See [`Insens`] for case-insensitive matching.
#[derive(Clone, Hash, PartialEq, Eq)]
pub struct Str<T: StringWrapper + 'static>(PhantomData<T>);
impl<T: StringWrapper> StringWrapper for Str<T> {
    const CONTENT: &'static str = T::CONTENT;
}
impl<T: StringWrapper> From<()> for Str<T> {
    fn from(_value: ()) -> Self {
        Self(PhantomData)
    }
}
impl<'i, R: RuleType, T: StringWrapper> TypedNode<'i, R> for Str<T> {
    fn try_parse_with_partial(
        mut input: Position<'i>,
        _stack: &mut Stack<Span<'i>>,
        _tracker: &mut Tracker<'i, R>,
    ) -> Option<(Position<'i>, Self)> {
        if input.match_string(Self::CONTENT) {
            Some((input, Self::from(())))
        } else {
            None
        }
    }
}
impl<T: StringWrapper> Debug for Str<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Str").field(&T::CONTENT).finish()
    }
}
impl<T: StringWrapper> EmptyPairContainer for Str<T> {}

/// Match given string case insensitively.
///
/// - The field `content` is the matched string.
/// - The `CONTENT` on the type (by [`StringWrapper`]) is the original string to match, and it may differ from `content` in case.
///   
///   For example, A `^"x"` may match `"X"`, and in the parsing result, `self.content` is `"X"`, while `Self::CONTENT` is still `"x"`.    
///
/// See [`Str`] for case-sensitive matching.
#[derive(Clone, Hash, PartialEq, Eq)]
pub struct Insens<'i, T: StringWrapper> {
    /// Matched content.
    pub content: &'i str,
    _phantom: PhantomData<&'i T>,
}
impl<'i, T: StringWrapper> StringWrapper for Insens<'i, T> {
    const CONTENT: &'static str = T::CONTENT;
}
impl<'i, T: StringWrapper> From<&'i str> for Insens<'i, T> {
    fn from(content: &'i str) -> Self {
        Self {
            content,
            _phantom: PhantomData,
        }
    }
}
impl<'i, R: RuleType, T: StringWrapper> TypedNode<'i, R> for Insens<'i, T> {
    fn try_parse_with_partial(
        mut input: Position<'i>,
        _stack: &mut Stack<Span<'i>>,
        _tracker: &mut Tracker<'i, R>,
    ) -> Option<(Position<'i>, Self)> {
        let start = input;
        if input.match_insensitive(Self::CONTENT) {
            let span = start.span(&input);
            Some((input, Self::from(span.as_str())))
        } else {
            None
        }
    }
}
impl<'i, T: StringWrapper> Debug for Insens<'i, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Insens")
            .field(&T::CONTENT)
            .field(&self.content)
            .finish()
    }
}
impl<'i, T: StringWrapper> EmptyPairContainer for Insens<'i, T> {}

/// Skip `n` characters if there are.
#[derive(Clone, Hash, PartialEq, Eq)]
pub struct SkipChar<'i, const N: usize> {
    /// Skipped span.
    pub span: Span<'i>,
}
impl<'i, R: RuleType, const N: usize> TypedNode<'i, R> for SkipChar<'i, N> {
    fn try_parse_with_partial(
        mut input: Position<'i>,
        _stack: &mut Stack<Span<'i>>,
        _tracker: &mut Tracker<'i, R>,
    ) -> Option<(Position<'i>, Self)> {
        let start = input;
        match input.skip(N) {
            true => {
                let span = start.span(&input);
                Some((input, Self { span }))
            }
            false => None,
        }
    }
}
impl<'i, const N: usize> Debug for SkipChar<'i, N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("SkipChar")
            .field(&N)
            .field(&self.span)
            .finish()
    }
}
impl<'i, const N: usize> EmptyPairContainer for SkipChar<'i, N> {}

/// Match a character in the range `[MIN, MAX]`.
/// Inclusively both below and above.
#[derive(Clone, Hash, PartialEq, Eq)]
pub struct CharRange<const MIN: char, const MAX: char> {
    /// Matched character.
    pub content: char,
}
impl<'i, R: RuleType, const MIN: char, const MAX: char> TypedNode<'i, R> for CharRange<MIN, MAX> {
    #[inline]
    fn try_parse_with_partial(
        mut input: Position<'i>,
        _stack: &mut Stack<Span<'i>>,
        _tracker: &mut Tracker<'i, R>,
    ) -> Option<(Position<'i>, Self)> {
        let start = input;
        match input.match_range(MIN..MAX) {
            true => {
                let span = start.span(&input);
                let content = span.as_str().chars().next().unwrap();
                Some((input, Self { content }))
            }
            false => None,
        }
    }
}
impl<const MIN: char, const MAX: char> Debug for CharRange<MIN, MAX> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Char")
            .field(&MIN)
            .field(&MAX)
            .field(&self.content)
            .finish()
    }
}
impl<const MIN: char, const MAX: char> EmptyPairContainer for CharRange<MIN, MAX> {}

/// Match exact character `CHAR`.
#[derive(Clone, Hash, PartialEq, Eq)]
pub struct Char<const CHAR: char>;
impl<'i, R: RuleType, const CHAR: char> TypedNode<'i, R> for Char<CHAR> {
    #[inline]
    fn try_parse_with_partial(
        mut input: Position<'i>,
        _stack: &mut Stack<Span<'i>>,
        _tracker: &mut Tracker<'i, R>,
    ) -> Option<(Position<'i>, Self)> {
        match input.match_char_by(|c| c == CHAR) {
            true => Some((input, Self)),
            false => None,
        }
    }
}
impl<const CHAR: char> Debug for Char<CHAR> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Char").field(&CHAR).finish()
    }
}
impl<const CHAR: char> EmptyPairContainer for Char<CHAR> {}

fn constrain_idx(index: isize, len: usize) -> Option<usize> {
    if index > (len as isize) {
        None
    } else if index >= 0 {
        Some(index as usize)
    } else if index >= -(len as isize) {
        Some(((index as isize) + (len as isize)) as usize)
    } else {
        None
    }
}

fn constrain_idxs(start: isize, end: Option<isize>, len: usize) -> Option<Range<usize>> {
    let start = constrain_idx(start, len);
    let end = match end {
        Some(end) => constrain_idx(end, len),
        None => Some(len),
    };
    match (start, end) {
        (Some(start), Some(end)) => Some(start..end),
        _ => None,
    }
}

/// Try to create stack slice.
#[inline]
fn stack_slice<'i, 's, R: RuleType>(
    input: Position<'i>,
    start: isize,
    end: Option<isize>,
    stack: &'s Stack<Span<'i>>,
    tracker: &mut Tracker<'i, R>,
) -> Option<core::slice::Iter<'s, Span<'i>>> {
    let range = match constrain_idxs(start, end, stack.len()) {
        Some(range) => range,
        None => {
            tracker.out_of_bound(input, start, end);
            return None;
        }
    };
    // return true if an empty sequence is requested
    if range.end <= range.start {
        return Some(core::slice::Iter::default());
    }
    Some(stack[range].iter())
}

/// Match a part of the stack without popping.
/// Will match (consume) input.
#[inline]
fn peek_spans<'s, 'i: 's, R: RuleType>(
    input: Position<'i>,
    iter: impl Iterator<Item = &'s Span<'i>>,
    _tracker: &mut Tracker<'i, R>,
) -> Option<(Position<'i>, Span<'i>)> {
    let mut matching_pos = input;
    for span in iter {
        match matching_pos.match_string(span.as_str()) {
            true => (),
            false => {
                return None;
            }
        }
    }
    Some((matching_pos, input.span(&matching_pos)))
}

/// Positive predicate.
///
/// Peeked expressions will not occur in Pair/Pairs API.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Positive<T> {
    /// Peeked content.
    pub content: T,
}
impl<T> From<T> for Positive<T> {
    fn from(content: T) -> Self {
        Self { content }
    }
}
impl<T> Deref for Positive<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.content
    }
}
impl<T> DerefMut for Positive<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.content
    }
}
impl<'i, R: RuleType, T: TypedNode<'i, R>> TypedNode<'i, R> for Positive<T> {
    fn try_parse_with_partial(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
        tracker: &mut Tracker<'i, R>,
    ) -> Option<(Position<'i>, Self)> {
        tracker.positive_during(|tracker| {
            stack.snapshot();
            match T::try_parse_with_partial(input, stack, tracker) {
                Some((_pos, content)) => {
                    stack.restore();
                    Some((input, Self::from(content)))
                }
                None => {
                    stack.restore();
                    None
                }
            }
        })
    }
}
impl<T> EmptyPairContainer for Positive<T> {}

/// Negative predicate.
///
/// Will not contain anything.
#[derive(Clone, Hash, PartialEq, Eq)]
pub struct Negative<T> {
    _phantom: PhantomData<T>,
}
impl<T> From<()> for Negative<T> {
    fn from(_value: ()) -> Self {
        Self {
            _phantom: PhantomData,
        }
    }
}
impl<'i, R: RuleType, T: TypedNode<'i, R>> TypedNode<'i, R> for Negative<T> {
    fn try_parse_with_partial(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
        tracker: &mut Tracker<'i, R>,
    ) -> Option<(Position<'i>, Self)> {
        tracker.negative_during(|tracker| {
            stack.snapshot();
            match T::try_parse_with_partial(input, stack, tracker) {
                Some(_) => {
                    stack.restore();
                    None
                }
                None => {
                    stack.restore();
                    Some((input, Self::from(())))
                }
            }
        })
    }
}
impl<T> Debug for Negative<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Negative").finish()
    }
}
impl<N> EmptyPairContainer for Negative<N> {}

/// Match any character.
#[derive(Clone, Hash, PartialEq, Eq)]
pub struct ANY {
    /// Matched character.
    pub content: char,
}
impl<'i, R: RuleType> TypedNode<'i, R> for ANY {
    #[inline]
    fn try_parse_with_partial(
        mut input: Position<'i>,
        _stack: &mut Stack<Span<'i>>,
        _tracker: &mut Tracker<'i, R>,
    ) -> Option<(Position<'i>, Self)> {
        let mut c: char = ' ';
        match input.match_char_by(|ch| {
            c = ch;
            true
        }) {
            true => Some((input, Self { content: c })),
            false => None,
        }
    }
}
impl Debug for ANY {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("ANY").field(&self.content).finish()
    }
}
impl EmptyPairContainer for ANY {}

/// Never matches.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NONE;
impl<'i, R: RuleType> TypedNode<'i, R> for NONE {
    #[inline]
    fn try_parse_with_partial(
        _input: Position<'i>,
        _stack: &mut Stack<Span<'i>>,
        _tracker: &mut Tracker<'i, R>,
    ) -> Option<(Position<'i>, Self)> {
        None
    }
}
impl EmptyPairContainer for NONE {}

/// Always accept and consume no input.
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
pub struct Empty;
impl<'i, R: RuleType> TypedNode<'i, R> for Empty {
    #[inline]
    fn try_parse_with_partial(
        input: Position<'i>,
        _stack: &mut Stack<Span<'i>>,
        _tracker: &mut Tracker<'i, R>,
    ) -> Option<(Position<'i>, Self)> {
        Some((input, Self))
    }
}
impl<'i, R: RuleType> NeverFailedTypedNode<'i, R> for Empty {
    fn parse_with_partial(
        input: Position<'i>,
        _stack: &mut Stack<Span<'i>>,
    ) -> (Position<'i>, Self) {
        (input, Self)
    }
}
impl EmptyPairContainer for Empty {}

/// Match the start of input.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SOI;
impl<'i, R: RuleType> TypedNode<'i, R> for SOI {
    #[inline]
    fn try_parse_with_partial(
        input: Position<'i>,
        _stack: &mut Stack<Span<'i>>,
        _tracker: &mut Tracker<'i, R>,
    ) -> Option<(Position<'i>, Self)> {
        if input.at_start() {
            Some((input, Self))
        } else {
            None
        }
    }
}
impl EmptyPairContainer for SOI {}

/// Match the end of input.
///
/// [`EOI`] will record its rule if not matched.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct EOI;
impl<'i, R: RuleType> TypedNode<'i, R> for EOI {
    #[inline]
    fn try_parse_with_partial(
        input: Position<'i>,
        _stack: &mut Stack<Span<'i>>,
        _tracker: &mut Tracker<'i, R>,
    ) -> Option<(Position<'i>, Self)> {
        if input.at_end() {
            Some((input, Self))
        } else {
            None
        }
    }
}
impl EmptyPairContainer for EOI {}

/// Type of eol.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum NewLineType {
    /// `\r\n`
    CRLF,
    /// `\n`
    LF,
    /// `\r`
    CR,
}

/// Match a new line character.
/// A built-in rule. Equivalent to `"\r\n" | "\n" | "\r"`.
#[derive(Clone, Hash, PartialEq, Eq)]
pub struct NEWLINE {
    /// Type of matched character.
    pub content: NewLineType,
}
impl<'i, R: RuleType> TypedNode<'i, R> for NEWLINE {
    #[inline]
    fn try_parse_with_partial(
        mut input: Position<'i>,
        _stack: &mut Stack<Span<'i>>,
        _tracker: &mut Tracker<'i, R>,
    ) -> Option<(Position<'i>, Self)> {
        let (input, t) = if input.match_string("\r\n") {
            (input, NewLineType::CRLF)
        } else if input.match_string("\n") {
            (input, NewLineType::LF)
        } else if input.match_string("\r") {
            (input, NewLineType::CR)
        } else {
            return None;
        };
        Some((input, Self { content: t }))
    }
}
impl Debug for NEWLINE {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("NEWLINE").field(&self.content).finish()
    }
}
impl EmptyPairContainer for NEWLINE {}

/// Peek all spans in stack reversely.
/// Will consume input.
#[allow(non_camel_case_types)]
#[derive(Clone, Hash, PartialEq, Eq)]
pub struct PEEK_ALL<'i> {
    /// Pair span.
    pub span: Span<'i>,
}
impl<'i, R: RuleType> TypedNode<'i, R> for PEEK_ALL<'i> {
    #[inline]
    fn try_parse_with_partial(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
        tracker: &mut Tracker<'i, R>,
    ) -> Option<(Position<'i>, Self)> {
        let spans = stack[0..stack.len()].iter().rev();
        let (input, span) = peek_spans::<R>(input, spans, tracker)?;
        Some((input, Self { span }))
    }
}
impl<'i> Debug for PEEK_ALL<'i> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("PEEK_ALL").field(&self.span).finish()
    }
}
impl<'i> EmptyPairContainer for PEEK_ALL<'i> {}

/// Peek top span in stack.
/// Will consume input.
#[derive(Clone, Hash, PartialEq, Eq)]
pub struct PEEK<'i> {
    /// Pair span.
    pub span: Span<'i>,
}
impl<'i> From<Span<'i>> for PEEK<'i> {
    fn from(span: Span<'i>) -> Self {
        Self { span }
    }
}
impl<'i, R: RuleType> TypedNode<'i, R> for PEEK<'i> {
    #[inline]
    fn try_parse_with_partial(
        mut input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
        tracker: &mut Tracker<'i, R>,
    ) -> Option<(Position<'i>, Self)> {
        let start = input;
        match stack.peek() {
            Some(string) => match input.match_string(string.as_str()) {
                true => Some((input, Self::from(start.span(&input)))),
                false => None,
            },
            None => {
                tracker.empty_stack(input);
                None
            }
        }
    }
}
impl<'i> Debug for PEEK<'i> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("PEEK").field(&self.span).finish()
    }
}
impl<'i> EmptyPairContainer for PEEK<'i> {}

/// Drop the top of the stack.
///
/// Fail if there is no span in the stack.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct DROP;
impl<'i, R: RuleType> TypedNode<'i, R> for DROP {
    #[inline]
    fn try_parse_with_partial(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
        tracker: &mut Tracker<'i, R>,
    ) -> Option<(Position<'i>, Self)> {
        match stack.pop() {
            Some(_) => Some((input, Self)),
            None => {
                tracker.empty_stack(input);
                None
            }
        }
    }
}
impl EmptyPairContainer for DROP {}

/// Match and pop the top span of the stack.
#[derive(Clone, Hash, PartialEq, Eq)]
pub struct POP<'i> {
    /// Matched span.
    pub span: Span<'i>,
}

impl<'i> From<Span<'i>> for POP<'i> {
    fn from(span: Span<'i>) -> Self {
        Self { span }
    }
}
impl<'i, R: RuleType> TypedNode<'i, R> for POP<'i> {
    #[inline]
    fn try_parse_with_partial(
        mut input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
        tracker: &mut Tracker<'i, R>,
    ) -> Option<(Position<'i>, Self)> {
        match stack.pop() {
            Some(span) => match input.match_string(span.as_str()) {
                true => Some((input, Self::from(span))),
                false => None,
            },
            None => {
                tracker.empty_stack(input);
                None
            }
        }
    }
}
impl<'i> Debug for POP<'i> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("POP").field(&self.span).finish()
    }
}
impl<'i> EmptyPairContainer for POP<'i> {}

/// Match and pop all spans in the stack in top-to-bottom-order.
#[allow(non_camel_case_types)]
#[derive(Clone, Hash, PartialEq, Eq)]
pub struct POP_ALL<'i> {
    /// Matched span.
    pub span: Span<'i>,
}
impl<'i> From<Span<'i>> for POP_ALL<'i> {
    fn from(span: Span<'i>) -> Self {
        Self { span }
    }
}
impl<'i, R: RuleType> TypedNode<'i, R> for POP_ALL<'i> {
    #[inline]
    fn try_parse_with_partial(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
        tracker: &mut Tracker<'i, R>,
    ) -> Option<(Position<'i>, Self)> {
        let (input, res) = PEEK_ALL::try_parse_with_partial(input, stack, tracker)?;
        while stack.pop().is_some() {}
        Some((input, Self::from(res.span)))
    }
}
impl<'i> Debug for POP_ALL<'i> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("POP_ALL").field(&self.span).finish()
    }
}
impl<'i> EmptyPairContainer for POP_ALL<'i> {}

/// Match an expression and push it to the [Stack].
#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct PUSH<T> {
    /// Matched content.
    pub content: T,
}
impl<T> From<T> for PUSH<T> {
    fn from(content: T) -> Self {
        Self { content }
    }
}
impl<'i, R: RuleType, T: TypedNode<'i, R>> TypedNode<'i, R> for PUSH<T> {
    #[inline]
    fn try_parse_with_partial(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
        tracker: &mut Tracker<'i, R>,
    ) -> Option<(Position<'i>, Self)> {
        let start = input;
        let (input, content) = T::try_parse_with_partial(input, stack, tracker)?;
        stack.push(start.span(&input));
        Some((input, Self::from(content)))
    }
}
impl<T> Deref for PUSH<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.content
    }
}
impl<T> DerefMut for PUSH<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.content
    }
}
impl<T: Debug> Debug for PUSH<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("PUSH").field(&self.content).finish()
    }
}
impl<R: RuleType, T: PairContainer<R>> PairContainer<R> for PUSH<T> {
    fn for_each_child_pair(&self, f: &mut impl FnMut(crate::token::Pair<R>)) {
        self.content.for_self_or_for_each_child_pair(f)
    }
    fn vec_children_pairs(&self) -> Vec<crate::token::Pair<R>> {
        self.content.vec_children_pairs()
    }
}

/// Match `[START..END]` in top-to-bottom order of the stack.
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct PeekSlice2<const START: isize, const END: isize>;
impl<'i, R: RuleType, const START: isize, const END: isize> TypedNode<'i, R>
    for PeekSlice2<START, END>
{
    #[inline]
    fn try_parse_with_partial(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
        tracker: &mut Tracker<'i, R>,
    ) -> Option<(Position<'i>, Self)> {
        let spans = stack_slice(input, START, Some(END), stack, tracker)?;
        let (input, _) = peek_spans::<R>(input, spans, tracker)?;
        Some((input, Self))
    }
}
impl<const START: isize, const END: isize> EmptyPairContainer for PeekSlice2<START, END> {}

/// Match `[START..]` in top-to-bottom order of the stack.
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct PeekSlice1<const START: isize>;
impl<'i, R: RuleType, const START: isize> TypedNode<'i, R> for PeekSlice1<START> {
    #[inline]
    fn try_parse_with_partial(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
        tracker: &mut Tracker<'i, R>,
    ) -> Option<(Position<'i>, Self)> {
        let spans = stack_slice(input, START, None, stack, tracker)?;
        let (input, _) = peek_spans::<R>(input, spans, tracker)?;
        Some((input, Self))
    }
}
impl<const START: isize> EmptyPairContainer for PeekSlice1<START> {}

/// ASCII Digit. `'0'..'9'`
#[allow(non_camel_case_types)]
pub type ASCII_DIGIT = CharRange<'0', '9'>;

/// Non-zero ASCII Digit. `'1'..'9'`
#[allow(non_camel_case_types)]
pub type ASCII_NONZERO_DIGIT = CharRange<'1', '9'>;

/// Binary ASCII Digit. `'0'..'1'`
#[allow(non_camel_case_types)]
pub type ASCII_BIN_DIGIT = CharRange<'0', '1'>;

/// Octal ASCII Digit. `'0'..'7'`
#[allow(non_camel_case_types)]
pub type ASCII_OCT_DIGIT = CharRange<'0', '7'>;

use crate::choice::{Choice2, Choice3};
/// Hexadecimal ASCII Digit. `'0'..'9' | 'a'..'f' | 'A'..'F'`
#[allow(non_camel_case_types)]
pub type ASCII_HEX_DIGIT = Choice3<ASCII_DIGIT, CharRange<'a', 'f'>, CharRange<'A', 'F'>>;

/// Lower case ASCII alphabet.
#[allow(non_camel_case_types)]
pub type ASCII_ALPHA_LOWER = CharRange<'a', 'z'>;

/// Upper case ASCII alphabet.
#[allow(non_camel_case_types)]
pub type ASCII_ALPHA_UPPER = CharRange<'A', 'Z'>;

/// ASCII alphabet.
#[allow(non_camel_case_types)]
pub type ASCII_ALPHA = Choice2<ASCII_ALPHA_LOWER, ASCII_ALPHA_UPPER>;

/// ASCII alphabet or digit.
#[allow(non_camel_case_types)]
pub type ASCII_ALPHANUMERIC = Choice2<ASCII_ALPHA, ASCII_DIGIT>;

/// ASCII alphabet.
#[allow(non_camel_case_types)]
pub type ASCII = CharRange<'\x00', '\x7f'>;

/// Match char by a predicate.
///
/// Return Some(char) if matched.
pub fn match_char_by(position: &mut Position<'_>, pred: impl FnOnce(char) -> bool) -> Option<char> {
    let mut res = None;
    position.match_char_by(|c| {
        let matched = pred(c);
        if matched {
            res = Some(c);
        }
        matched
    });
    res
}

/// Restore on error.
pub fn restore_on_none<'i, T>(
    stack: &mut Stack<Span<'i>>,
    f: impl FnOnce(&mut Stack<Span<'i>>) -> Option<T>,
) -> Option<T> {
    stack.snapshot();
    let res = f(stack);
    match res {
        Some(_) => stack.clear_snapshot(),
        None => stack.restore(),
    }
    res
}

/// Handle trivia rule defined in [RuleType].
#[inline]
pub fn try_handle_trivia<'i, R: RuleType, const TRIVIA: u8>(
    input: Position<'i>,
    stack: &mut Stack<Span<'i>>,
    tracker: &mut Tracker<'i, R>,
) -> Option<Position<'i>> {
    let input = match TRIVIA {
        // None.
        0 => input,
        // Optional.
        1 => R::OptionalTrivia::try_parse_with_partial(input, stack, tracker)?.0,
        // Mandatory.
        2 => R::MandatoryTrivia::try_parse_with_partial(input, stack, tracker)?.0,
        _ => unreachable!(),
    };
    Some(input)
}
