use super::{template::EOI, wrapper};
use crate::{token::Pair, typed::tracker::Tracker, Position, Span};
use core::{fmt::Debug, hash::Hash};
use pest2::{error::Error, Stack};

/// A trait which parser rules must implement.
///
/// See [pest2::RuleType].
pub trait RuleType: Copy + Debug + Eq + Hash + Ord {
    /// End of input.
    const EOI: Self;
    /// Whitespaces and comments that may exist and be ignored.
    type OptionalTrivia<'i>: TypedNode<'i, Self>;
    /// Whitespaces and comments that must exist and be ignored.
    type MandatoryTrivia<'i>: TypedNode<'i, Self>;
}

/// Node of a typed syntax tree.
pub trait TypedNode<'i, R: RuleType>: Sized {
    /// Try parse a part of or all of remained string into a node.
    fn try_parse_with_partial(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
        tracker: &mut Tracker<'i, R>,
    ) -> Option<(Position<'i>, Self)>;
    /// Try parse remained string into a node.
    #[inline]
    fn try_parse_with(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
        tracker: &mut Tracker<'i, R>,
    ) -> Option<Self> {
        let (input, res) = Self::try_parse_with_partial(input, stack, tracker)?;
        let (_input, _eoi) = tracker.record_option_during_with(
            input,
            |tracker| EOI::try_parse_with_partial(input, stack, tracker),
            <R as RuleType>::EOI,
        )?;
        Some(res)
    }
    /// Try parsing leading part of string into a node.
    #[inline]
    fn try_parse_partial(input: &'i str) -> Result<(Position<'i>, Self), Error<R>> {
        let mut stack = Stack::new();
        let input = Position::from_start(input);
        let mut tracker = Tracker::new(input);
        Self::try_parse_with_partial(input, &mut stack, &mut tracker)
            .ok_or_else(|| tracker.collect())
    }
    /// Try parsing given string into a node.
    #[inline]
    fn try_parse(input: &'i str) -> Result<Self, Error<R>> {
        let mut stack = Stack::new();
        let input = Position::from_start(input);
        let mut tracker = Tracker::new(input);
        Self::try_parse_with(input, &mut stack, &mut tracker).ok_or_else(|| tracker.collect())
    }
    /// Check whether the some leading part of the result can be accepted.
    fn check_with_partial(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
        tracker: &mut Tracker<'i, R>,
    ) -> Option<Position<'i>>;
    /// Check whether the some leading part of the result can be accepted.
    #[inline]
    fn check_with(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
        tracker: &mut Tracker<'i, R>,
    ) -> bool {
        let input = match Self::check_with_partial(input, stack, tracker) {
            Some(input) => input,
            None => return false,
        };
        let _input = match tracker.record_option_during_with(
            input,
            |tracker| EOI::check_with_partial(input, stack, tracker),
            <R as RuleType>::EOI,
        ) {
            Some(input) => input,
            None => return false,
        };
        true
    }
    /// Try parsing leading part of string into a node.
    #[inline]
    fn check_partial(input: &'i str) -> Result<Position<'i>, Error<R>> {
        let mut stack = Stack::new();
        let input = Position::from_start(input);
        let mut tracker = Tracker::new(input);
        Self::check_with_partial(input, &mut stack, &mut tracker).ok_or_else(|| tracker.collect())
    }
    /// Try parsing given string into a node.
    #[inline]
    fn check(input: &'i str) -> Result<(), Error<R>> {
        let mut stack = Stack::new();
        let input = Position::from_start(input);
        let mut tracker = Tracker::new(input);
        match Self::check_with(input, &mut stack, &mut tracker) {
            true => Ok(()),
            false => Err(tracker.collect()),
        }
    }
    // /// Whether this node accepts null input.
    // const NULLABLE: bool;
    // /// Leading characters that this node accepts.
    // const FIRST: &'static [char];
}

/// Struct for rules with full capacity.
pub trait FullRuleStruct<'i>: wrapper::Rule {
    /// Wrapped inner type.
    type Inner: TypedNode<'i, Self::Rule>;
    /// Wrapped content type.
    type Content: From<Self::Inner>;
    /// Create from span and content.
    fn new(content: Self::Content, span: Span<'i>) -> Self;
}
impl<'i, R: RuleType, T: FullRuleStruct<'i, Rule = R>> TypedNode<'i, R> for T {
    fn try_parse_with_partial(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
        tracker: &mut Tracker<'i, R>,
    ) -> Option<(Position<'i>, Self)> {
        tracker.record_option_during(input, |tracker| {
            let (pos, content) = <T::Inner>::try_parse_with_partial(input, stack, tracker)?;
            let content = content.into();
            let span = input.span(&pos);
            Some((pos, Self::new(content, span)))
        })
    }
    fn check_with_partial(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
        tracker: &mut Tracker<'i, R>,
    ) -> Option<Position<'i>> {
        tracker.record_empty_during::<T>(input, |tracker| {
            let pos = <T::Inner>::check_with_partial(input, stack, tracker)?;
            Some(pos)
        })
    }
}

/// A container of pairs.
pub trait PairContainer<R> {
    /// for `Self` as a pair if it is, otherwise for each child pair.
    fn for_self_or_for_each_child_pair(&self, f: &mut impl FnMut(Pair<R>)) {
        self.for_each_child_pair(f)
    }
    /// For each child pair.
    fn for_each_child_pair(&self, f: &mut impl FnMut(Pair<R>));
    /// Convert children pairs into a [Vec].
    fn vec_children_pairs(&self) -> Vec<Pair<R>> {
        let mut vec = vec![];
        self.for_each_child_pair(&mut |token| vec.push(token));
        vec
    }
}

impl<R, T: PairContainer<R>> PairContainer<R> for Option<T> {
    fn for_each_child_pair(&self, f: &mut impl FnMut(Pair<R>)) {
        match self {
            Some(val) => val.for_self_or_for_each_child_pair(f),
            None => (),
        }
    }
}

/// Contains no pair.
pub trait EmptyPairContainer {}
impl<R, T: EmptyPairContainer> PairContainer<R> for T {
    fn for_each_child_pair(&self, _f: &mut impl FnMut(Pair<R>)) {}
}

/// A pair that can be converted to a pair tree.
pub trait PairTree<R: RuleType>: PairContainer<R> + wrapper::Rule<Rule = R> {
    /// Get pair span.
    fn get_span(&self) -> (usize, usize);
    /// Convert `Self` to a pair tree.
    fn as_pair_tree(&self) -> Pair<R> {
        let rule = Self::RULE;
        let (start, end) = self.get_span();
        let children = self.vec_children_pairs();
        Pair {
            rule,
            start,
            end,
            children,
        }
    }
}

impl<'i, R: RuleType, T: TypedNode<'i, R>> TypedNode<'i, R> for Option<T> {
    #[inline]
    fn try_parse_with_partial(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
        tracker: &mut Tracker<'i, R>,
    ) -> Option<(Position<'i>, Self)> {
        stack.snapshot();
        match T::try_parse_with_partial(input, stack, tracker) {
            Some((pos, res)) => {
                stack.clear_snapshot();
                Some((pos, Some(res)))
            }
            None => {
                stack.restore();
                Some((input, None))
            }
        }
    }
    #[inline]
    fn check_with_partial(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
        tracker: &mut Tracker<'i, R>,
    ) -> Option<Position<'i>> {
        stack.snapshot();
        match T::check_with_partial(input, stack, tracker) {
            Some(pos) => {
                stack.clear_snapshot();
                Some(pos)
            }
            None => {
                stack.restore();
                Some(input)
            }
        }
    }
}

/// Parser that can produce typed syntax tree.
pub trait TypedParser<R: RuleType> {
    /// See [TypedNode::try_parse_with].
    #[inline]
    fn try_parse_with<'i, T: TypedNode<'i, R>>(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
        tracker: &mut Tracker<'i, R>,
    ) -> Option<T> {
        T::try_parse_with(input, stack, tracker)
    }
    /// See [TypedNode::try_parse_with_partial].
    #[inline]
    fn try_parse_with_partial<'i, T: TypedNode<'i, R>>(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
        tracker: &mut Tracker<'i, R>,
    ) -> Option<(Position<'i>, T)> {
        T::try_parse_with_partial(input, stack, tracker)
    }
    /// See [TypedNode::try_parse].
    #[inline]
    fn try_parse<'i, T: TypedNode<'i, R>>(input: &'i str) -> Result<T, Error<R>> {
        T::try_parse(input)
    }
    /// See [TypedNode::try_parse_partial].
    #[inline]
    fn try_parse_partial<'i, T: TypedNode<'i, R>>(
        input: &'i str,
    ) -> Result<(Position<'i>, T), Error<R>> {
        T::try_parse_partial(input)
    }
}

/// Node of concrete syntax tree that never fails.
pub trait NeverFailedTypedNode<'i, R: RuleType>
where
    Self: Sized + Debug + Clone + PartialEq + Default,
{
    /// Parse leading part of given input into a typed node.
    fn parse_with_partial(input: Position<'i>, stack: &mut Stack<Span<'i>>)
        -> (Position<'i>, Self);
    /// Parse leading part of given input into a typed node.
    fn parse_partial(input: &'i str) -> (Position<'i>, Self) {
        let input = Position::from_start(input);
        let mut stack = Stack::new();
        Self::parse_with_partial(input, &mut stack)
    }
}
