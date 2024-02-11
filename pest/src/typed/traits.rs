use super::template::EOI;
use crate::{typed::tracker::Tracker, Position, Span};
use core::{fmt::Debug, hash::Hash};
use pest2::{error::Error, Stack};

/// A trait which parser rules must implement.
///
/// See [pest2::RuleType].
pub trait RuleType: Copy + Debug + Eq + Hash + Ord {
    /// End of input.
    const EOI: Self;
    /// Whitespaces and comments that you want to ignore.
    type Trivia<'i>: TypedNode<'i, Self>;
}

pub trait TypedNode<'i, R: RuleType>: Sized {
    #[inline]
    fn try_parse_with(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
        tracker: &mut Tracker<'i, R>,
    ) -> Option<Self> {
        let (input, res) = match Self::try_parse_with_partial(input, stack, tracker) {
            Some((input, res)) => (input, res),
            None => return None,
        };
        let (_input, _eoi) = match tracker.record_option_during_with(
            input,
            |tracker| EOI::try_parse_with_partial(input, stack, tracker),
            <R as RuleType>::EOI,
        ) {
            Some((input, res)) => (input, res),
            None => return None,
        };
        Some(res)
    }
    fn try_parse_with_partial(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
        tracker: &mut Tracker<'i, R>,
    ) -> Option<(Position<'i>, Self)>;
    #[inline]
    fn try_parse(input: &'i str) -> Result<Self, Error<R>> {
        let mut stack = Stack::new();
        let input = Position::from_start(input);
        let mut tracker = Tracker::new(input);
        match Self::try_parse_with(input, &mut stack, &mut tracker) {
            Some(res) => Ok(res),
            None => Err(tracker.collect()),
        }
    }
    #[inline]
    fn try_parse_partial(input: &'i str) -> Result<(Position<'i>, Self), Error<R>> {
        let mut stack = Stack::new();
        let input = Position::from_start(input);
        let mut tracker = Tracker::new(input);
        match Self::try_parse_with_partial(input, &mut stack, &mut tracker) {
            Some((input, res)) => Ok((input, res)),
            None => Err(tracker.collect()),
        }
    }
    // /// Whether this node accepts null input.
    // const NULLABLE: bool;
    // /// Leading characters that this node accepts.
    // const FIRST: &'static [char];
}

impl<'i, R: RuleType, T: TypedNode<'i, R>> TypedNode<'i, R> for Option<T> {
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
}

pub trait TypedParser<R: RuleType> {
    #[inline]
    fn try_parse_with<'i, T: TypedNode<'i, R>>(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
        tracker: &mut Tracker<'i, R>,
    ) -> Option<T> {
        T::try_parse_with(input, stack, tracker)
    }
    #[inline]
    fn try_parse_with_partial<'i, T: TypedNode<'i, R>>(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
        tracker: &mut Tracker<'i, R>,
    ) -> Option<(Position<'i>, T)> {
        T::try_parse_with_partial(input, stack, tracker)
    }
    #[inline]
    fn try_parse<'i, T: TypedNode<'i, R>>(input: &'i str) -> Result<T, Error<R>> {
        T::try_parse(input)
    }
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
    /// Create typed node.
    /// `ATOMIC` refers to the external status, and it can be overriden by rule definition.
    fn parse_with(input: Position<'i>, stack: &mut Stack<Span<'i>>) -> (Position<'i>, Self);
}
