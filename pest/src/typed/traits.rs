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
        let (_input, _eoi) = match tracker.record_during_with_option(
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

/// Node of concrete syntax tree that never fails.
pub trait NeverFailedTypedNode<'i, R: RuleType>
where
    Self: Sized + Debug + Clone + PartialEq + Default,
{
    /// Create typed node.
    /// `ATOMIC` refers to the external status, and it can be overriden by rule definition.
    fn parse_with(input: Position<'i>, stack: &mut Stack<Span<'i>>) -> (Position<'i>, Self);
}
