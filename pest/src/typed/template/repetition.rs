//! Predefined tree nodes generics.
//! The generator may use this for convenience.
//! Normally you don't need to reference this module by yourself.

use super::{restore_on_none, RuleType};
use crate::{
    typed::{tracker::Tracker, wrapper::Bound as BoundWrapper, NeverFailedTypedNode, TypedNode},
    Position, Span, Stack,
};
use alloc::vec::Vec;
use core::{fmt::Debug, usize};

/// Repeatably match `T` at least `MIN` times and at most `MAX` times.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct RepMinMax<T, const MIN: usize, const MAX: usize> {
    /// Skipped and Matched expressions.
    pub content: Vec<T>,
}

impl<T, const MAX: usize> Default for RepMinMax<T, 0, MAX> {
    fn default() -> Self {
        Self {
            content: Vec::new(),
        }
    }
}
impl<
        'i,
        R: RuleType,
        T: TypedNode<'i, R> + Debug + Clone + PartialEq + Default,
        const MAX: usize,
    > NeverFailedTypedNode<'i, R> for RepMinMax<T, 0, MAX>
{
    #[inline]
    fn parse_with(mut input: Position<'i>, stack: &mut Stack<Span<'i>>) -> (Position<'i>, Self) {
        let mut vec = Vec::new();

        let mut tracker = Tracker::new(input);

        for i in 0..MAX {
            match restore_on_none(stack, |stack| try_parse_unit(input, stack, &mut tracker, i)) {
                Some((next, matched)) => {
                    input = next;
                    vec.push(matched);
                }
                None => {
                    break;
                }
            }
        }

        (input, Self { content: vec })
    }
}
impl<'i, R: RuleType, T: TypedNode<'i, R>, const MIN: usize, const MAX: usize> TypedNode<'i, R>
    for RepMinMax<T, MIN, MAX>
{
    #[inline]
    fn try_parse_with_partial(
        mut input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
        tracker: &mut Tracker<'i, R>,
    ) -> Option<(Position<'i>, Self)> {
        let mut vec = Vec::new();

        for i in 0..MAX {
            match restore_on_none(stack, |stack| try_parse_unit(input, stack, tracker, i)) {
                Some((next, matched)) => {
                    input = next;
                    vec.push(matched);
                }
                None => {
                    if i < MIN {
                        return None;
                    } else {
                        break;
                    }
                }
            }
        }

        Some((input, Self { content: vec }))
    }
}
impl<T, const MIN: usize, const MAX: usize> RepMinMax<T, MIN, MAX> {
    /// Returns an iterator over all skipped or matched expressions by reference.
    #[allow(clippy::needless_lifetimes)]
    pub fn iter_all<'n>(&'n self) -> alloc::slice::Iter<'n, T> {
        self.content.iter()
    }
    /// Returns an iterator over all skipped or matched expressions by value.
    pub fn into_iter_all(self) -> alloc::vec::IntoIter<T> {
        self.content.into_iter()
    }
}
impl<T: Clone + PartialEq, const MIN: usize, const MAX: usize> BoundWrapper
    for RepMinMax<T, MIN, MAX>
{
    const MIN: usize = MIN;
    const MAX: usize = MAX;
}

/// Repeat at least one times.
pub type RepMin<T, const MIN: usize> = RepMinMax<T, MIN, { usize::MAX }>;
/// Repeat at least one times.
pub type RepMax<T, const MAX: usize> = RepMinMax<T, 0, MAX>;
/// Repeat arbitrary times.
pub type Rep<T> = RepMin<T, 0>;
/// Repeat at least one times.
pub type RepOnce<T> = RepMin<T, 1>;

#[inline]
fn try_parse_unit<'i, R: RuleType, T: TypedNode<'i, R>>(
    mut input: Position<'i>,
    stack: &mut Stack<Span<'i>>,
    tracker: &mut Tracker<'i, R>,
    i: usize,
) -> Option<(Position<'i>, T)> {
    if i > 0 {
        while let Some((next, _trivia)) = R::Trivia::try_parse_with_partial(input, stack, tracker) {
            input = next;
        }
    }
    let (next, matched) = T::try_parse_with_partial(input, stack, tracker)?;
    input = next;
    Some((input, matched))
}
