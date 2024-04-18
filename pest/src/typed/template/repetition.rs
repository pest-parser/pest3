//! Predefined tree nodes generics.
//! The generator may use this for convenience.
//! Normally you don't need to reference this module by yourself.

use super::{restore_on_none, RuleType};
use crate::{
    typed::{
        tracker::Tracker, wrapper::Bound as BoundWrapper, NeverFailedTypedNode, PairContainer,
        TypedNode,
    },
    Position, Span, Stack,
};
use alloc::vec::Vec;
use core::{fmt::Debug, hash::Hash, marker::PhantomData, usize};

struct Phantom<T>(PhantomData<T>);

impl<T> Default for Phantom<T> {
    fn default() -> Self {
        Self(PhantomData)
    }
}
impl<T> Clone for Phantom<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T> Copy for Phantom<T> {}
impl<T> Hash for Phantom<T> {
    fn hash<H: std::hash::Hasher>(&self, _: &mut H) {}
}
impl<T> PartialEq for Phantom<T> {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}
impl<T> Eq for Phantom<T> {}

/// Repeatably match `T` at least `MIN` times and at most `MAX` times.
#[derive(Hash, PartialEq, Eq)]
pub struct RepMinMax<T, TRIVIA, const MIN: usize, const MAX: usize> {
    /// Skipped and Matched expressions.
    pub content: Vec<T>,
    __trivia: Phantom<TRIVIA>,
}

impl<T, TRIVIA, const MAX: usize> Default for RepMinMax<T, TRIVIA, 0, MAX> {
    fn default() -> Self {
        Self {
            content: Vec::new(),
            __trivia: Phantom::default(),
        }
    }
}

impl<
        'i,
        R: RuleType,
        T: TypedNode<'i, R> + Debug + Clone + PartialEq + Default,
        TRIVIA: TypedNode<'i, R> + PartialEq,
        const MAX: usize,
    > NeverFailedTypedNode<'i, R> for RepMinMax<T, TRIVIA, 0, MAX>
{
    #[inline]
    fn parse_with_partial(
        mut input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
    ) -> (Position<'i>, Self) {
        let mut vec = Vec::new();

        let mut tracker = Tracker::new(input);

        for i in 0..MAX {
            match restore_on_none(stack, |stack| {
                try_parse_unit::<R, T, TRIVIA>(input, stack, &mut tracker, i)
            }) {
                Some((next, matched)) => {
                    input = next;
                    vec.push(matched);
                }
                None => {
                    break;
                }
            }
        }

        (
            input,
            Self {
                content: vec,
                __trivia: Phantom::default(),
            },
        )
    }
}
impl<
        'i,
        R: RuleType,
        T: TypedNode<'i, R>,
        TRIVIA: TypedNode<'i, R>,
        const MIN: usize,
        const MAX: usize,
    > TypedNode<'i, R> for RepMinMax<T, TRIVIA, MIN, MAX>
{
    #[inline]
    fn try_parse_with_partial(
        mut input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
        tracker: &mut Tracker<'i, R>,
    ) -> Option<(Position<'i>, Self)> {
        let mut vec = Vec::new();

        for i in 0..MAX {
            match restore_on_none(stack, |stack| {
                try_parse_unit::<R, T, TRIVIA>(input, stack, tracker, i)
            }) {
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

        Some((
            input,
            Self {
                content: vec,
                __trivia: Phantom::default(),
            },
        ))
    }
    #[inline]
    fn check_with_partial(
        mut input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
        tracker: &mut Tracker<'i, R>,
    ) -> Option<Position<'i>> {
        for i in 0..MAX {
            match restore_on_none(stack, |stack| {
                check_unit::<R, T, TRIVIA>(input, stack, tracker, i)
            }) {
                Some(next) => {
                    input = next;
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

        Some(input)
    }
}
impl<T, TRIVIA, const MIN: usize, const MAX: usize> IntoIterator
    for RepMinMax<T, TRIVIA, MIN, MAX>
{
    type Item = T;

    type IntoIter = <Vec<T> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.content.into_iter()
    }
}
impl<T, TRIVIA, const MIN: usize, const MAX: usize> RepMinMax<T, TRIVIA, MIN, MAX> {
    /// Returns an iterator over all smatched expressions by reference.
    pub fn iter(&self) -> alloc::slice::Iter<'_, T> {
        self.content.iter()
    }
}
impl<T, TRIVIA, const MIN: usize, const MAX: usize> BoundWrapper
    for RepMinMax<T, TRIVIA, MIN, MAX>
{
    const MIN: usize = MIN;
    const MAX: usize = MAX;
}
impl<T: Debug, TRIVIA, const MIN: usize, const MAX: usize> Debug
    for RepMinMax<T, TRIVIA, MIN, MAX>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("RepMinMax").field(&self.content).finish()
    }
}
impl<T: Clone, TRIVIA, const MIN: usize, const MAX: usize> Clone
    for RepMinMax<T, TRIVIA, MIN, MAX>
{
    fn clone(&self) -> Self {
        let content = self.content.clone();
        let __trivia = self.__trivia;
        Self { content, __trivia }
    }
}
impl<R: RuleType, T: PairContainer<R>, TRIVIA, const MIN: usize, const MAX: usize> PairContainer<R>
    for RepMinMax<T, TRIVIA, MIN, MAX>
{
    fn for_each_child_pair(&self, f: &mut impl FnMut(crate::token::Pair<R>)) {
        for item in &self.content {
            item.for_self_or_for_each_child_pair(f)
        }
    }
}

/// Repeat at least one times.
pub type RepMin<T, TRIVIA, const MIN: usize> = RepMinMax<T, TRIVIA, MIN, { usize::MAX }>;
/// Repeat at least one times.
pub type RepMax<T, TRIVIA, const MAX: usize> = RepMinMax<T, TRIVIA, 0, MAX>;
/// Repeat arbitrary times.
pub type Rep<T, TRIVIA> = RepMin<T, TRIVIA, 0>;
/// Repeat at least one times.
pub type RepOnce<T, TRIVIA> = RepMin<T, TRIVIA, 1>;

#[inline]
fn try_parse_unit<'i, R: RuleType, T: TypedNode<'i, R>, TRIVIA: TypedNode<'i, R>>(
    mut input: Position<'i>,
    stack: &mut Stack<Span<'i>>,
    tracker: &mut Tracker<'i, R>,
    i: usize,
) -> Option<(Position<'i>, T)> {
    if i > 0 {
        input = TRIVIA::check_with_partial(input, stack, tracker)?;
    }
    let (next, matched) = T::try_parse_with_partial(input, stack, tracker)?;
    input = next;
    Some((input, matched))
}

#[inline]
fn check_unit<'i, R: RuleType, T: TypedNode<'i, R>, TRIVIA: TypedNode<'i, R>>(
    mut input: Position<'i>,
    stack: &mut Stack<Span<'i>>,
    tracker: &mut Tracker<'i, R>,
    i: usize,
) -> Option<Position<'i>> {
    if i > 0 {
        input = TRIVIA::check_with_partial(input, stack, tracker)?;
    }
    let next = T::check_with_partial(input, stack, tracker)?;
    input = next;
    Some(input)
}
