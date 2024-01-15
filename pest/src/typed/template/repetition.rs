// pest-typed. A statically typed version of pest.
// Copyright (c) 2023 黄博奕
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! Predefined tree nodes generics.
//! The generator may use this for convenience.
//! Normally you don't need to reference this module by yourself.

use core::ops::{Deref, DerefMut};

use crate::{
    predefined_node::{restore_on_err, Skipped},
    tracker::Tracker,
    wrapper::BoundWrapper,
    NeverFailedTypedNode, Position, RuleType, Span, Stack, TypedNode,
};
use alloc::vec::Vec;

type Iter<'n, T, IGNORED, const SKIP: usize> = core::iter::Map<
    alloc::slice::Iter<'n, Skipped<T, IGNORED, SKIP>>,
    fn(&'n Skipped<T, IGNORED, SKIP>) -> &'n T,
>;
type IntoIter<T, IGNORED, const SKIP: usize> = core::iter::Map<
    alloc::vec::IntoIter<Skipped<T, IGNORED, SKIP>>,
    fn(Skipped<T, IGNORED, SKIP>) -> T,
>;

/// Repeatably match `T` at least `MIN` times.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct AtomicRep<T> {
    /// Skipped and Matched expressions.
    pub content: Vec<T>,
}
impl<T> Default for AtomicRep<T> {
    fn default() -> Self {
        let content = Vec::new();
        Self { content }
    }
}
impl<'i, R: RuleType, T: TypedNode<'i, R>> NeverFailedTypedNode<'i, R> for AtomicRep<T> {
    fn parse_with(mut input: Position<'i>, stack: &mut Stack<Span<'i>>) -> (Position<'i>, Self) {
        let mut vec = Vec::new();
        let mut tracker = Tracker::new(input);

        for _ in 0usize.. {
            match restore_on_err(stack, |stack| T::try_parse_with(input, stack, &mut tracker)) {
                Ok((next, matched)) => {
                    input = next;
                    vec.push(matched);
                }
                Err(_) => break,
            }
        }
        (input, Self { content: vec })
    }
}
impl<'i, R: RuleType, T: TypedNode<'i, R>> TypedNode<'i, R> for AtomicRep<T> {
    #[inline]
    fn try_parse_with(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
        _tracker: &mut Tracker<'i, R>,
    ) -> Result<(Position<'i>, Self), ()> {
        Ok(Self::parse_with(input, stack))
    }
}
impl<T> Deref for AtomicRep<T> {
    type Target = Vec<T>;
    fn deref(&self) -> &Self::Target {
        &self.content
    }
}
impl<T> DerefMut for AtomicRep<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.content
    }
}
impl<T: Clone + PartialEq> BoundWrapper for AtomicRep<T> {
    const MIN: usize = 0;
    const MAX: usize = usize::MAX;
}

/// Repeatably match `T` at least `MIN` times.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct RepMin<T, const MIN: usize> {
    /// Skipped and Matched expressions.
    pub content: Vec<T>,
}
impl<
        'i,
        R: RuleType,
        T: TypedNode<'i, R>,
        Skip: NeverFailedTypedNode<'i, R>,
        const SKIP: usize,
    > NeverFailedTypedNode<'i, R> for RepMin<Skipped<T, Skip, SKIP>, 0>
{
    fn parse_with(mut input: Position<'i>, stack: &mut Stack<Span<'i>>) -> (Position<'i>, Self) {
        let mut vec = Vec::new();
        let mut tracker = Tracker::new(input);

        for i in 0usize.. {
            match restore_on_err(stack, |stack| try_parse_unit(input, stack, &mut tracker, i)) {
                Ok((next, matched)) => {
                    input = next;
                    vec.push(matched);
                }
                Err(_) => break,
            }
        }
        (input, Self { content: vec })
    }
}
impl<T> Default for RepMin<T, 0> {
    fn default() -> Self {
        let content = Vec::new();
        Self { content }
    }
}
impl<
        'i,
        R: RuleType,
        T: TypedNode<'i, R>,
        Skip: NeverFailedTypedNode<'i, R>,
        const SKIP: usize,
        const MIN: usize,
    > TypedNode<'i, R> for RepMin<Skipped<T, Skip, SKIP>, MIN>
{
    #[inline]
    fn try_parse_with(
        mut input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
        tracker: &mut Tracker<'i, R>,
    ) -> Result<(Position<'i>, Self), ()> {
        let mut vec = Vec::new();

        for i in 0usize.. {
            match restore_on_err(stack, |stack| try_parse_unit(input, stack, tracker, i)) {
                Ok((next, matched)) => {
                    input = next;
                    vec.push(matched);
                }
                Err(err) => {
                    if i < MIN {
                        return Err(err);
                    } else {
                        break;
                    }
                }
            }
        }

        Ok((input, Self { content: vec }))
    }
}
impl<T, IGNORED, const SKIP: usize, const MIN: usize> RepMin<Skipped<T, IGNORED, SKIP>, MIN> {
    /// Returns an iterator over all matched expressions by reference.
    #[allow(clippy::needless_lifetimes)]
    pub fn iter_matched<'n>(&'n self) -> Iter<'n, T, IGNORED, SKIP> {
        self.content.iter().map(|s| &s.matched)
    }
    /// Returns an iterator over all matched expressions by value.
    pub fn into_iter_matched(self) -> IntoIter<T, IGNORED, SKIP> {
        self.content.into_iter().map(|s| s.matched)
    }
}
impl<T, const MIN: usize> RepMin<T, MIN> {
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
impl<T: Clone + PartialEq, const MIN: usize> BoundWrapper for RepMin<T, MIN> {
    const MIN: usize = MIN;
    const MAX: usize = usize::MAX;
}

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
        T: TypedNode<'i, R>,
        Skip: NeverFailedTypedNode<'i, R>,
        const SKIP: usize,
        const MAX: usize,
    > NeverFailedTypedNode<'i, R> for RepMinMax<Skipped<T, Skip, SKIP>, 0, MAX>
{
    #[inline]
    fn parse_with(mut input: Position<'i>, stack: &mut Stack<Span<'i>>) -> (Position<'i>, Self) {
        let mut vec = Vec::new();

        let mut tracker = Tracker::new(input);

        for i in 0..MAX {
            match restore_on_err(stack, |stack| try_parse_unit(input, stack, &mut tracker, i)) {
                Ok((next, matched)) => {
                    input = next;
                    vec.push(matched);
                }
                Err(_) => {
                    break;
                }
            }
        }

        (input, Self { content: vec })
    }
}
impl<
        'i,
        R: RuleType,
        T: TypedNode<'i, R>,
        Skip: NeverFailedTypedNode<'i, R>,
        const SKIP: usize,
        const MIN: usize,
        const MAX: usize,
    > TypedNode<'i, R> for RepMinMax<Skipped<T, Skip, SKIP>, MIN, MAX>
{
    #[inline]
    fn try_parse_with(
        mut input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
        tracker: &mut Tracker<'i, R>,
    ) -> Result<(Position<'i>, Self), ()> {
        let mut vec = Vec::new();

        for i in 0..MAX {
            match restore_on_err(stack, |stack| try_parse_unit(input, stack, tracker, i)) {
                Ok((next, matched)) => {
                    input = next;
                    vec.push(matched);
                }
                Err(err) => {
                    if i < MIN {
                        return Err(err);
                    } else {
                        break;
                    }
                }
            }
        }

        Ok((input, Self { content: vec }))
    }
}
impl<T, IGNORED, const SKIP: usize, const MIN: usize, const MAX: usize>
    RepMinMax<Skipped<T, IGNORED, SKIP>, MIN, MAX>
{
    /// Returns an iterator over all matched expressions by reference.
    #[allow(clippy::needless_lifetimes)]
    pub fn iter_matched<'n>(&'n self) -> Iter<'n, T, IGNORED, SKIP> {
        self.content.iter().map(|s| &s.matched)
    }
    /// Returns an iterator over all matched expressions by value.
    pub fn into_iter_matched(self) -> IntoIter<T, IGNORED, SKIP> {
        self.content.into_iter().map(|s| s.matched)
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

/// Repeat arbitrary times.
pub type Rep<T, IGNORED, const SKIP: usize> = RepMin<Skipped<T, IGNORED, SKIP>, 0>;
/// Repeat at least one times.
pub type RepOnce<T, IGNORED, const SKIP: usize> = RepMin<Skipped<T, IGNORED, SKIP>, 1>;

fn try_parse_unit<
    'i,
    R: RuleType,
    T: TypedNode<'i, R>,
    Skip: NeverFailedTypedNode<'i, R>,
    const SKIP: usize,
>(
    mut input: Position<'i>,
    stack: &mut Stack<Span<'i>>,
    tracker: &mut Tracker<'i, R>,
    i: usize,
) -> Result<(Position<'i>, Skipped<T, Skip, SKIP>), ()> {
    let skipped = core::array::from_fn(|_| {
        if i == 0 {
            Skip::default()
        } else {
            let (next, skipped) = Skip::parse_with(input, stack);
            input = next;
            skipped
        }
    });
    let (next, matched) = T::try_parse_with(input, stack, tracker)?;
    input = next;
    let res = Skipped { skipped, matched };
    Ok((input, res))
}
