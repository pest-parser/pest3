//! This benchmark is designed to test the performance of the `TypedNode` trait.
#![allow(non_camel_case_types)]

use std::time::Duration;

use criterion::{criterion_group, criterion_main, Criterion};
use pest3::{
    choice::{Choice16, Choice2},
    sequence::{Sequence16, Sequence2},
    typed::{
        template::{
            Char, CharRange, Empty, Insens, RepMinMax, SkipChar, Str, ANY, PEEK, PEEK_ALL, PUSH,
        },
        wrapper, RuleType, TypedNode,
    },
};

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
enum Rule {
    EOI,
}

impl RuleType for Rule {
    const EOI: Self = Self::EOI;
}

const TIMES: usize = 0x10000usize;
const FRAG: &'static str = "0123456789abcdef";
const LEN: usize = 16;
const TOTAL: usize = TIMES * LEN;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct S;
impl wrapper::String for S {
    const CONTENT: &'static str = FRAG;
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct I;
impl wrapper::String for I {
    const CONTENT: &'static str = "0123456789ABCDEF";
}

fn benchmark(b: &mut Criterion) {
    let input = FRAG.repeat(TIMES);

    mod types {
        use super::*;
        pub type any = RepMinMax<ANY, Empty, TOTAL, TOTAL>;
        pub type choices_16 = RepMinMax<
            Choice16<
                Char<'0'>,
                Char<'1'>,
                Char<'2'>,
                Char<'3'>,
                Char<'4'>,
                Char<'5'>,
                Char<'6'>,
                Char<'7'>,
                Char<'8'>,
                Char<'9'>,
                Char<'a'>,
                Char<'b'>,
                Char<'c'>,
                Char<'d'>,
                Char<'e'>,
                Char<'f'>,
            >,
            Empty,
            TOTAL,
            TOTAL,
        >;
        pub type sequence_16 = RepMinMax<
            Sequence16<
                Char<'0'>,
                Empty,
                Char<'1'>,
                Empty,
                Char<'2'>,
                Empty,
                Char<'3'>,
                Empty,
                Char<'4'>,
                Empty,
                Char<'5'>,
                Empty,
                Char<'6'>,
                Empty,
                Char<'7'>,
                Empty,
                Char<'8'>,
                Empty,
                Char<'9'>,
                Empty,
                Char<'a'>,
                Empty,
                Char<'b'>,
                Empty,
                Char<'c'>,
                Empty,
                Char<'d'>,
                Empty,
                Char<'e'>,
                Empty,
                Char<'f'>,
                Empty,
            >,
            Empty,
            TIMES,
            TIMES,
        >;
        pub type strings = RepMinMax<Str<S>, Empty, TIMES, TIMES>;
        pub type insensitive_strings<'i> = RepMinMax<Insens<'i, I>, Empty, TIMES, TIMES>;
        pub type range = RepMinMax<CharRange<'0', 'f'>, Empty, TOTAL, TOTAL>;
        pub type range2 =
            RepMinMax<Choice2<CharRange<'0', '9'>, CharRange<'a', 'f'>>, Empty, TOTAL, TOTAL>;
        pub type skip_fragments<'i> = RepMinMax<SkipChar<'i, LEN>, Empty, TIMES, TIMES>;
        pub type skip_all<'i> = SkipChar<'i, TOTAL>;
        pub type push = RepMinMax<PUSH<Str<S>>, Empty, TIMES, TIMES>;
        pub type push_peek<'i> = Sequence2<
            PUSH<Str<S>>,
            Empty,
            RepMinMax<PEEK<'i>, Empty, { TIMES - 1 }, { TIMES - 1 }>,
            Empty,
        >;
        pub type push_peek_all<'i> = Sequence2<
            PUSH<Str<S>>,
            Empty,
            RepMinMax<PEEK_ALL<'i>, Empty, { TIMES - 1 }, { TIMES - 1 }>,
            Empty,
        >;
    }
    macro_rules! test_series {
        ($($name:ident),*) => {
            b
            $(
                .bench_function(stringify!(parse - $name), |b| b.iter(|| {
                    <types::$name as TypedNode<'_, Rule>>::try_parse(&input).unwrap()
                }))
                .bench_function(stringify!(check - $name), |b| b.iter(|| {
                    <types::$name as TypedNode<'_, Rule>>::check(&input).unwrap()
                }))
            )*
        };
    }
    test_series!(
        any,
        choices_16,
        sequence_16,
        range,
        range2,
        strings,
        insensitive_strings,
        skip_fragments,
        skip_all,
        push,
        push_peek,
        push_peek_all
    );
}

criterion_group!(
    name = benchmarks;
    config = Criterion::default().measurement_time(Duration::from_secs(10));
    targets = benchmark
);
criterion_main!(benchmarks);
