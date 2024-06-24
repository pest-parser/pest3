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

fn any(input: &str) {
    type R = RepMinMax<ANY, Empty, TOTAL, TOTAL>;
    <R as TypedNode<'_, Rule>>::try_parse(input).unwrap();
}

fn choices_16(input: &str) {
    type R = RepMinMax<
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
    <R as TypedNode<'_, Rule>>::try_parse(input).unwrap();
}

fn sequence_16(input: &str) {
    type R = RepMinMax<
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
    <R as TypedNode<'_, Rule>>::try_parse(input).unwrap();
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct S;
impl wrapper::String for S {
    const CONTENT: &'static str = FRAG;
}

fn strings(input: &str) {
    type R = RepMinMax<Str<S>, Empty, TIMES, TIMES>;
    <R as TypedNode<'_, Rule>>::try_parse(input).unwrap();
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct I;
impl wrapper::String for I {
    const CONTENT: &'static str = "0123456789ABCDEF";
}

fn insensitive_strings(input: &str) {
    type R<'i> = RepMinMax<Insens<'i, I>, Empty, TIMES, TIMES>;
    <R as TypedNode<'_, Rule>>::try_parse(input).unwrap();
}

fn range(input: &str) {
    type R = RepMinMax<CharRange<'0', 'f'>, Empty, TOTAL, TOTAL>;
    <R as TypedNode<'_, Rule>>::try_parse(input).unwrap();
}

fn range2(input: &str) {
    type R = RepMinMax<Choice2<CharRange<'0', '9'>, CharRange<'a', 'f'>>, Empty, TOTAL, TOTAL>;
    <R as TypedNode<'_, Rule>>::try_parse(input).unwrap();
}

fn skip_fragments(input: &str) {
    type R<'i> = RepMinMax<SkipChar<'i, LEN>, Empty, TIMES, TIMES>;
    <R as TypedNode<'_, Rule>>::try_parse(input).unwrap();
}

fn skip_all(input: &str) {
    type R<'i> = SkipChar<'i, TOTAL>;
    <R as TypedNode<'_, Rule>>::try_parse(input).unwrap();
}

fn push(input: &str) {
    type R<'i> = RepMinMax<PUSH<Str<S>>, Empty, TIMES, TIMES>;
    <R as TypedNode<'_, Rule>>::try_parse(input).unwrap();
}

fn push_peek(input: &str) {
    type R<'i> = Sequence2<
        PUSH<Str<S>>,
        Empty,
        RepMinMax<PEEK<'i>, Empty, { TIMES - 1 }, { TIMES - 1 }>,
        Empty,
    >;
    <R as TypedNode<'_, Rule>>::try_parse(input).unwrap();
}

fn push_peek_all(input: &str) {
    type R<'i> = Sequence2<
        PUSH<Str<S>>,
        Empty,
        RepMinMax<PEEK_ALL<'i>, Empty, { TIMES - 1 }, { TIMES - 1 }>,
        Empty,
    >;
    <R as TypedNode<'_, Rule>>::try_parse(input).unwrap();
}

fn benchmark(b: &mut Criterion) {
    let input = FRAG.repeat(TIMES);
    macro_rules! test_series {
        ($($func:expr),*) => {
            b
            $(
                .bench_function(stringify!($func), |b| b.iter(|| $func(&input)))
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
