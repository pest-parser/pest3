//! Copied from [pest-typed/formatter.rs].
//!
//! [pest-typed/formatter.rs]: https://github.com/TheVeryDarkness/pest-typed/blob/0.12.1/main/src/formatter.rs

use crate::{Position, Span};
use alloc::{format, string::String, vec::Vec};
use core::{fmt, marker::PhantomData};
use unicode_width::UnicodeWidthStr;

struct Pos {
    line: usize,
    col: usize,
}

/// Visualize white spaces and control characters.
///
/// ␀␁␂␃␄␅␆␇␈␉␊␋␌␍␎␏
/// ␐␑␒␓␔␕␖␗␘␙␚␛␜␝␞␟
/// ␠
/// ␡
/// \r ␍
/// \n ␊
fn visualize_ws_and_cntrl(line: &str) -> String {
    line.chars()
        .map(|c| match c {
            '\u{0}' => '␀',
            '\u{1}' => '␁',
            '\u{2}' => '␂',
            '\u{3}' => '␃',
            '\u{4}' => '␄',
            '\u{5}' => '␅',
            '\u{6}' => '␆',
            '\u{7}' => '␇',
            '\u{8}' => '␈',
            '\u{9}' => '␉',
            '\u{a}' => '␊',
            '\u{b}' => '␋',
            '\u{c}' => '␌',
            '\u{d}' => '␍',
            '\u{e}' => '␎',
            '\u{f}' => '␏',
            '\u{10}' => '␐',
            '\u{11}' => '␑',
            '\u{12}' => '␒',
            '\u{13}' => '␓',
            '\u{14}' => '␔',
            '\u{15}' => '␕',
            '\u{16}' => '␖',
            '\u{17}' => '␗',
            '\u{18}' => '␘',
            '\u{19}' => '␙',
            '\u{1a}' => '␚',
            '\u{1b}' => '␛',
            '\u{1c}' => '␜',
            '\u{1d}' => '␝',
            '\u{1e}' => '␞',
            '\u{1f}' => '␟',
            /*
            '\u{20}' => '␠',
            */
            '\u{7f}' => '␡',
            _ => c,
        })
        .collect()
}

struct Partition2<'i> {
    line: usize,
    former: String,
    middle: String,
    latter: String,
    _p: PhantomData<&'i str>,
}
impl<'i> Partition2<'i> {
    fn new<'s: 'i>(line: usize, s: &'s str, col_start: usize, col_end: usize) -> Self {
        let (former, latter) = s.split_at(col_end);
        let (former, middle) = former.split_at(col_start);
        let former = visualize_ws_and_cntrl(former);
        let middle = visualize_ws_and_cntrl(middle);
        let latter = visualize_ws_and_cntrl(latter);
        let _p = PhantomData;
        Self {
            line,
            former,
            middle,
            latter,
            _p,
        }
    }
}

struct Partition<'i> {
    line: usize,
    former: String,
    latter: String,
    _p: PhantomData<&'i str>,
}
impl<'i> Partition<'i> {
    fn new<'s: 'i>(line: usize, s: &'s str, col: usize) -> Self {
        let (former, latter) = s.split_at(col);
        let former = visualize_ws_and_cntrl(former);
        let latter = visualize_ws_and_cntrl(latter);
        let _p = PhantomData;
        Self {
            line,
            former,
            latter,
            _p,
        }
    }
}

/// Formatter options for [Span](crate::Span).
pub struct FormatOption<SpanFormatter, MarkerFormatter, NumberFormatter> {
    /// Formatter for characters in span.
    pub span_formatter: SpanFormatter,
    /// Formatter for position marker.
    pub marker_formatter: MarkerFormatter,
    /// Formatter for line numbers and column numbers of start and end.
    pub number_formatter: NumberFormatter,
}

type FmtPtr<Writer> = fn(&str, &mut Writer) -> fmt::Result;
impl<Writer: fmt::Write> Default for FormatOption<FmtPtr<Writer>, FmtPtr<Writer>, FmtPtr<Writer>> {
    fn default() -> Self {
        Self {
            span_formatter: |s, f| write!(f, "{s}"),
            marker_formatter: |m, f| write!(f, "{m}"),
            number_formatter: |n, f| write!(f, "{n}"),
        }
    }
}

impl<SF, MF, NF> FormatOption<SF, MF, NF> {
    /// Create option with given functions.
    pub fn new<Writer>(span_formatter: SF, marker_formatter: MF, number_formatter: NF) -> Self
    where
        Writer: fmt::Write,
        SF: FnMut(&str, &mut Writer) -> fmt::Result,
        MF: FnMut(&str, &mut Writer) -> fmt::Result,
        NF: FnMut(&str, &mut Writer) -> fmt::Result,
    {
        Self {
            span_formatter,
            marker_formatter,
            number_formatter,
        }
    }
    fn ceil_log10(num: usize) -> usize {
        let mut digit = 1usize;
        let mut i = num;
        while i >= 10 {
            digit += 1;
            i /= 10;
        }
        digit
    }
    fn display_snippet_single_pos<Writer>(
        mut self,
        f: &mut Writer,
        index_digit: usize,
        line: Partition<'_>,
    ) -> fmt::Result
    where
        Writer: fmt::Write,
        SF: FnMut(&str, &mut Writer) -> fmt::Result,
        MF: FnMut(&str, &mut Writer) -> fmt::Result,
        NF: FnMut(&str, &mut Writer) -> fmt::Result,
    {
        let spacing = " ".repeat(index_digit);
        write!(f, "{} ", spacing)?;
        (self.number_formatter)("|", f)?;
        writeln!(f)?;

        let number = format!("{:w$}", line.line + 1, w = index_digit);
        (self.number_formatter)(&number, f)?;
        write!(f, " ")?;
        (self.number_formatter)("|", f)?;
        write!(f, " {}{}", line.former, line.latter)?;
        writeln!(f)?;

        write!(f, "{} ", spacing)?;
        (self.number_formatter)("|", f)?;
        write!(
            f,
            " {}",
            " ".repeat(UnicodeWidthStr::width_cjk(line.former.as_str())),
        )?;
        (self.marker_formatter)("^", f)?;
        writeln!(f)?;

        Ok(())
    }
    fn display_snippet_single_line<Writer>(
        mut self,
        f: &mut Writer,
        index_digit: usize,
        line: Partition2<'_>,
    ) -> fmt::Result
    where
        Writer: fmt::Write,
        SF: FnMut(&str, &mut Writer) -> fmt::Result,
        MF: FnMut(&str, &mut Writer) -> fmt::Result,
        NF: FnMut(&str, &mut Writer) -> fmt::Result,
    {
        let spacing = " ".repeat(index_digit);
        write!(f, "{} ", spacing)?;
        (self.number_formatter)("|", f)?;
        writeln!(f)?;

        let number = format!("{:w$}", line.line + 1, w = index_digit);
        (self.number_formatter)(&number, f)?;
        write!(f, " ")?;
        (self.number_formatter)("|", f)?;
        write!(f, " {}", line.former)?;
        (self.span_formatter)(&line.middle, f)?;
        write!(f, "{}", line.latter)?;
        writeln!(f)?;

        write!(f, "{} ", spacing)?;
        (self.number_formatter)("|", f)?;
        write!(
            f,
            " {}",
            " ".repeat(UnicodeWidthStr::width_cjk(line.former.as_str())),
        )?;
        (self.marker_formatter)(
            &"^".repeat(UnicodeWidthStr::width_cjk(line.middle.as_str())),
            f,
        )?;
        writeln!(f)?;

        Ok(())
    }
    fn display_full_covered_snippet<Writer>(
        &mut self,
        f: &mut Writer,
        index_digit: usize,
        line: usize,
        line_content: &str,
    ) -> fmt::Result
    where
        Writer: fmt::Write,
        SF: FnMut(&str, &mut Writer) -> fmt::Result,
        MF: FnMut(&str, &mut Writer) -> fmt::Result,
        NF: FnMut(&str, &mut Writer) -> fmt::Result,
    {
        let number = format!("{:w$}", line, w = index_digit);
        (self.number_formatter)(&number, f)?;
        write!(f, " ")?;
        (self.number_formatter)("|", f)?;
        write!(f, " ")?;
        (self.span_formatter)(line_content, f)?;
        writeln!(f)?;
        Ok(())
    }
    fn display_snippet_multi_line<Writer>(
        mut self,
        f: &mut Writer,
        index_digit: usize,
        start: Partition<'_>,
        end: Partition<'_>,
        // 100
        // 101
        // 111
        // 101
        inner: (Option<&str>, Option<&str>, bool, Option<&str>),
    ) -> fmt::Result
    where
        Writer: fmt::Write,
        SF: FnMut(&str, &mut Writer) -> fmt::Result,
        MF: FnMut(&str, &mut Writer) -> fmt::Result,
        NF: FnMut(&str, &mut Writer) -> fmt::Result,
    {
        let spacing = " ".repeat(index_digit);
        write!(f, "{} ", spacing)?;
        (self.number_formatter)("|", f)?;
        write!(
            f,
            " {}",
            " ".repeat(UnicodeWidthStr::width_cjk(start.former.as_str()))
        )?;
        (self.marker_formatter)("v", f)?;
        writeln!(f)?;

        let number = format!("{:w$}", start.line + 1, w = index_digit);
        (self.number_formatter)(&number, f)?;
        write!(f, " ")?;
        (self.number_formatter)("|", f)?;
        write!(f, " {}", start.former)?;
        (self.span_formatter)(&start.latter, f)?;
        writeln!(f)?;

        if let Some(line) = inner.0 {
            self.display_full_covered_snippet(f, index_digit, start.line + 2, line)?;
        }

        if let Some(line) = inner.1 {
            self.display_full_covered_snippet(f, index_digit, start.line + 3, line)?;
        } else if inner.2 {
            write!(f, "{} ", spacing)?;
            (self.number_formatter)("|", f)?;
            writeln!(f, " ...")?;
        }

        if let Some(line) = inner.3 {
            self.display_full_covered_snippet(f, index_digit, end.line, line)?;
        }

        let number = format!("{:w$}", end.line + 1, w = index_digit);
        (self.number_formatter)(&number, f)?;
        write!(f, " ")?;
        (self.number_formatter)("|", f)?;
        write!(f, " ")?;
        (self.span_formatter)(&end.former, f)?;
        writeln!(f, "{}", end.latter)?;

        write!(f, "{} ", spacing)?;
        (self.number_formatter)("|", f)?;
        write!(
            f,
            " {}",
            " ".repeat(UnicodeWidthStr::width_cjk(end.former.as_str()).saturating_sub(1))
        )?;
        (self.marker_formatter)("^", f)?;
        writeln!(f)?;

        Ok(())
    }
    #[allow(clippy::needless_lifetimes)]
    pub(crate) fn display_span<'i, Writer>(self, span: &Span<'i>, f: &mut Writer) -> fmt::Result
    where
        Writer: fmt::Write,
        SF: FnMut(&str, &mut Writer) -> fmt::Result,
        MF: FnMut(&str, &mut Writer) -> fmt::Result,
        NF: FnMut(&str, &mut Writer) -> fmt::Result,
    {
        let mut start = None;
        let mut end = None;
        let mut pos = 0usize;
        let input = Span::new(span.get_input(), 0, span.get_input().len()).unwrap();
        let mut iter = input.lines().enumerate().peekable();
        while let Some((index, line)) = iter.peek() {
            if pos + line.len() >= span.start() {
                start = Some(Pos {
                    line: *index,
                    col: span.start() - pos,
                });
                break;
            }
            pos += line.len();
            iter.next();
        }
        for (index, line) in iter {
            if pos + line.len() >= span.end() {
                end = Some(Pos {
                    line: index,
                    col: span.end() - pos,
                });
                break;
            }
            pos += line.len();
        }
        let start = start.unwrap();
        let end = end.unwrap();
        let mut lines = input
            .lines()
            .skip(start.line)
            .take(end.line - start.line + 1)
            .peekable();
        let index_digit = Self::ceil_log10(end.line + 1);
        if start.line == end.line {
            let cur_line = lines.next().unwrap();
            let line = Partition2::new(start.line, cur_line, start.col, end.col);
            self.display_snippet_single_line(f, index_digit, line)?;
        } else {
            let lines: Vec<_> = lines.collect();
            let start_line = lines.first().unwrap();
            let end_line = lines.last().unwrap();
            let start = Partition::new(start.line, start_line, start.col);
            let end = Partition::new(end.line, end_line, end.col);
            let inner_first = if lines.len() >= 3 {
                Some(visualize_ws_and_cntrl(lines[1]))
            } else {
                None
            };
            let inner_mid = match lines.len() {
                6.. => (None, true),
                5 => (Some(visualize_ws_and_cntrl(lines[2])), false),
                _ => (None, false),
            };
            let inner_last = if lines.len() >= 4 {
                Some(visualize_ws_and_cntrl(lines[lines.len() - 2]))
            } else {
                None
            };
            let inner = (
                inner_first.as_deref(),
                inner_mid.0.as_deref(),
                inner_mid.1,
                inner_last.as_deref(),
            );
            self.display_snippet_multi_line(f, index_digit, start, end, inner)?;
        }
        Ok(())
    }
    #[allow(clippy::needless_lifetimes)]
    pub(crate) fn display_position<'i, Writer>(
        self,
        position: &Position<'i>,
        f: &mut Writer,
    ) -> fmt::Result
    where
        Writer: fmt::Write,
        SF: FnMut(&str, &mut Writer) -> fmt::Result,
        MF: FnMut(&str, &mut Writer) -> fmt::Result,
        NF: FnMut(&str, &mut Writer) -> fmt::Result,
    {
        let mut pos = 0usize;
        let input = Span::new(position.input, 0, position.input.len()).unwrap();
        let mut iter = input.lines().enumerate().peekable();
        while let Some((index, line)) = iter.peek() {
            if pos + line.len() > position.pos() {
                let l = *index;
                let c = position.pos() - pos;
                let index_digit = Self::ceil_log10(l + 1);
                let line = Partition::new(l, line, c);
                self.display_snippet_single_pos(f, index_digit, line)?;
                break;
            }
            pos += line.len();
            iter.next();
        }
        Ok(())
    }
}

#[cfg(test)]
mod control {
    #[test]
    fn visualize_ws_and_cntrl() {
        let s = super::visualize_ws_and_cntrl(
            "\
            \u{00}\u{01}\u{02}\u{03}\u{04}\u{05}\u{06}\u{07}\u{08}\u{09}\u{0a}\u{0b}\u{0c}\u{0d}\u{0e}\u{0f}\
            \u{10}\u{11}\u{12}\u{13}\u{14}\u{15}\u{16}\u{17}\u{18}\u{19}\u{1a}\u{1b}\u{1c}\u{1d}\u{1e}\u{1f}\
            \u{20}\u{7f}",
        );
        assert_eq!(s, "␀␁␂␃␄␅␆␇␈␉␊␋␌␍␎␏␐␑␒␓␔␕␖␗␘␙␚␛␜␝␞␟ ␡");
    }
}

#[cfg(test)]
mod position {
    use super::*;
    use alloc::string::ToString;

    #[test]
    fn first_pos() {
        let msg = Position::new("123\n456\n789\n", 4).unwrap().to_string();
        assert_eq!(
            msg,
            "  \
  |
2 | 456␊
  | ^
"
        );
    }

    #[test]
    fn mid_pos() {
        let msg = Position::new("123\n456\n789\n", 5).unwrap().to_string();
        assert_eq!(
            msg,
            "  \
  |
2 | 456␊
  |  ^
"
        );
    }

    #[test]
    fn last_pos() {
        let msg = Position::new("123\n456\n789\n", 8).unwrap().to_string();
        assert_eq!(
            msg,
            "  \
  |
3 | 789␊
  | ^
"
        );
    }
}

#[cfg(test)]
mod span {
    use super::*;
    use alloc::string::{String, ToString};
    use core::fmt::Write;

    #[test]
    fn display_span_first_line() {
        let msg = Span::new("123\n456\n789\n", 1, 2).unwrap().to_string();
        assert_eq!(
            msg,
            "  \
  |
1 | 123␊
  |  ^
"
        );
    }

    #[test]
    fn display_span_mid_line() {
        let msg = Span::new("123\n456\n789\n", 6, 7).unwrap().to_string();
        assert_eq!(
            msg,
            "  \
  |
2 | 456␊
  |   ^
"
        );
    }

    #[test]
    fn display_span_last_line() {
        let msg = Span::new("123\n456\n789\n", 9, 10).unwrap().to_string();
        assert_eq!(
            msg,
            "  \
  |
3 | 789␊
  |  ^
"
        );
    }

    #[test]
    fn display_span_same_pos() {
        let msg = Span::new("123\n456\n789\n", 9, 9).unwrap().to_string();
        assert_eq!(
            msg,
            "  \
  |
3 | 789␊
  |  
"
        );
    }

    #[test]
    fn display_span_cr_lf_single_line() {
        let msg = Span::new("123\r\n", 4, 5).unwrap().to_string();
        assert_eq!(
            msg,
            "  \
  |
1 | 123␍␊
  |     ^
"
        );
    }

    #[test]
    fn display_span_cr_lf_three_line() {
        let msg = Span::new("123\r\n456\r\n789\r\n", 4, 14)
            .unwrap()
            .to_string();
        assert_eq!(
            msg,
            "  \
  |     v
1 | 123␍␊
2 | 456␍␊
3 | 789␍␊
  |    ^
"
        );
    }

    #[test]
    fn display_span_two_lines() {
        let msg = Span::new("123\n456\n", 2, 5).unwrap().to_string();
        assert_eq!(
            msg,
            "  \
  |   v
1 | 123␊
2 | 456␊
  | ^
"
        );
    }

    #[test]
    fn display_span_three_lines() {
        let msg = Span::new("123\n456\n789\n", 2, 11).unwrap().to_string();
        assert_eq!(
            msg,
            "  \
  |   v
1 | 123␊
2 | 456␊
3 | 789␊
  |   ^
"
        );
    }

    #[test]
    fn display_span_four_lines() {
        let msg = Span::new("123\n456\n789\nabc\n", 2, 14)
            .unwrap()
            .to_string();
        assert_eq!(
            msg,
            "  \
  |   v
1 | 123␊
2 | 456␊
3 | 789␊
4 | abc␊
  |  ^
"
        );
    }

    #[test]
    fn display_span_five_lines() {
        let msg = Span::new("123\n456\n789\nabc\ndef\n", 2, 20)
            .unwrap()
            .to_string();
        assert_eq!(
            msg,
            "  \
  |   v
1 | 123␊
2 | 456␊
3 | 789␊
4 | abc␊
5 | def␊
  |    ^
"
        );
    }

    #[test]
    fn display_span_six_lines() {
        let msg = Span::new("123\n456\n789\nabc\ndef\nghi\n", 2, 23)
            .unwrap()
            .to_string();
        assert_eq!(
            msg,
            "  \
  |   v
1 | 123␊
2 | 456␊
  | ...
5 | def␊
6 | ghi␊
  |   ^
"
        );
    }

    #[test]
    fn display_span_unicode() {
        let msg = Span::new("ß\n∆\n中\n", 2, 10).unwrap().to_string();
        assert_eq!(
            msg,
            "  \
  |   v
1 | ß␊
2 | ∆␊
3 | 中␊
  |  ^
"
        );
    }

    #[test]
    fn display_span_quoted() {
        let mut msg = String::new();
        Span::new("123\n456\n789\n", 2, 11)
            .unwrap()
            .display(
                &mut msg,
                FormatOption::new::<String>(
                    |s, f| write!(f, "{{{s}}}"),
                    |s, f| write!(f, "[{s}]"),
                    |s, f| write!(f, "<{s}>"),
                ),
            )
            .unwrap();
        assert_eq!(
            msg,
            "  \
  <|>   [v]
<1> <|> 12{3␊}
<2> <|> {456␊}
<3> <|> {789}␊
  <|>   [^]
"
        );
    }
}
