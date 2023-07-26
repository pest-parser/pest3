use std::ops::Range;

#[derive(Debug)]
pub enum Expr {
    Accept,
    Reject,
    Any,
    None,
    Str(String),
    Insens(String),
    Range(Range<char>),
    Ident(String),
    Seq(Box<Expr>, Box<Expr>),
    Choice(Box<Expr>, Box<Expr>),
    Rep(Box<Expr>),
    Pred(Box<Expr>, bool),
}
