use crate::token::{Literal, Token};

#[derive(Debug)]
enum Expr<'source> {
    Binary(Box<Expr<'source>>, Token<'source>, Box<Expr<'source>>),
    Grouping(Box<Expr<'source>>),
    Literal(Literal<'source>),
    Unary,
}
