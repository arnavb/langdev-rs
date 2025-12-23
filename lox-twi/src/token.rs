use std::{
    fmt::{self, Display},
    str::from_utf8,
};

#[derive(Debug, Clone, Copy)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Identifier,
    String,
    Number,
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Eof,
    SingleLineComment,
    MultiLineComment,
}

#[derive(PartialEq)]
pub enum Literal<'source> {
    Number(f64),
    String(&'source [u8]),
}

impl fmt::Debug for Literal<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Number(num) => write!(f, "{}", num),
            Self::String(string) => write!(
                f,
                "\"{}\"",
                from_utf8(string).expect("Invalid UTF-8 when formatting literal")
            ),
        }
    }
}

#[derive(PartialEq)]
pub struct Lexeme<'source>(pub &'source [u8]);

// Helper so lexemes are visible as strings
impl fmt::Debug for Lexeme<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            from_utf8(self.0).expect("Invalid UTF-8 when formatting lexeme")
        )
    }
}

#[derive(Debug)]
pub struct Token<'source> {
    pub token_type: TokenType,
    pub lexeme: Lexeme<'source>,
    pub literal: Option<Literal<'source>>,
    pub line: usize,
}
