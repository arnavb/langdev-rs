use std::{collections::HashMap, str::from_utf8};

use crate::token::{Lexeme, Literal, Token, TokenType};

pub enum ScanError {
    UnexpectedCharacter(u8),
    UnterminatedString,
    UnterminatedMultiLineComment,
}

pub struct Scanner<'source> {
    source: &'source [u8],
    pub tokens: Vec<Token<'source>>,
    start: usize,
    current: usize,
    line: usize,
    reserved_keywords: HashMap<&'static str, TokenType>,
}

impl<'source> Scanner<'source> {
    pub fn new(source: &'source str) -> Self {
        use TokenType::*;

        Self {
            source: source.as_bytes(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,

            reserved_keywords: HashMap::from([
                ("and", And),
                ("class", Class),
                ("else", Else),
                ("false", False),
                ("for", For),
                ("fun", Fun),
                ("if", If),
                ("nil", Nil),
                ("or", Or),
                ("print", Print),
                ("return", Return),
                ("super", Super),
                ("this", This),
                ("true", True),
                ("var", Var),
                ("while", While),
            ]),
        }
    }

    pub fn scan_tokens(mut self) -> (Vec<Token<'source>>, Vec<ScanError>) {
        let mut scan_errors = Vec::new();

        while !self.is_at_end() {
            self.start = self.current;

            if let Err(e) = self.scan_next_token() {
                scan_errors.push(e);
            }
        }

        self.tokens.push(Token {
            token_type: TokenType::Eof,
            lexeme: Lexeme(b""),
            literal: None,
            line: self.line,
        });

        (self.tokens, scan_errors)
    }

    fn scan_next_token(&mut self) -> Result<(), ScanError> {
        use TokenType::*;

        let next_character = self.advance();

        match next_character {
            b'(' => Ok(self.push_single_character_token_object(LeftParen)),
            b')' => Ok(self.push_single_character_token_object(RightParen)),
            b'{' => Ok(self.push_single_character_token_object(LeftBrace)),
            b'}' => Ok(self.push_single_character_token_object(RightBrace)),
            b',' => Ok(self.push_single_character_token_object(Comma)),
            b'.' => Ok(self.push_single_character_token_object(Dot)),
            b'-' => Ok(self.push_single_character_token_object(Minus)),
            b'+' => Ok(self.push_single_character_token_object(Plus)),
            b';' => Ok(self.push_single_character_token_object(Semicolon)),
            b'*' => Ok(self.push_single_character_token_object(Star)),

            // Two character lexeme
            b'!' => Ok(self.push_two_character_token_object(b'=', BangEqual, Bang)),
            b'=' => Ok(self.push_two_character_token_object(b'=', EqualEqual, Equal)),
            b'>' => Ok(self.push_two_character_token_object(b'=', LessEqual, Less)),
            b'<' => Ok(self.push_two_character_token_object(b'=', GreaterEqual, Greater)),

            // Comments or division
            b'/' => self.comment_or_slash(),

            // Ignored whitespace
            b' ' | b'\r' | b'\t' => Ok(()),

            b'\n' => {
                self.line += 1;
                Ok(())
            }

            // Strings
            b'"' => self.string(),

            // Numbers
            b'0'..=b'9' => self.number(),

            // Keywords or identifiers
            b'_' | b'a'..=b'z' | b'A'..=b'Z' => self.keyword_or_identifier(),

            e => Err(ScanError::UnexpectedCharacter(e)),
        }
    }

    fn push_single_character_token_object(&mut self, next_token_type: TokenType) {
        let next_token = self.create_token_object(next_token_type, None);
        self.tokens.push(next_token)
    }

    fn push_two_character_token_object(
        &mut self,
        expected: u8,
        double: TokenType,
        single: TokenType,
    ) {
        let next_token_type = if self.match_next(expected) {
            double
        } else {
            single
        };
        let next_token = self.create_token_object(next_token_type, None);
        self.tokens.push(next_token)
    }

    fn peek(&self) -> Option<u8> {
        if self.is_at_end() {
            None
        } else {
            Some(self.source[self.current])
        }
    }

    fn peek_next(&self) -> Option<u8> {
        if self.current + 1 >= self.source.len() {
            None
        } else {
            Some(self.source[self.current + 1])
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> u8 {
        let result = *self
            .source
            .get(self.current)
            .expect("out of range scanner index (this should never happen)");

        self.current += 1;

        result
    }

    fn match_next(&mut self, expected: u8) -> bool {
        if self.is_at_end() {
            return false;
        }

        if self.source[self.current] != expected {
            return false;
        }

        self.current += 1;

        true
    }

    fn create_token_object(
        &self,
        token_type: TokenType,
        literal: Option<Literal<'source>>,
    ) -> Token<'source> {
        let text = &self.source[self.start..self.current];

        Token {
            token_type,
            lexeme: Lexeme(text),
            literal,
            line: self.line,
        }
    }

    fn comment_or_slash(&mut self) -> Result<(), ScanError> {
        if self.match_next(b'/') {
            while let Some(ch) = self.peek() {
                if self.is_at_end() || ch == b'\n' {
                    break;
                }
                self.advance();
            }

            // Consume leading slashes
            let value = &self.source[self.start + 2..self.current];

            let next_token = self
                .create_token_object(TokenType::SingleLineComment, Some(Literal::String(value)));

            Ok(self.tokens.push(next_token))
        } else if self.match_next(b'*') {
            // Found outermost multiline comment
            self.multiline_comment_context()
        } else {
            Ok(self.push_single_character_token_object(TokenType::Slash))
        }
    }

    fn string(&mut self) -> Result<(), ScanError> {
        while self.peek() != Some(b'"') && !self.is_at_end() {
            if self.peek() == Some(b'\n') {
                self.line += 1;
            }

            self.advance();
        }

        if self.is_at_end() {
            Err(ScanError::UnterminatedString)
        } else {
            // Closing quote
            self.advance();

            let value = &self.source[self.start + 1..self.current - 1];

            let next_token =
                self.create_token_object(TokenType::String, Some(Literal::String(value)));

            Ok(self.tokens.push(next_token))
        }
    }

    fn number(&mut self) -> Result<(), ScanError> {
        self.consume_digits();

        if self.peek() == Some(b'.') {
            if let Some(ch) = self.peek_next() {
                if ch >= b'0' && ch <= b'9' {
                    self.consume_digits();
                }
            }
        }

        let value = &self.source[self.start..self.current];

        // Ugly but we're fairly certain these bytes represent a valid number
        let parsed_f64 = from_utf8(value).unwrap().parse::<f64>().unwrap();

        let next_token =
            self.create_token_object(TokenType::Number, Some(Literal::Number(parsed_f64)));
        Ok(self.tokens.push(next_token))
    }

    fn consume_digits(&mut self) {
        while let Some(ch) = self.peek() {
            if !(ch >= b'0' && ch <= b'9') {
                break;
            }

            self.advance();
        }
    }

    fn keyword_or_identifier(&mut self) -> Result<(), ScanError> {
        while let Some(ch) = self.peek() {
            if !((ch >= b'0' && ch <= b'9') || ch.is_ascii_alphabetic()) {
                break;
            }

            self.advance();
        }

        let text = from_utf8(&self.source[self.start..self.current]).unwrap();
        let token_type = *self
            .reserved_keywords
            .get(text)
            .unwrap_or(&TokenType::Identifier);

        let next_token = self.create_token_object(token_type, None);
        Ok(self.tokens.push(next_token))
    }

    fn multiline_comment_context(&mut self) -> Result<(), ScanError> {
        // We start after consuming the /* in self.comment_or_slash, so level is set to 1, not 0
        let mut current_level = 1;

        while let Some(ch) = self.peek() {
            if ch == b'/' && self.peek_next() == Some(b'*') {
                current_level += 1;
                self.advance();
                self.advance();
                continue;
            } else if ch == b'*' && self.peek_next() == Some(b'/') {
                current_level -= 1;
                self.advance();
                self.advance();
                continue;
            }

            self.advance();
        }

        if current_level > 0 {
            // One of the comments was unterminated
            Err(ScanError::UnterminatedMultiLineComment)
        } else {
            let text = &self.source[self.start + 2..self.current - 2];

            let next_token =
                self.create_token_object(TokenType::MultiLineComment, Some(Literal::String(text)));

            Ok(self.tokens.push(next_token))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_multiline_comment() {
        let source_string = "/* comment */";

        let scanner = Scanner::new(&source_string);

        let (tokens, _) = scanner.scan_tokens();

        let comment_token = &tokens[0];

        matches!(comment_token.token_type, TokenType::MultiLineComment);

        assert_eq!(comment_token.lexeme, Lexeme(source_string.as_bytes()));

        assert_eq!(
            comment_token.literal.as_ref().unwrap(),
            &Literal::String(b" comment ")
        );
    }

    #[test]
    fn nested_valid_mutliline_comments() {
        let source_string = "/* comment /* another */ /* deeper */ */";

        let scanner = Scanner::new(&source_string);

        let (tokens, errors) = scanner.scan_tokens();

        assert!(errors.is_empty());

        let comment_token = &tokens[0];

        matches!(comment_token.token_type, TokenType::MultiLineComment);

        assert_eq!(comment_token.lexeme, Lexeme(source_string.as_bytes()));

        assert_eq!(
            comment_token.literal.as_ref().unwrap(),
            &Literal::String(b" comment /* another */ /* deeper */ ")
        );
    }

    #[test]
    fn unterminated_simple_multiline_comment() {
        let source_string = "/* comment ";

        let scanner = Scanner::new(&source_string);

        let (_, errors) = scanner.scan_tokens();

        assert!(!errors.is_empty());

        matches!(errors[0], ScanError::UnterminatedMultiLineComment);
    }

    #[test]
    fn unterminated_nested_multiline_comment() {
        let source_string = "/* comment /* /* /* */";

        let scanner = Scanner::new(&source_string);

        let (_, errors) = scanner.scan_tokens();

        assert!(!errors.is_empty());

        matches!(errors[0], ScanError::UnterminatedMultiLineComment);
    }

    #[test]
    fn advanced_multiline_nested_comment() {
        let source_string = "/* /* */ /* /* */ */ */";

        let scanner = Scanner::new(&source_string);

        let (tokens, errors) = scanner.scan_tokens();

        assert!(errors.is_empty());

        let comment_token = &tokens[0];

        matches!(comment_token.token_type, TokenType::MultiLineComment);

        assert_eq!(comment_token.lexeme, Lexeme(source_string.as_bytes()));

        assert_eq!(
            comment_token.literal.as_ref().unwrap(),
            &Literal::String(b" /* */ /* /* */ */ ")
        );
    }
}
