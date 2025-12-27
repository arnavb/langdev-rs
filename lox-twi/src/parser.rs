use crate::token::{Token, TokenType};

struct Parser<'source> {
    tokens: Vec<Token<'source>>,
    current: usize,
}

impl<'source> Parser<'source> {
    fn new(tokens: Vec<Token<'source>>) -> Self {
        Parser { tokens, current: 0 }
    }

    fn match_tokens(&mut self, token_types: &[TokenType]) -> bool {
        for token_type in token_types {
            if self.check(*token_type) {
                self.advance();
                return true;
            }
        }

        return false;
    }

    fn check(&self, token_type: TokenType) -> bool {
        if self.is_at_end() {
            false
        } else {
            self.peek().token_type == token_type
        }
    }

    fn is_at_end(&self) -> bool {
        self.current == self.tokens.len() - 1
    }

    fn peek(&self) -> &Token<'source> {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token<'source> {
        &self.tokens[self.current - 1]
    }

    fn advance(&mut self) -> &Token<'source> {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }
}
