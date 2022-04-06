use crate::{
    ast::{FactorOp, TermOp},
    scanner::TokenResult,
    tok::Token,
    utils::{Keyword, RelOp},
};
use super::{
    err::ParseError,
    ParseResult
};

pub struct TokenStream<T: Iterator<Item = TokenResult>> {
    current: Option<TokenResult>,
    tokens: T,
}

impl<T: Iterator<Item = TokenResult>> TokenStream<T> {
    pub fn new(mut tokens: T) -> TokenStream<T> {
        TokenStream {
            current: tokens.next(),
            tokens,
        }
    }

    pub fn peek(&self) -> Option<&TokenResult> {
        self.current.as_ref()
    }

    fn advance(&mut self) -> Option<TokenResult> {
        let prev = self.current.take();
        self.current = self.tokens.next();

        prev
    }

    pub fn expect_assign_op(&mut self) -> bool {
        match self.current {
            Some(Ok(Token::AssignOp)) => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    pub fn expect_keyword(&mut self, keyword: Keyword) -> bool {
        match self.peek_keyword_if_exists() {
            Some(kw) if kw == keyword => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    pub fn expect_keyword_or_err(&mut self, keyword: Keyword) -> ParseResult<()> {
        if self.expect_keyword(keyword) {
            Ok(())
        } else {
            Err(ParseError::ExpectedKeyword(keyword))
        }
    }

    pub fn expect_relop(&mut self) -> ParseResult<RelOp> {
        match self.current {
            Some(Ok(Token::RelOp(op))) => {
                self.advance();
                Ok(op)
            }

            _ => Err(ParseError::ExpectedRelOp),
        }
    }

    pub fn expect_punctuation_matching(&mut self, c: char) -> bool {
        match self.current {
            Some(Ok(Token::Punctuation(ch))) if ch == c => {
                self.advance();
                true
            }

            _ => false,
        }
    }

    pub fn expect_punctuation_or_err(&mut self, c: char) -> ParseResult<()> {
        if self.expect_punctuation_matching(c) {
            Ok(())
        } else {
            Err(ParseError::ExpectedPunctuation(c))
        }
    }

    pub fn consume_number_if_exists(&mut self) -> Option<u32> {
        match self.current {
            Some(Ok(Token::Number(n))) => {
                self.advance();
                Some(n)
            }
            _ => None,
        }
    }

    pub fn consume_ident_if_exists(&mut self) -> ParseResult<String> {
        match self.current {
            Some(Ok(Token::Ident(_))) => match self.advance() {
                Some(Ok(Token::Ident(ident))) => Ok(ident),
                _ => unreachable!(),
            },
            _ => Err(ParseError::ExpectedIdentifier),
        }
    }

    pub fn consume_termop_if_exists(&mut self) -> Option<TermOp> {
        match self.current {
            Some(Ok(Token::Punctuation('+'))) => {
                self.advance();
                Some(TermOp::Add)
            }

            Some(Ok(Token::Punctuation('-'))) => {
                self.advance();
                Some(TermOp::Sub)
            }

            _ => None,
        }
    }

    pub fn consume_factorop_if_exists(&mut self) -> Option<FactorOp> {
        match self.current {
            Some(Ok(Token::Punctuation('*'))) => {
                self.advance();
                Some(FactorOp::Mul)
            }

            Some(Ok(Token::Punctuation('/'))) => {
                self.advance();
                Some(FactorOp::Div)
            }

            _ => None,
        }
    }

    pub fn peek_keyword_if_exists(&self) -> Option<Keyword> {
        match self.peek() {
            Some(Ok(Token::Keyword(kw))) => Some(*kw),
            _ => None,
        }
    }
}
