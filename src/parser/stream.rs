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

// TokenStream has just been transplanted from the root of the parser module
// It really needs to be rewritten from the ground up
// This means reducing its functionality to a set of core methods with actual good names,
// and also the most elegant internal and external apis possible

// you want to:
    // expect what type the next token is; consume if it exists, error otherwise
    // expect exactly what the next token is; if it doesn't match, error
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

    pub fn try_consume_assign_op(&mut self) -> ParseResult<()> {
        match self.current {
            Some(Ok(Token::AssignOp)) => {
                self.advance();
                Ok(())
            },
            _ => Err(ParseError::ExpectedAssignOp),
        }
    }

    pub fn try_consume_matching_keyword(&mut self, target: Keyword) -> ParseResult<()> {
        match self.current {
            Some(Ok(Token::Keyword(kw))) if kw == target => {
                self.advance();
                Ok(())
            },
            _ => Err(ParseError::ExpectedKeyword(target)),
        }
    }

    pub fn try_consume_relop(&mut self) -> ParseResult<RelOp> {
        match self.current {
            Some(Ok(Token::RelOp(op))) => {
                self.advance();
                Ok(op)
            }

            _ => Err(ParseError::ExpectedRelOp),
        }
    }

    pub fn try_consume_matching_punctuation(&mut self, target: char) -> ParseResult<()> {
        match self.current {
            Some(Ok(Token::Punctuation(c))) if c == target => {
                self.advance();
                Ok(())
            }
            _ => Err(ParseError::ExpectedPunctuation(target)),
        }
    }

    pub fn try_consume_number(&mut self) -> Option<u32> {
        match self.current {
            Some(Ok(Token::Number(n))) => {
                self.advance();
                Some(n)
            }
            _ => None,
        }
    }

    pub fn try_consume_ident(&mut self) -> ParseResult<String> {
        match self.current {
            Some(Ok(Token::Ident(_))) => match self.advance() {
                Some(Ok(Token::Ident(ident))) => Ok(ident),
                _ => unreachable!(),
            },
            _ => Err(ParseError::ExpectedIdentifier),
        }
    }

    pub fn try_consume_termop(&mut self) -> Option<TermOp> {
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

    pub fn try_consume_factorop(&mut self) -> Option<FactorOp> {
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

    pub fn try_peek_keyword(&self) -> Option<Keyword> {
        match self.peek() {
            Some(Ok(Token::Keyword(kw))) => Some(*kw),
            _ => None,
        }
    }
}
