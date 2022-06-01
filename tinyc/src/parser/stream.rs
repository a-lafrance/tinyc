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
    current: Option<Token>,
    tokens: T,
}

impl<T: Iterator<Item = TokenResult>> TokenStream<T> {
    pub fn new(mut tokens: T) -> ParseResult<TokenStream<T>> {
        Ok(TokenStream {
            current: tokens.next().transpose()?,
            tokens,
        })
    }

    pub fn peek(&self) -> Option<&Token> {
        self.current.as_ref()
    }

    fn advance(&mut self) -> ParseResult<Option<Token>> {
        let prev = self.current.take();
        self.current = self.tokens.next().transpose()?;

        Ok(prev)
    }

    pub fn try_consume_assign_op(&mut self) -> ParseResult<()> {
        match self.current {
            Some(Token::AssignOp) => {
                self.advance()?;
                Ok(())
            },
            _ => Err(ParseError::ExpectedAssignOp),
        }
    }

    pub fn try_consume_matching_keyword(&mut self, target: Keyword) -> ParseResult<()> {
        match self.current {
            Some(Token::Keyword(kw)) if kw == target => {
                self.advance()?;
                Ok(())
            },
            _ => Err(ParseError::ExpectedKeyword(target)),
        }
    }

    pub fn try_consume_relop(&mut self) -> ParseResult<RelOp> {
        match self.current {
            Some(Token::RelOp(op)) => {
                self.advance()?;
                Ok(op)
            }

            _ => Err(ParseError::ExpectedRelOp),
        }
    }

    pub fn try_consume_matching_punctuation(&mut self, target: char) -> ParseResult<()> {
        match self.current {
            Some(Token::Punctuation(c)) if c == target => {
                self.advance()?;
                Ok(())
            }
            _ => Err(ParseError::ExpectedPunctuation(target)),
        }
    }

    pub fn try_consume_number(&mut self) -> ParseResult<Option<i32>> {
        match self.current {
            Some(Token::Number(n)) => {
                self.advance()?;
                Ok(Some(n))
            }
            _ => Ok(None),
        }
    }

    pub fn try_consume_ident(&mut self) -> ParseResult<String> {
        // it's fine to advance here because if it's not an identifier, an error is thrown so
        // the token won't be used anyway
        match self.advance()? {
            Some(Token::Ident(ident)) => Ok(ident),
            _ => Err(ParseError::ExpectedIdentifier),
        }
    }

    pub fn try_consume_termop(&mut self) -> ParseResult<Option<TermOp>> {
        match self.current {
            Some(Token::Punctuation('+')) => {
                self.advance()?;
                Ok(Some(TermOp::Add))
            }

            Some(Token::Punctuation('-')) => {
                self.advance()?;
                Ok(Some(TermOp::Sub))
            }

            _ => Ok(None),
        }
    }

    pub fn try_consume_factorop(&mut self) -> ParseResult<Option<FactorOp>> {
        match self.current {
            Some(Token::Punctuation('*')) => {
                self.advance()?;
                Ok(Some(FactorOp::Mul))
            }

            Some(Token::Punctuation('/')) => {
                self.advance()?;
                Ok(Some(FactorOp::Div))
            }

            _ => Ok(None),
        }
    }

    pub fn try_peek_keyword(&self) -> Option<Keyword> {
        match self.peek() {
            Some(Token::Keyword(kw)) => Some(*kw),
            _ => None,
        }
    }
}
