mod astparse;

use crate::{
    ast::{Computation, FactorOp, Relation, TermOp},
    scanner::{
        tok::{RelOp, Token},
        TokenResult,
    },
};

pub type ParseResult<T> = Result<T, ()>;

pub trait Parse: Sized {
    fn parse(stream: &mut TokenStream<impl Iterator<Item = TokenResult>>) -> ParseResult<Self>;
}

pub fn parse(tokens: impl Iterator<Item = TokenResult>) -> ParseResult<Computation> {
    let mut stream = TokenStream::new(tokens);
    Computation::parse(&mut stream)
}

pub struct TokenStream<T: Iterator<Item = TokenResult>> {
    current: Option<TokenResult>,
    stream: T,
}

impl<T: Iterator<Item = TokenResult>> TokenStream<T> {
    pub fn new(mut stream: T) -> Self {
        TokenStream {
            current: stream.next(),
            stream,
        }
    }

    fn advance(&mut self) -> Option<TokenResult> {
        let prev = self.current.take();
        self.current = self.stream.next();

        prev
    }

    pub fn expect_relop(&mut self) -> ParseResult<RelOp> {
        match self.current {
            Some(Ok(Token::RelOp(op))) => {
                self.advance();
                Ok(op)
            }

            _ => Err(()),
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

    pub fn consume_number_if_exists(&mut self) -> Option<u32> {
        match self.current {
            Some(Ok(Token::Number(n))) => {
                self.advance();
                Some(n)
            },
            _ => None,
        }
    }

    pub fn consume_ident_if_exists(&mut self) -> Option<String> {
        match self.current {
            Some(Ok(Token::Ident(_))) => match self.advance() {
                Some(Ok(Token::Ident(ident))) => Some(ident),
                _ => unreachable!(),
            },
            _ => None,
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
}
