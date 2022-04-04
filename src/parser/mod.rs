mod astparse;

use crate::{
    ast::{Computation, Relation},
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

    fn advance(&mut self) {
        self.current = self.stream.next();
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
}
