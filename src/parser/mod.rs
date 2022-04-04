use crate::{
    ast::Computation,
    scanner::TokenResult,
};

type ParseResult<T> = Result<T, ()>;

pub trait Parse: Sized {
    fn parse(tokens: impl Iterator<Item = TokenResult>) -> ParseResult<Self>;
}

pub fn parse(tokens: impl Iterator<Item = TokenResult>) -> ParseResult<Computation> {
    Computation::parse(tokens)
}

impl Parse for Computation {
    fn parse(tokens: impl Iterator<Item = TokenResult>) -> ParseResult<Self> {
        todo!()
    }
}
