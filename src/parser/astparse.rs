use super::{Parse, ParseResult, TokenStream};
use crate::{
    ast::{Computation, Expr, Factor, FactorOp, OpChain, Relation, Term, TermOp},
    scanner::TokenResult,
};
use std::fmt::Debug;

impl Parse for Computation {
    fn parse(stream: &mut TokenStream<impl Iterator<Item = TokenResult>>) -> ParseResult<Self> {
        todo!()
    }
}

impl<Operand, Operation> Parse for OpChain<Operand, Operation>
where
    Operand: Clone + Debug + Parse + PartialEq,
    Operation: Clone + Debug + Parse + PartialEq,
{
    fn parse(stream: &mut TokenStream<impl Iterator<Item = TokenResult>>) -> ParseResult<Self> {
        let root = Operand::parse(stream)?;
        // TODO: parse ops

        Ok(OpChain { root, ops: vec![] })
    }
}

impl Parse for Factor {
    fn parse(stream: &mut TokenStream<impl Iterator<Item = TokenResult>>) -> ParseResult<Self> {
        todo!()
    }
}

impl Parse for FactorOp {
    fn parse(stream: &mut TokenStream<impl Iterator<Item = TokenResult>>) -> ParseResult<Self> {
        todo!()
    }
}

impl Parse for Relation {
    fn parse(stream: &mut TokenStream<impl Iterator<Item = TokenResult>>) -> ParseResult<Self> {
        let lhs = Expr::parse(stream)?;
        let op = stream.expect_relop()?;
        let rhs = Expr::parse(stream)?;

        Ok(Relation { lhs, rhs, op })
    }
}

impl Parse for TermOp {
    fn parse(stream: &mut TokenStream<impl Iterator<Item = TokenResult>>) -> ParseResult<Self> {
        todo!()
    }
}
