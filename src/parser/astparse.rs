use super::{Parse, ParseResult, TokenStream};
use crate::{
    ast::{Computation, Expr, Factor, FactorOp, Relation, Term, TermOp},
    scanner::TokenResult,
};
use std::fmt::Debug;

impl Parse for Computation {
    fn parse(stream: &mut TokenStream<impl Iterator<Item = TokenResult>>) -> ParseResult<Self> {
        todo!()
    }
}

impl Parse for Expr {
    fn parse(stream: &mut TokenStream<impl Iterator<Item = TokenResult>>) -> ParseResult<Self> {
        let root = Term::parse(stream)?;
        let mut ops = vec![];

        while let Some(op) = stream.consume_termop_if_exists() {
            let next = Term::parse(stream)?;
            ops.push((op, next))
        }

        Ok(Expr { root, ops })
    }
}

impl Parse for Factor {
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

impl Parse for Term {
    fn parse(stream: &mut TokenStream<impl Iterator<Item = TokenResult>>) -> ParseResult<Self> {
        let root = Factor::parse(stream)?;
        let mut ops = vec![];

        while let Some(op) = stream.consume_factorop_if_exists() {
            let next = Factor::parse(stream)?;
            ops.push((op, next))
        }

        Ok(Term { root, ops })
    }
}
