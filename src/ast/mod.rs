use crate::{
    parser::{Parse, ParseResult, TokenStream},
    scanner::{tok::RelOp, TokenResult},
};

#[derive(Clone, Debug, PartialEq)]
pub struct Computation {
    pub vars: Vec<VarDecl>,
    pub funcs: Vec<FuncDecl>,
    pub body: Block,
}

impl Parse for Computation {
    fn parse(stream: &mut TokenStream<impl Iterator<Item = TokenResult>>) -> ParseResult<Self> {
        todo!()
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct VarDecl;

#[derive(Clone, Debug, PartialEq)]
pub struct FuncDecl;

#[derive(Clone, Debug, PartialEq)]
pub struct Block;


#[derive(Clone, Debug, PartialEq)]
pub struct Relation {
    pub lhs: Expr,
    pub rhs: Expr,
    pub op: RelOp,
}

impl Parse for Relation {
    fn parse(stream: &mut TokenStream<impl Iterator<Item = TokenResult>>) -> ParseResult<Self> {
        let lhs = Expr::parse(stream)?;
        let op = stream.expect_relop()?;
        let rhs = Expr::parse(stream)?;

        Ok(Relation { lhs, rhs, op })
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub root: Term,
    pub ops: Vec<(TermOp, Term)>,
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


#[derive(Clone, Debug, PartialEq)]
pub struct Term {
    pub root: Factor,
    pub ops: Vec<(FactorOp, Factor)>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TermOp {
    Add,
    Sub,
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


#[derive(Clone, Debug, PartialEq)]
pub enum Factor {
    VarRef(String),
    Number(u32),
    SubExpr(Box<Expr>),
    Call(FuncCall),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum FactorOp {
    Mul,
    Div,
}

impl Parse for Factor {
    fn parse(stream: &mut TokenStream<impl Iterator<Item = TokenResult>>) -> ParseResult<Self> {
        if stream.expect_punctuation_matching('(') {
            let subexpr = Box::new(Expr::parse(stream)?);

            if stream.expect_punctuation_matching(')') {
                Ok(Factor::SubExpr(subexpr))
            } else {
                Err(())
            }
        } else if let Some(n) = stream.consume_number_if_exists() {
            Ok(Factor::Number(n))
        } else {
            match stream.consume_ident_if_exists() {
                Some(call_keyword) if call_keyword == "call" => Ok(Factor::Call(FuncCall::parse(stream)?)),
                Some(ident) => Ok(Factor::VarRef(ident)),
                None => Err(()),
            }
        }
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct FuncCall;

impl Parse for FuncCall {
    fn parse(stream: &mut TokenStream<impl Iterator<Item = TokenResult>>) -> ParseResult<Self> {
        todo!()
    }
}
