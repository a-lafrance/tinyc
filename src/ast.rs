use std::convert::TryFrom;
use crate::tok::RelOp;

/* DECLARATIONS */

#[derive(Clone, Debug, PartialEq)]
pub struct Computation {
    pub vars: Vec<VarDecl>,
    pub funcs: Vec<FuncDecl>,
    pub body: Block,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FuncDecl;

#[derive(Clone, Debug, PartialEq)]
pub struct VarDecl;

/* EXPRESSIONS */

#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub root: Term,
    pub ops: Vec<(TermOp, Term)>,
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

impl TryFrom<char> for FactorOp {
    type Error = char;

    fn try_from(c: char) -> Result<FactorOp, char> {
        match c {
            '*' => Ok(FactorOp::Mul),
            '/' => Ok(FactorOp::Div),
            other => Err(other),
        }
    }
}

impl From<FactorOp> for char {
    fn from(op: FactorOp) -> char {
        match op {
            FactorOp::Mul => '*',
            FactorOp::Div => '/',
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Relation {
    pub lhs: Expr,
    pub rhs: Expr,
    pub op: RelOp,
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

impl TryFrom<char> for TermOp {
    type Error = char;

    fn try_from(c: char) -> Result<TermOp, char> {
        match c {
            '+' => Ok(TermOp::Add),
            '-' => Ok(TermOp::Sub),
            other => Err(other),
        }
    }
}

impl From<TermOp> for char {
    fn from(op: TermOp) -> char {
        match op {
            TermOp::Add => '+',
            TermOp::Sub => '-',
        }
    }
}

/* STATEMENTS */

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment {
    pub place: String,
    pub value: Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block;

#[derive(Clone, Debug, PartialEq)]
pub struct FuncCall {
    pub name: String,
    pub args: Vec<Expr>,
}
