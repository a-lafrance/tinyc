use crate::{parser::Parse, scanner::tok::RelOp};
use std::fmt::Debug;

#[derive(Clone, Debug, PartialEq)]
pub struct Computation {
    pub vars: Vec<VarDecl>,
    pub funcs: Vec<FuncDecl>,
    pub body: Block,
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

#[derive(Clone, Debug, PartialEq)]
pub struct OpChain<Operand, Operation>
where
    Operand: Clone + Debug + Parse + PartialEq,
    Operation: Clone + Debug + Parse + PartialEq,
{
    pub root: Operand,
    pub ops: Vec<(Operation, Operand)>,
}

pub type Expr = OpChain<Term, TermOp>;
pub type Term = OpChain<Factor, FactorOp>;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TermOp {
    Add,
    Sub,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Factor; // TODO

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum FactorOp {
    Mul,
    Div,
}
