use crate::tok::RelOp;

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
pub struct Expr {
    pub root: Term,
    pub ops: Vec<(TermOp, Term)>,
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

#[derive(Clone, Debug, PartialEq)]
pub struct FuncCall;
