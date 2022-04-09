pub mod visit;

use std::convert::TryFrom;
use crate::utils::RelOp;

/* DECLARATIONS */

#[derive(Clone, Debug, PartialEq)]
pub struct Computation {
    pub vars: Vec<VarDecl>,
    pub funcs: Vec<FuncDecl>,
    pub body: Block,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FuncDecl {
    pub returns_void: bool,
    pub name: String,
    pub params: Vec<String>,
    pub vars: Vec<VarDecl>,
    pub body: Block,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarDecl {
    pub vars: Vec<String>,
}

impl VarDecl {
    #[cfg(test)]
    pub fn empty() -> VarDecl {
        VarDecl { vars: vec![] }
    }
}

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
pub struct Block {
    pub body: Vec<Stmt>,
}

impl Block {
    pub fn empty() -> Block {
        Block { body: vec![] }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FuncCall {
    pub name: String,
    pub args: Vec<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfStmt {
    pub condition: Relation,
    pub then_block: Block,
    pub else_block: Option<Block>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Loop {
    pub condition: Relation,
    pub body: Block,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Return {
    pub value: Option<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Assignment(Assignment),
    FuncCall(FuncCall),
    If(IfStmt),
    Loop(Loop),
    Return(Return),
}

impl From<Assignment> for Stmt {
    fn from(assign: Assignment) -> Stmt {
        Stmt::Assignment(assign)
    }
}

impl From<FuncCall> for Stmt {
    fn from(call: FuncCall) -> Stmt {
        Stmt::FuncCall(call)
    }
}

impl From<IfStmt> for Stmt {
    fn from(if_stmt: IfStmt) -> Stmt {
        Stmt::If(if_stmt)
    }
}

impl From<Loop> for Stmt {
    fn from(loop_stmt: Loop) -> Stmt {
        Stmt::Loop(loop_stmt)
    }
}

impl From<Return> for Stmt {
    fn from(ret: Return) -> Stmt {
        Stmt::Return(ret)
    }
}
