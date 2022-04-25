use std::{
    error::Error,
    fmt::{self, Display, Formatter},
};
use crate::{
    scanner::InvalidCharError,
    sym::UndefinedSymbolError,
    utils::{Keyword, RelOp},
};
use super::FuncCallContext;

#[derive(Debug, PartialEq)]
pub enum ParseError {
    InvalidChar(InvalidCharError),
    ExpectedKeyword(Keyword),
    ExpectedIdentifier,
    ExpectedStatement,
    ExpectedPunctuation(char),
    ExpectedAssignOp,
    ExpectedRelOp,
    UndefinedSymbol(UndefinedSymbolError),
    InvalidFuncCall(String, FuncCallContext),
    DuplicateFuncDecl(String),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            ParseError::InvalidChar(e) => write!(f, "{}", e),
            ParseError::ExpectedKeyword(kw) => write!(f, "expected keyword '{}'", kw),
            ParseError::ExpectedIdentifier => write!(f, "expected identifier"),
            ParseError::ExpectedStatement => write!(f, "expected statement"),
            ParseError::ExpectedPunctuation(c) => write!(f, "expected '{}'", c),
            ParseError::ExpectedAssignOp => write!(f, "expected '<-'"),
            ParseError::ExpectedRelOp => write!(f, "expected relational operator: one of {}", RelOp::all_as_str()),
            ParseError::UndefinedSymbol(e) => write!(f, "{}", e),
            ParseError::InvalidFuncCall(func, context) => {
                let exp_return = match context {
                    FuncCallContext::Expr => "non-void",
                    FuncCallContext::Stmt => "void",
                };

                write!(f, "invalid function call, expected {} return for function '{}'", exp_return, func)
            },
            ParseError::DuplicateFuncDecl(func) => write!(f, "invalid redeclaration of function '{}'", func),
        }
    }
}

impl Error for ParseError { }

impl From<InvalidCharError> for ParseError {
    fn from(e: InvalidCharError) -> Self {
        ParseError::InvalidChar(e)
    }
}

impl From<UndefinedSymbolError> for ParseError {
    fn from(e: UndefinedSymbolError) -> Self {
        ParseError::UndefinedSymbol(e)
    }
}
