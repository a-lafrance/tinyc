use std::fmt::{self, Display, Formatter};
use crate::utils::{Keyword, RelOp};

#[derive(Debug, PartialEq)]
pub enum Token {
    Number(i32),
    Ident(String),
    Keyword(Keyword),
    RelOp(RelOp),
    Punctuation(char),
    AssignOp,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Token::Number(n) => write!(f, "{}", n),
            Token::Ident(ident) => write!(f, "{}", ident),
            Token::Keyword(kw) => write!(f, "{}", kw),
            Token::RelOp(op) => write!(f, "{}", op),
            Token::Punctuation(c) => write!(f, "{}", c),
            Token::AssignOp => write!(f, "<-"),
        }
    }
}
