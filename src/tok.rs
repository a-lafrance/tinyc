use std::fmt::{self, Display, Formatter};
use crate::utils::RelOp;

#[derive(Debug, PartialEq)]
pub enum Token {
    Number(u32),
    Ident(String),
    RelOp(RelOp),
    Punctuation(char),
    AssignOp,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Token::Number(n) => write!(f, "{}", n),
            Token::Ident(ident) => write!(f, "{}", ident),
            Token::RelOp(op) => write!(f, "{}", op),
            Token::Punctuation(c) => write!(f, "{}", c),
            Token::AssignOp => write!(f, "<-"),
        }
    }
}
